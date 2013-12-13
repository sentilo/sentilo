/*
 * Sentilo
 *   
 * Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de  Barcelona.
 *   
 * This program is licensed and may be used, modified and redistributed under the
 * terms  of the European Public License (EUPL), either version 1.1 or (at your 
 * option) any later version as soon as they are approved by the European 
 * Commission.
 *   
 * Alternatively, you may redistribute and/or modify this program under the terms
 * of the GNU Lesser General Public License as published by the Free Software 
 * Foundation; either  version 3 of the License, or (at your option) any later 
 * version. 
 *   
 * Unless required by applicable law or agreed to in writing, software distributed
 * under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR 
 * CONDITIONS OF ANY KIND, either express or implied. 
 *   
 * See the licenses for the specific language governing permissions, limitations 
 * and more details.
 *   
 * You should have received a copy of the EUPL1.1 and the LGPLv3 licenses along 
 * with this program; if not, you may find them at: 
 *   
 *   https://joinup.ec.europa.eu/software/page/eupl/licence-eupl
 *   http://www.gnu.org/licenses/ 
 *   and 
 *   https://www.gnu.org/licenses/lgpl.txt
 */
package org.sentilo.platform.service.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.sentilo.common.domain.SubscribeType;
import org.sentilo.platform.common.domain.Subscription;
import org.sentilo.platform.common.service.SubscribeService;
import org.sentilo.platform.service.listener.MessageListenerImpl;
import org.sentilo.platform.service.listener.MockMessageListenerImpl;
import org.sentilo.platform.service.utils.ChannelUtils;
import org.sentilo.platform.service.utils.PubSubConstants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.listener.RedisMessageListenerContainer;
import org.springframework.data.redis.listener.Topic;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;


@Service
public class SubscribeServiceImpl extends AbstractPlatformServiceImpl implements SubscribeService {

	private final Logger logger = LoggerFactory.getLogger(SubscribeServiceImpl.class);		
	
	@Autowired
	private RedisMessageListenerContainer listenerContainer;
	
	private Map<String, MessageListenerImpl> listeners = new HashMap<String, MessageListenerImpl>();
	
	private boolean subscriptionsRegistered = false;
	private static final String DUMMY_TOPIC = "trash:dummy"; 
	
	//@PostConstruct
    @Scheduled(initialDelay=30000,fixedDelay=300000)		
	public void loadSubscriptions(){
		//TODO Mikel: pensar en ejecutar el método de modo asíncrono @Async
		// ya que el comando KEYS de Redis se ve penalizado a medida que el número de claves crece
		//     http://redis.io/commands/keys
		
		// Al iniciar la plataforma, en caso de existir subscripciones registradas en Redis, inicializamos los MessageListeners correspondientes y los 
    	// registramos en el container:
		// 1. Recuperamos todas las claves del tipo REDIS_SUBS_PATTERN_KEY
		// 2. Para cada clave, recuperamos la información relativa al cjto de subscripciones asociadas
		// 3. Para cada subscripcion inicializamos el messagelistener correspondiente
    	//
    	// IMPORTANTE: en caso de no existir ninguna subscripción registrada, igualmente inicializamos un MessageListener que se ponga a escuchar por un canal dummy
    	// ya que sino el container acaba lanzando una excepción del tipo  ERR wrong number of arguments for 'psubscribe' command;
    	// la cual está asociada a que la conexión de subscripción no es válida
		boolean listenerContainerRunning = listenerContainer!=null && listenerContainer.isRunning();		
		logger.debug("Listener container isRunning? {}", listenerContainerRunning);
    	
		if(!subscriptionsRegistered && listenerContainerRunning){
    		logger.debug("Initialize subscriptions already registered in Redis");
			try {
				Set<String> subscriptions = jedisTemplate.keys(PubSubConstants.REDIS_SUBS_PATTERN_KEY);
				if(CollectionUtils.isEmpty(subscriptions)){
					logger.debug("Not found subscriptions in Redis");
					logger.debug("Registering a mock subscription to channel {}", DUMMY_TOPIC);
					doRegisterMockSubscription();
					return;
				}
				
				logger.debug("Found {} subscriptions in Redis", subscriptions.size());
				
				for(String subscriptionKey : subscriptions){
					// Cada subscriptionKey corresponde a una entidad subscrita a N canales de la plataforma
					// Toda la información de cada una de estas subscripciones está almacenada en una hash, donde cada
					// entrada corresponde a la info <canal,endpoint>
					Map<String,String> activeSubscriptions = jedisTemplate.hGetAll(subscriptionKey);
					if(CollectionUtils.isEmpty(activeSubscriptions)){
						return;
					}
				
					Set<String> channels = activeSubscriptions.keySet();
					String listenerName = listenerNameFromSubscriptionKey(subscriptionKey);
					
					for(String channel:channels){
						doRegisterSubscription(listenerName, ChannelUtils.buildTopic(channel), activeSubscriptions.get(channel));
					}
				}
				subscriptionsRegistered = true;
			} catch (Exception e) {				
				subscriptionsRegistered = false;
			}
    	}    	    	
	}
	
	private String listenerNameFromSubscriptionKey(String subscriptionKey){
		int pos = subscriptionKey.lastIndexOf(PubSubConstants.REDIS_KEY_TOKEN);
		return subscriptionKey.substring(pos+1);
	}
	
	@Override
	public void subscribe(Subscription subscription) {		
		// Al subscribirse, no sólo se debe habilitar el listener correspondiente, sino que tb se debe persistir en Redis
		// la subscripcion para la entidad de turno. De esta manera se podrá levantar los listeners ya existentes cuando se 
		// arranque este modulo.
		
		// Estos registros en Redis serán del tipo Hash y habrá uno para cada entidad. 
		// Es decir, para cada entidad que este subscrita a algun canal tendremos una Hash con:
		//    key igual a subs:<entityId>
		//    	field: cada campo de la hash corresponderá a una subscripcion , por lo que el nombre del campo será el nombre del channel al cual esta subscrito.subscripcion igual a <event_type>:element_id, donde element_id es el identificador del recurso al cual se esta subscrito.
		//    	value: el value del campo será el endpoint al cual se debe notificar via HTTP Callback.
		
		Topic topic = ChannelUtils.getChannel(subscription);		
		
		// Habilitamos listener
		doRegisterSubscription(subscription.getSourceEntityId(), topic, subscription.getEndpoint());
		
		// Persistimos en Redis la subscripcion
		jedisTemplate.hSet(keysBuilder.getSubscriptionKey(subscription.getSourceEntityId()), topic.getTopic(), subscription.getEndpoint());
		
		logger.debug("Listener {} subscribe to channel {}", subscription.getSourceEntityId(), topic.getTopic());
	}
	
	private void doRegisterSubscription(String listenerName, Topic topic, String endpoint){
		MessageListenerImpl listener = listeners.get(listenerName);
		if(listener == null){
			listener = new MessageListenerImpl(listenerName);
			listeners.put(listener.getName(), listener);
		}								
		
		logger.debug("Subscribing listener {} to channel {}", listener.getName(), topic.getTopic());
		
		listenerContainer.addMessageListener(listener, topic);	
		listener.addSubscription(topic, endpoint);
	}
	
	private void doRegisterMockSubscription(){
		MessageListenerImpl listener = new MockMessageListenerImpl(DUMMY_TOPIC);
		listenerContainer.addMessageListener(listener, ChannelUtils.buildTopic(DUMMY_TOPIC));	
	}

	@Override
	public void remove(Subscription subscription) {
		// Al eliminar una subscripcion, no solo se deberá eliminar la relación listener - topic del contenedor de listeners, 
		// sino que también se deberá eliminar la subscripcion de este listener a este canal en Redis. 
		
		// Este metodo puede invocarse para:
		// 1. eliminar una subscripcion en concreto (hdel pasando un field)
		// 2. todas las subscripciones de un tipo (hdel pasando más de un field)
		// 3. todas las subscripcione (del de la key)
		// En cada caso el comportamiento es diferente.
		
		if(subscription.getType() == null){
			removeAllSubscriptions(subscription);
		}else if(subscription.hasResourceIdentified()){
			removeSubscription(subscription);
		}else{
			removeSubscriptionsOfOneType(subscription);
		}
	}
	
	private void removeSubscription(Subscription subscription) {				
		Topic topic = ChannelUtils.getChannel(subscription);
		
		logger.debug("Removing subscription to channel {} for listener {} ", topic.getTopic(), subscription.getSourceEntityId());		
		
		MessageListenerImpl listener = listeners.get(subscription.getSourceEntityId());
		if(listener != null){
			// Solo tiene sentido eliminar la subscripcion del contenedor de listeners si el listener esta activo
			listenerContainer.removeMessageListener(listener, topic);
			listener.removeSubscription(topic);
		}
								
		// Eliminamos en Redis la subscripcion del listener al topic
		jedisTemplate.hDel(keysBuilder.getSubscriptionKey(subscription.getSourceEntityId()), topic.getTopic());
		
		logger.debug("Subscription from listener {} subscribe to channel {} removed", subscription.getSourceEntityId(), topic.getTopic());
	}
	
	
	private void removeAllSubscriptions(Subscription subscription){	
		
		logger.debug("Removing all subscriptions for listener {}", subscription.getSourceEntityId());		
		
		// Recuperamos todos los canales a los cuales esta subscrito el listener.
		Set<String> topics = jedisTemplate.hKeys(keysBuilder.getSubscriptionKey(subscription.getSourceEntityId()));
		
		MessageListenerImpl listener = listeners.get(subscription.getSourceEntityId());
		if(listener != null && !CollectionUtils.isEmpty(topics)){
			Iterator<String> it = topics.iterator();
			while(it.hasNext()){
				String topicName = it.next();
				listenerContainer.removeMessageListener(listener, ChannelUtils.buildTopic(topicName));
			}
			
			//Eliminamos el listener de la lista de listeners activos
			listeners.remove(subscription.getSourceEntityId());
		}
								
		// Por ultimo, eliminamos en Redis la hash de subscripciones de este listener:
		jedisTemplate.del(keysBuilder.getSubscriptionKey(subscription.getSourceEntityId()));				
		
		logger.debug("Subscriptions removed for listener {} ", subscription.getSourceEntityId());
	}
	
	private void removeSubscriptionsOfOneType(Subscription subscription){
		//Por ejemplo, puede ser borrar todas las subscripciones de tipo alarm del listener.
		//Esto implica en Redis recuperar primero todas las subscripciones de ese tipo y despues borrar via un hdel
		//En el container se trata de recuperar todas las subscripciones que existen (holders) y para cada una, si se trata 
		//de  una subscripcion del tipo indicado se pasa a eliminar el listener de su lista de listeners.				
		
		logger.debug("Removing all subscriptions of type {} for listener {}", subscription.getType(), subscription.getSourceEntityId());		
		
		// Recuperamos todos los canales a los cuales esta subscrito el listener.
		Set<String> topics = jedisTemplate.hKeys(keysBuilder.getSubscriptionKey(subscription.getSourceEntityId()));
		List<String> topicsToRemove = ChannelUtils.filterTopicsOfType(topics, subscription.getType());
		
		// Y para cada uno, eliminamos la subscripcion activa del listener
		MessageListenerImpl listener = listeners.get(subscription.getSourceEntityId());				
		for(String topicName:topicsToRemove){
			if(listener != null){
				Topic topic = ChannelUtils.buildTopic(topicName);
				listenerContainer.removeMessageListener(listener, topic);
				listener.removeSubscription(topic);
			}
		}
		
		logger.debug("Number of subscriptions to remove in Redis: {}",topicsToRemove.size());
								
		if(!CollectionUtils.isEmpty(topicsToRemove)){
			// Por ultimo, eliminamos en Redis de la hash de subscripciones todas aquellas que corresponden a canales del tipo indicado:
			jedisTemplate.hDel(keysBuilder.getSubscriptionKey(subscription.getSourceEntityId()), topicsToRemove.toArray(new String[0]));				
		}
		
		logger.debug("Subscriptions of type {} removed for listener {} ", subscription.getType(), subscription.getSourceEntityId());
	}

	@Override
	public List<Subscription> get(Subscription subscription) {		
		// Las subscripciones de una entidad estan registradas bajo la clave subs:idEntity en el servidor Redis asociado a la pub/sub
		// El valor asociado a la clave es una hash de pares <channel, endpoint>
		
		logger.debug("Retrieving subscriptions for entity {}", subscription.getSourceEntityId());
		
		List<Subscription> subscriptionList = null;
		String key = keysBuilder.getSubscriptionKey(subscription.getSourceEntityId());
		Map<String,String> subscriptions = jedisTemplate.hGetAll(key);
		if(!CollectionUtils.isEmpty(subscriptions)){
			subscriptionList = new ArrayList<Subscription>();
			Iterator<String> it = subscriptions.keySet().iterator();
			SubscribeType type = subscription.getType();
			while(it.hasNext()){
				String field = it.next();
				if(type==null || ChannelUtils.isTopicOfType(field, type)){
					subscriptionList.add(ChannelUtils.getSubscription(subscription.getSourceEntityId(), field, subscriptions.get(field)));
				}								
			}
		}else{
			subscriptionList = Collections.emptyList();
		}
		
		logger.debug("Entity {} has {} subscriptions", subscription.getSourceEntityId(), subscriptionList.size());
		
		return subscriptionList;		
	}		
				
			
	public void setListenerContainer(RedisMessageListenerContainer listenerContainer) {
		this.listenerContainer = listenerContainer;
	}
			
}
