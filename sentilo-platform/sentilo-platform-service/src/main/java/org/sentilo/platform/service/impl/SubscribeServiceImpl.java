/*
 * Sentilo
 *  
 * Original version 1.4 Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de Barcelona.
 * Modified by Opentrends adding support for multitenant deployments and SaaS. Modifications on version 1.5 Copyright (C) 2015 Opentrends Solucions i Sistemes, S.L.
 * 
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
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.sentilo.common.domain.SubscribeType;
import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.platform.common.domain.Subscription;
import org.sentilo.platform.common.service.SubscribeService;
import org.sentilo.platform.service.listener.MessageListenerFactory;
import org.sentilo.platform.service.listener.MessageListenerImpl;
import org.sentilo.platform.service.listener.MockMessageListenerImpl;
import org.sentilo.platform.service.listener.NotificationParams;
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
import org.springframework.util.StringUtils;

@Service
public class SubscribeServiceImpl extends AbstractPlatformServiceImpl implements SubscribeService {

  private static final Logger LOGGER = LoggerFactory.getLogger(SubscribeServiceImpl.class);

  @Autowired
  private RedisMessageListenerContainer listenerContainer;
  @Autowired
  private MessageListenerFactory listenerFactory;

  private final Map<String, MessageListenerImpl> listeners = new HashMap<String, MessageListenerImpl>();

  private boolean storedSubscriptionsActivated = false;
  private static final String DUMMY_TOPIC = "/trash/dummy";

  @Scheduled(initialDelay = 30000, fixedDelay = 300000)
  public void loadSubscriptions() {

    // Al iniciar la plataforma, en caso de existir subscripciones registradas en Redis,
    // inicializamos los MessageListeners correspondientes y los
    // registramos en el container:
    // 1. Recuperamos todas las claves del tipo REDIS_SUBS_PATTERN_KEY
    // 2. Para cada clave, recuperamos la información relativa al cjto de subscripciones asociadas
    // 3. Para cada subscripcion inicializamos el messagelistener correspondiente
    //
    // IMPORTANTE: en caso de no existir ninguna subscripción registrada, igualmente inicializamos
    // un MessageListener que se ponga a escuchar por un canal dummy
    // ya que sino el container acaba lanzando una excepción del tipo" ERR wrong number of arguments
    // for 'psubscribe' command;" la cual está asociada a que la conexión de subscripción no es
    // válida
    final boolean listenerContainerRunning = listenerContainer != null && listenerContainer.isRunning();
    LOGGER.debug("Listener container isRunning? {}", listenerContainerRunning);

    if (!storedSubscriptionsActivated && listenerContainerRunning) {
      LOGGER.debug("Initialize subscriptions already registered in Redis");
      try {
        final Set<String> storedSubscriptions = jedisTemplate.keys(PubSubConstants.REDIS_SUBS_PATTERN_KEY);
        activateStoredSubscriptions(storedSubscriptions);
        storedSubscriptionsActivated = true;
      } catch (final Exception e) {
        storedSubscriptionsActivated = false;
      }
    }
  }

  private void activateStoredSubscriptions(final Set<String> storedSubscriptions) {
    if (CollectionUtils.isEmpty(storedSubscriptions)) {
      doRegisterMockSubscription();
      return;
    }

    LOGGER.debug("Found {} subscriptions stored in Redis", storedSubscriptions.size());

    for (final String subscriptionKey : storedSubscriptions) {
      // Cada subscriptionKey corresponde a una entidad subscrita a N canales de la plataforma
      // Toda la información de cada una de estas subscripciones está almacenada en una hash,
      // donde cada entrada corresponde a la info <canal,endpoint>
      final Map<String, String> storedSubscription = jedisTemplate.hGetAll(subscriptionKey);
      if (CollectionUtils.isEmpty(storedSubscription)) {
        return;
      }

      final Set<String> channels = storedSubscription.keySet();
      final String listenerName = listenerNameFromSubscriptionKey(subscriptionKey);

      for (final String channel : channels) {
        activateSubscription(listenerName, ChannelUtils.buildTopic(channel), storedSubscription.get(channel));
      }
    }
  }

  private String listenerNameFromSubscriptionKey(final String subscriptionKey) {
    // subscriptionKey follows the expression subs:<listenerName>
    final int pos = subscriptionKey.lastIndexOf(PubSubConstants.REDIS_KEY_TOKEN);
    return subscriptionKey.substring(pos + 1);
  }

  @Override
  public void subscribe(final Subscription subscription) {
    // Al subscribirse, no sólo se debe habilitar el listener correspondiente, sino que tb se debe
    // persistir en Redis la subscripcion para la entidad de turno. De esta manera se podrán
    // iniciar los listeners asociados a las subscripciones ya existentes cuando se arranque
    // este modulo.

    // Estos registros en Redis serán del tipo Hash y habrá uno para cada entidad.
    // Es decir, para cada entidad que este subscrita a algun canal tendremos en Redis
    // una Hash con key igual a subs:<entityId> y N entradas <field, value> donde:
    // - field: cada campo de la hash corresponderá a una subscripcion , por lo que el nombre del
    // campo identificará el canal al cual se está subscrito <event_type>/element_id, donde
    // element_id es el identificador del recurso al cual se esta subscrito.
    // - value: el value del campo contiene la informacion necesaria para realizar la notificacion
    // via HTTP Callback (estos datos son el endpoint, la secretKey a utilizar, ...)

    final Topic topic = ChannelUtils.getChannel(subscription);

    final String notificationParamsChain = buildNotificationParams(subscription);

    // Habilitamos listener
    activateSubscription(subscription.getSourceEntityId(), topic, notificationParamsChain);

    // Persistimos en Redis la subscripcion
    jedisTemplate.hSet(keysBuilder.getSubscriptionKey(subscription.getSourceEntityId()), topic.getTopic(), notificationParamsChain);

    LOGGER.debug("Listener {} subscribe to channel {}", subscription.getSourceEntityId(), topic.getTopic());
  }

  private String buildNotificationParams(final Subscription subscription) {
    // Como el valor en las hash es un String, definimos un formato con el cual almacenar un String
    // que contenga todos los parametros necesarios para realizar la notificacion (via callback):
    // value = param1#@#param2#@#...#@#paramN
    final StringBuilder sb = new StringBuilder(subscription.getEndpoint());
    if (StringUtils.hasText(subscription.getSecretCallbackKey())) {
      sb.append(SentiloConstants.SENTILO_INTERNAL_TOKEN).append(subscription.getSecretCallbackKey());
    }

    return sb.toString();
  }

  private void activateSubscription(final String listenerName, final Topic topic, final String notificationParamsChain) {
    MessageListenerImpl listener = listeners.get(listenerName);
    if (listener == null) {
      listener = addNewListener(listenerName);
    }

    LOGGER.debug("Subscribing listener {} to channel {}", listener.getName(), topic.getTopic());

    listenerContainer.addMessageListener(listener, topic);
    listener.addSubscription(topic, new NotificationParams(notificationParamsChain));
  }

  private MessageListenerImpl addNewListener(final String listenerName) {
    MessageListenerImpl listener = null;
    try {
      listenerFactory.setListenerName(listenerName);
      listener = listenerFactory.getObject();
      listeners.put(listener.getName(), listener);
    } catch (final Exception e) {
      LOGGER.warn("Has been unable to build MessageListener {}. ", listenerName, e);
    }
    return listener;

  }

  private void doRegisterMockSubscription() {
    LOGGER.debug("Not found stored subscriptions in Redis.");
    LOGGER.debug("Registering a mock subscription to channel {}", DUMMY_TOPIC);
    final MessageListenerImpl listener = new MockMessageListenerImpl(DUMMY_TOPIC);
    listenerContainer.addMessageListener(listener, ChannelUtils.buildTopic(DUMMY_TOPIC));
  }

  @Override
  public void remove(final Subscription subscription) {
    // Al eliminar una subscripcion, no solo se deberá eliminar la relación listener - topic del
    // contenedor de listeners, sino que también se deberá eliminar la subscripcion de este
    // listener a este canal en Redis.

    // Este metodo puede invocarse para:
    // 1. eliminar una subscripcion en concreto (hdel pasando un field)
    // 2. todas las subscripciones de un tipo (hdel pasando más de un field)
    // 3. todas las subscripciones (de la entidad identificada por la key)
    // En cada caso el comportamiento es diferente.

    if (subscription.getType() == null) {
      removeAllSubscriptions(subscription);
    } else if (subscription.hasResourceIdentified()) {
      removeSubscription(subscription);
    } else {
      removeSubscriptionsOfOneType(subscription);
    }
  }

  private void removeSubscription(final Subscription subscription) {
    final Topic topic = ChannelUtils.getChannel(subscription);

    LOGGER.debug("Removing subscription to channel {} for listener {} ", topic.getTopic(), subscription.getSourceEntityId());

    final MessageListenerImpl listener = listeners.get(subscription.getSourceEntityId());
    if (listener != null) {
      listenerContainer.removeMessageListener(listener, topic);
      listener.removeSubscription(topic);
    }

    // Eliminamos en Redis la subscripcion del listener al topic
    jedisTemplate.hDel(keysBuilder.getSubscriptionKey(subscription.getSourceEntityId()), topic.getTopic());

    LOGGER.debug("Subscription from listener {} subscribe to channel {} removed", subscription.getSourceEntityId(), topic.getTopic());
  }

  private void removeAllSubscriptions(final Subscription subscription) {
    LOGGER.debug("Removing all subscriptions for listener {}", subscription.getSourceEntityId());

    final MessageListenerImpl listener = listeners.get(subscription.getSourceEntityId());

    if (listener != null) {
      // Utilizamos el nuevo método expuesto por el Container para eliminar
      // a un listener de todas las subscripciones.
      listenerContainer.removeMessageListener(listener);

      // Eliminamos el listener de la lista de listeners activos
      listeners.remove(subscription.getSourceEntityId());
    }

    // Por ultimo, eliminamos en Redis la hash de subscripciones de este listener:
    jedisTemplate.del(keysBuilder.getSubscriptionKey(subscription.getSourceEntityId()));

    LOGGER.debug("Subscriptions removed for listener {} ", subscription.getSourceEntityId());
  }

  private void removeSubscriptionsOfOneType(final Subscription subscription) {
    // Por ejemplo, puede ser borrar todas las subscripciones de tipo alarm del listener.
    // Esto implica en Redis recuperar primero todas las subscripciones de ese tipo y despues borrar
    // via un hdel
    // En el container se trata de recuperar todas las subscripciones que existen (holders) y para
    // cada una, si se trata de una subscripcion del tipo indicado se pasa a eliminar el listener
    // de su lista de listeners.

    LOGGER.debug("Removing all subscriptions of type {} for listener {}", subscription.getType(), subscription.getSourceEntityId());

    // Recuperamos todos los canales a los cuales esta subscrito el listener.
    final Set<String> topics = jedisTemplate.hKeys(keysBuilder.getSubscriptionKey(subscription.getSourceEntityId()));
    final List<String> topicsToRemove = ChannelUtils.filterTopicsOfType(topics, subscription.getType());

    // Y para cada uno, eliminamos la subscripcion activa del listener
    final MessageListenerImpl listener = listeners.get(subscription.getSourceEntityId());
    for (final String topicName : topicsToRemove) {
      if (listener != null) {
        final Topic topic = ChannelUtils.buildTopic(topicName);
        listenerContainer.removeMessageListener(listener, topic);
        listener.removeSubscription(topic);
      }
    }

    LOGGER.debug("Number of subscriptions to remove in Redis: {}", topicsToRemove.size());

    if (!CollectionUtils.isEmpty(topicsToRemove)) {
      // Por ultimo, eliminamos en Redis de la hash de subscripciones todas aquellas que
      // corresponden a canales del tipo indicado:
      jedisTemplate.hDel(keysBuilder.getSubscriptionKey(subscription.getSourceEntityId()), topicsToRemove.toArray(new String[0]));
    }

    LOGGER.debug("Subscriptions of type {} removed for listener {} ", subscription.getType(), subscription.getSourceEntityId());
  }

  @Override
  public List<Subscription> get(final Subscription subscription) {
    // Las subscripciones de una entidad estan registradas bajo la clave subs:idEntity en Redis
    // El valor asociado a la clave es una hash de pares <channel, notificationParamsChain
    // --> endpoint#@#secret> donde cada channel representa una subscripción activa de esa entidad.

    LOGGER.debug("Retrieving active subscriptions for entity {}", subscription.getSourceEntityId());

    final String key = keysBuilder.getSubscriptionKey(subscription.getSourceEntityId());
    final Map<String, String> subscriptions = jedisTemplate.hGetAll(key);
    final List<Subscription> subscriptionList = buildEntitySubscriptions(subscription, subscriptions);

    LOGGER.debug("Entity {} has {} subscriptions", subscription.getSourceEntityId(), subscriptionList.size());

    return subscriptionList;
  }

  private List<Subscription> buildEntitySubscriptions(final Subscription subscription, final Map<String, String> subscriptions) {
    final List<Subscription> subscriptionList = new ArrayList<Subscription>();
    if (!CollectionUtils.isEmpty(subscriptions)) {
      final Iterator<String> it = subscriptions.keySet().iterator();
      final SubscribeType filterType = subscription.getType();
      while (it.hasNext()) {
        final String field = it.next();
        if (filterType == null || ChannelUtils.isTopicOfType(field, filterType)) {
          subscriptionList.add(ChannelUtils.getSubscription(subscription.getSourceEntityId(), field, subscriptions.get(field)));
        }
      }
    }

    return subscriptionList;
  }

}
