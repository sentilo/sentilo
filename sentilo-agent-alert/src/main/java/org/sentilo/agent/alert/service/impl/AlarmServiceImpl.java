/*
 * Sentilo
 * 
 * Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de Barcelona.
 * 
 * This program is licensed and may be used, modified and redistributed under the terms of the
 * European Public License (EUPL), either version 1.1 or (at your option) any later version as soon
 * as they are approved by the European Commission.
 * 
 * Alternatively, you may redistribute and/or modify this program under the terms of the GNU Lesser
 * General Public License as published by the Free Software Foundation; either version 3 of the
 * License, or (at your option) any later version.
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied.
 * 
 * See the licenses for the specific language governing permissions, limitations and more details.
 * 
 * You should have received a copy of the EUPL1.1 and the LGPLv3 licenses along with this program;
 * if not, you may find them at:
 * 
 * https://joinup.ec.europa.eu/software/page/eupl/licence-eupl http://www.gnu.org/licenses/ and
 * https://www.gnu.org/licenses/lgpl.txt
 */
package org.sentilo.agent.alert.service.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Executor;

import org.sentilo.agent.alert.domain.Alarm;
import org.sentilo.agent.alert.event.CheckFrozenAlarmEvent;
import org.sentilo.agent.alert.listener.MessageListenerImpl;
import org.sentilo.agent.alert.listener.MockMessageListenerImpl;
import org.sentilo.agent.alert.service.AlarmService;
import org.sentilo.agent.alert.utils.AlertUtils;
import org.sentilo.agent.alert.utils.enums.AlarmType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationListener;
import org.springframework.core.task.SimpleAsyncTaskExecutor;
import org.springframework.data.mongodb.core.MongoOperations;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.data.redis.connection.MessageListener;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.listener.ChannelTopic;
import org.springframework.data.redis.listener.RedisMessageListenerContainer;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

@Service
public class AlarmServiceImpl implements AlarmService, ApplicationListener<CheckFrozenAlarmEvent> {

  private final Logger logger = LoggerFactory.getLogger(AlarmServiceImpl.class);
  private static final String DUMMY_TOPIC = "trash:dummy";

  @Autowired
  private MongoOperations mongoOps;

  @Autowired
  private RedisMessageListenerContainer listenerContainer;

  @Autowired
  private RedisTemplate<String, String> redisTemplate;

  private final Map<String, MessageListenerImpl> currentListeners = new HashMap<String, MessageListenerImpl>();

  private final Executor taskExecutor = new SimpleAsyncTaskExecutor();

  /*
   * (non-Javadoc)
   * 
   * @see org.sentilo.agent.alert.service.AlarmService#loadAndSubscribeToInternalAlarms()
   */
  public void loadAndSubscribeToInternalAlarms() {
    // Las subscripciones a registrar vienen dadas por las alarmas internas definidas en el catalogo
    // de la PSAB.
    // Por lo tanto, el primer paso es recuperar del catálogo todas las alarmas de tipo interno:
    // esta petición la haremos
    // accediendo directamente al repositorio del catálogo.
    // Despues para cada una de las alarmas, registraremos un nuevo listener asociado a los datos
    // del sensor asociado a la alarmas.
    logger.info("Initializing alarm agent subscriptions");
    final List<Alarm> internalAlarms = getInternalAlarms();

    // Si no se hace ningún registro, el agente no permanecerá a la escucha y se parará. Si lo que
    // queremos es que esté siempre
    // alerta, deberíamos subscribirlo almenos a un canal (por ejemplo uno mock y de esta manera
    // forzar que siempre esté escuchando)

    // Tendremos un listener por cada sensor al cual debamos estar subscritos. Este listener deberá
    // procesar las N alarmas
    // asociadas a los datos de ese sensor.
    final Map<String, MessageListenerImpl> listeners = new HashMap<String, MessageListenerImpl>();

    if (!CollectionUtils.isEmpty(internalAlarms)) {
      logger.debug("Found {} internal alarms to process", internalAlarms.size());

      for (final Alarm alarm : internalAlarms) {
        // El identificador unívoco de cada listener vendrá definido por el nombre del canal (topic)
        // al cual estará asociado.
        // Y toda alarma interna tiene asociada un proveedor y un sensor. Por lo tanto, tenemos un
        // listener asociado a cada
        // uno de los sensores para los cuales hay alarmas. Este listener se encarga de evaluar,
        // cada vez que llega un dato,
        // las diferentes alaarmas asociadas al sensor.
        final String topicToListen = generateTopicName(alarm);

        if (listeners.get(topicToListen) == null) {
          listeners.put(topicToListen, new MessageListenerImpl(topicToListen, redisTemplate));
        }

        listeners.get(topicToListen).addAlarm(alarm);
      }
    } else {
      logger.info("No found internal alarms to process. Register dummy listener to keep alive the agent.");
      listeners.put(DUMMY_TOPIC, new MockMessageListenerImpl(DUMMY_TOPIC));
    }

    if (!CollectionUtils.isEmpty(listeners)) {
      updateAndRegisterListeners(listeners);
    }
  }

  /*
   * (non-Javadoc)
   * 
   * @see
   * org.springframework.context.ApplicationListener#onApplicationEvent(org.springframework.context
   * .ApplicationEvent)
   */
  public void onApplicationEvent(final CheckFrozenAlarmEvent event) {
    logger.debug("Checking possible frozen alarms");
    for (final MessageListenerImpl messageListener : currentListeners.values()) {
      taskExecutor.execute(new Runnable() {

        public void run() {
          messageListener.checkFrozenAlarm();
        }
      });
    }
  }

  public void updateAndRegisterListeners(final Map<String, MessageListenerImpl> listeners) {
    // Para cada listener definido en listeners vemos si ya forma parte de la colección de listeners
    // existentes.
    // 1. Si existe, actualizamos u lista de alarmas
    // 2. Si no existe, lo añadimos y registramos en el listenerContainer
    // Por último, si alguno de los listeners ya existentes no forma parte de la nueva lista lo
    // eliminamos tanto de la colección currentListeners
    // como del listenerContainer

    for (final String listenerName : listeners.keySet()) {
      if (currentListeners.containsKey(listenerName)) {
        logger.debug("Updating alarms collection from listener {}", listenerName);
        final MessageListenerImpl listener = listeners.get(listenerName);
        final MessageListenerImpl currentListener = listeners.get(listenerName);
        currentListener.updateAlarms(listener.getAlarms());

      } else {
        logger.debug("Registering new listener to topic {}", listenerName);
        final MessageListenerImpl listener = listeners.get(listenerName);
        currentListeners.put(listenerName, listener);
        registerSubscriptionIntoContainer(listener, listenerName);
      }
    }

    // Eliminamos listeners que han quedado invalidos.
    final List<String> listenersToRemove = new ArrayList<String>();
    for (final String listenerName : currentListeners.keySet()) {
      if (!listeners.containsKey(listenerName)) {
        listenersToRemove.add(listenerName);
      }
    }

    removeInvalidListeners(listenersToRemove);
  }

  private void removeInvalidListeners(final List<String> listenersToRemove) {
    for (final String listenerName : listenersToRemove) {
      logger.debug("Removing listener {}", listenerName);
      listenerContainer.removeMessageListener(currentListeners.get(listenerName));
      currentListeners.remove(listenerName);
    }
  }

  private List<Alarm> getInternalAlarms() {
    logger.debug("Searching internal alarms ....");
    final Criteria queryCriteria = Criteria.where("type").is(AlarmType.INTERNAL.name());
    final List<Alarm> content = mongoOps.find(new Query(queryCriteria), Alarm.class);
    logger.debug("... and found {} alarms to process.", content.size());
    return content;
  }

  private String generateTopicName(final Alarm alarm) {
    return AlertUtils.buildDataTopicAssociateToAlarm(alarm);
  }

  private void registerSubscriptionIntoContainer(final MessageListener messageListener, final String topic) {
    listenerContainer.addMessageListener(messageListener, new ChannelTopic(topic));
    logger.debug("Subscription to topic {} registered succesfully", topic);
  }

  public void setListenerContainer(final RedisMessageListenerContainer listenerContainer) {
    this.listenerContainer = listenerContainer;
  }

  public void setMongoOps(final MongoOperations mongoOps) {
    this.mongoOps = mongoOps;
  }

  public void setRedisTemplate(final RedisTemplate<String, String> redisTemplate) {
    this.redisTemplate = redisTemplate;
  }

}
