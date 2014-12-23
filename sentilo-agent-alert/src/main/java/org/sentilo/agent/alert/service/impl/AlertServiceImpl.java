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

import org.sentilo.agent.alert.domain.InternalAlert;
import org.sentilo.agent.alert.event.CheckFrozenAlertEvent;
import org.sentilo.agent.alert.listener.MessageListenerImpl;
import org.sentilo.agent.alert.repository.FrozenRepository;
import org.sentilo.agent.alert.service.AlertService;
import org.sentilo.agent.alert.service.PublishService;
import org.sentilo.agent.alert.utils.AlertUtils;
import org.sentilo.agent.alert.utils.enums.AlertType;
import org.sentilo.agent.common.listener.MockMessageListenerImpl;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationListener;
import org.springframework.data.mongodb.core.MongoOperations;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.data.redis.connection.MessageListener;
import org.springframework.data.redis.listener.ChannelTopic;
import org.springframework.data.redis.listener.RedisMessageListenerContainer;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

@Service
public class AlertServiceImpl implements AlertService, ApplicationListener<CheckFrozenAlertEvent> {

  private final Logger logger = LoggerFactory.getLogger(AlertServiceImpl.class);
  private static final String DUMMY_TOPIC = "/trash/dummy";

  @Autowired
  private MongoOperations mongoOps;

  @Autowired
  private RedisMessageListenerContainer listenerContainer;

  @Autowired
  private FrozenRepository frozenRepository;

  @Autowired
  private PublishService publishService;

  private final Map<String, MessageListenerImpl> currentListeners = new HashMap<String, MessageListenerImpl>();
  private final Map<String, InternalAlert> frozenAlertsCache = new HashMap<String, InternalAlert>();

  private boolean mockListenerActive = false;

  /*
   * (non-Javadoc)
   * 
   * @see org.sentilo.agent.alert.service.AlertService#loadAndMonitorInternalAlerts()
   */
  public void loadAndMonitorInternalAlerts() {
    // Las alertas a monitorizar vienen dadas por las alertas internas definidas en el catalogo
    // Por lo tanto, el primer paso es recuperar del catálogo todas las alertas de tipo interno:
    // esta petición la haremos accediendo directamente al repositorio del catálogo.

    // Despues agruparemos las alertas por sensor, y para cada grupo registraremos un
    // MessageListener el cual estará subscrito a los datos del sensor y, para cada dato que se
    // reciba, se encargará de procesar la validez del conjunto de reglas asociadas a las alertas
    // del sensor.

    frozenAlertsCache.clear();
    final List<InternalAlert> internalAlerts = getInternalAlerts();

    final Map<String, MessageListenerImpl> listeners = new HashMap<String, MessageListenerImpl>();

    if (!CollectionUtils.isEmpty(internalAlerts)) {
      logger.debug("Found {} internal alerts to monitor", internalAlerts.size());

      for (final InternalAlert alert : internalAlerts) {
        // El identificador unívoco de cada listener vendrá definido por el nombre del canal (topic)
        // al cual estará asociado, que verifica el patrón /data/<provider>/<sensor>, ya que toda
        // alerta interna tiene un proveedor y un sensor.
        final String topicToListen = generateTopicName(alert);
        if (listeners.get(topicToListen) == null) {
          listeners.put(topicToListen, new MessageListenerImpl(topicToListen, publishService, frozenRepository));
        }

        listeners.get(topicToListen).addAlert(alert);

        if (alert.isFrozenType()) {
          frozenAlertsCache.put(alert.getId(), alert);
        }
      }
    } else {

      // Si no se hace ninguna subscripción, el agente no permanecerá a la escucha y se parará. Si
      // lo que queremos es que esté siempre alerta, debemos subscribirlo almenos a un canal (por
      // ejemplo un canal mock y de esta manera forzar que siempre esté escuchando)
      logger.info("No found internal alarms to process. Register dummy listener to keep alive the agent.");
      if (!mockListenerActive) {
        registerSubscriptionIntoContainer(new MockMessageListenerImpl(DUMMY_TOPIC), DUMMY_TOPIC);
        mockListenerActive = true;
      }
    }

    // Finally, sync current alerts/listeners with the new alerts/listeners
    if (!CollectionUtils.isEmpty(listeners)) {
      updateAndRegisterListeners(listeners);
    }

    // ... and register/updates frozen alerts to monitor on the repository
    if (!CollectionUtils.isEmpty(frozenAlertsCache)) {
      frozenRepository.synchronizeAlerts(frozenAlertsCache.values());
    }

  }

  /*
   * (non-Javadoc)
   * 
   * @see
   * org.springframework.context.ApplicationListener#onApplicationEvent(org.springframework.context
   * .ApplicationEvent)
   */
  public void onApplicationEvent(final CheckFrozenAlertEvent event) {
    checkFrozenAlerts();
  }

  public void updateAndRegisterListeners(final Map<String, MessageListenerImpl> listeners) {
    // Para cada listener definido en listeners vemos si ya forma parte de la colección de listeners
    // existentes.
    // 1. Si existe, actualizamos su lista de alertas.
    // 2. Si no existe, lo añadimos y lo registramos en el listenerContainer

    // Por último, si alguno de los listeners ya existentes no forma parte de la nueva lista lo
    // eliminamos tanto de la colección currentListeners como del listenerContainer

    for (final String listenerName : listeners.keySet()) {
      if (currentListeners.containsKey(listenerName)) {
        logger.debug("Updating alerts collection for listener {}", listenerName);
        final MessageListenerImpl listener = listeners.get(listenerName);
        final MessageListenerImpl currentListener = currentListeners.get(listenerName);
        currentListener.updateAlerts(listener.getAlerts());
      } else {
        logger.debug("Registering new listener to topic {}", listenerName);
        final MessageListenerImpl listener = listeners.get(listenerName);
        currentListeners.put(listenerName, listener);
        registerSubscriptionIntoContainer(listener, listenerName);
      }
    }

    // Finally, unsubscribe and remove listeners from the container that are not longer required
    checkAndRemoveDeprecatedListeners(listeners);
  }

  private void checkAndRemoveDeprecatedListeners(final Map<String, MessageListenerImpl> listeners) {
    final List<String> listenersToRemove = new ArrayList<String>();
    for (final String listenerName : currentListeners.keySet()) {
      if (!listeners.containsKey(listenerName)) {
        listenersToRemove.add(listenerName);
      }
    }

    removeDeprecatedListeners(listenersToRemove);
  }

  private void removeDeprecatedListeners(final List<String> listenersToRemove) {
    for (final String listenerName : listenersToRemove) {
      logger.debug("Removing listener to topic {}", listenerName);
      listenerContainer.removeMessageListener(currentListeners.get(listenerName));
      currentListeners.remove(listenerName);
    }
  }

  private List<InternalAlert> getInternalAlerts() {
    logger.debug("Searching internal alerts ....");
    final Criteria queryCriteria = Criteria.where("type").is(AlertType.INTERNAL.name());
    final List<InternalAlert> content = mongoOps.find(new Query(queryCriteria), InternalAlert.class, "alert");
    logger.debug("... and found {} alerts to monitor.", content.size());
    return content;
  }

  private String generateTopicName(final InternalAlert alert) {
    return AlertUtils.buildDataTopicAssociateToAlert(alert);
  }

  private void registerSubscriptionIntoContainer(final MessageListener messageListener, final String topic) {
    listenerContainer.addMessageListener(messageListener, new ChannelTopic(topic));
    logger.debug("Subscription to topic {} registered succesfully", topic);
  }

  private void checkFrozenAlerts() {
    logger.info("Checking possible frozen sensors");

    // Each frozen alert retrieve from the repository doesn't have the expression value filled in.
    // (and it is mandatory to publish an alarm). This value is given from the frozenAlertsCache
    final List<InternalAlert> frozenAlerts = frozenRepository.checkFrozenAlerts();
    final List<InternalAlert> updatedFrozenAlerts = new ArrayList<InternalAlert>();
    for (final InternalAlert frozenAlert : frozenAlerts) {
      final InternalAlert aux = frozenAlertsCache.get(frozenAlert.getId());
      // If cache doesn't contains the alert means that this alert is deprecated in the repository
      // and must be ignored.
      if (aux != null) {
        frozenAlert.setExpression(aux.getExpression());
        publishService.publishFrozenAlarm(frozenAlert);
        updatedFrozenAlerts.add(frozenAlert);
      }
    }

    logger.info("Found and published {} frozen alarms", updatedFrozenAlerts.size());

    // and finally, updates the repository timeouts for every frozen alert
    frozenRepository.updateFrozenTimeouts(updatedFrozenAlerts);
  }

}
