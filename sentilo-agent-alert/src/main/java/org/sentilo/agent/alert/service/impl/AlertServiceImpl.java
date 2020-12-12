/*
 * Sentilo
 *
 * Original version 1.4 Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de
 * Barcelona. Modified by Opentrends adding support for multitenant deployments and SaaS.
 * Modifications on version 1.5 Copyright (C) 2015 Opentrends Solucions i Sistemes, S.L.
 *
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
import org.sentilo.agent.common.metrics.AgentMetricsCounter;
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

  private static final Logger LOGGER = LoggerFactory.getLogger(AlertServiceImpl.class);
  private static final String DUMMY_TOPIC = "/trash/dummy";

  @Autowired
  private MongoOperations mongoOps;

  @Autowired
  private RedisMessageListenerContainer listenerContainer;

  @Autowired
  private FrozenRepository frozenRepository;

  @Autowired
  private PublishService publishService;

  @Autowired
  private AgentMetricsCounter metricsCounters;

  private final Map<String, MessageListenerImpl> currentListeners = new HashMap<String, MessageListenerImpl>();
  private final Map<String, InternalAlert> frozenAlertsCache = new HashMap<String, InternalAlert>();

  private boolean mockListenerActive = false;

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.agent.alert.service.AlertService#loadAndMonitorInternalAlerts()
   */
  public void loadAndMonitorInternalAlerts() {

    frozenAlertsCache.clear();

    // First, retrieve the internal alerts defined in the Catalog repository
    final List<InternalAlert> internalAlerts = getInternalAlerts();

    final Map<String, MessageListenerImpl> listeners = new HashMap<String, MessageListenerImpl>();

    // Therefore, groups the alerts by sensor and for each group registers a new MessageListener
    // which should be subscribe to get all observations published by this sensor.

    if (!CollectionUtils.isEmpty(internalAlerts)) {
      LOGGER.debug("Found {} internal alerts to monitor", internalAlerts.size());

      for (final InternalAlert alert : internalAlerts) {
        // Every listener will have an unique identifier, defined by the channel name
        // /data/<provider>/<sensor>

        final String topicToListen = generateTopicName(alert);
        if (listeners.get(topicToListen) == null) {
          listeners.put(topicToListen, createMessageListener(topicToListen));
        }

        listeners.get(topicToListen).addAlert(alert);

        if (alert.isFrozenType()) {
          frozenAlertsCache.put(alert.getId(), alert);
        }
      }
    } else {
      // If no subscriptions are done, the agent will be stopped. Therefore, if we want to ensure
      // that the agent will be always running we should have at least one subscription: a
      // subscription to a mock channel.
      LOGGER.info("No found internal alerts to process. A dummy listener will be registered to keep alive the agent.");
      if (!mockListenerActive) {
        registerSubscriptionIntoContainer(new MockMessageListenerImpl(DUMMY_TOPIC), DUMMY_TOPIC);
        mockListenerActive = true;
      }
    }

    // Finally, sync current alerts/listeners with the new alerts/listeners loaded from the
    // repository
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
    // For each listener, the method verifies if it is member of the currentListeners list:
    // 1. If already exists, the method updates its internal alert list
    // 2. Otherwise, adds it to the currentListeners list and registers it into the
    // listenerContainer.

    // Finally, if any of the listeners into currentListeners is out-of-dated (i.e. it is not member
    // of the listeners list), it should be deleted from both the currentListeners list and the
    // listener container.

    for (final String listenerName : listeners.keySet()) {
      if (currentListeners.containsKey(listenerName)) {
        LOGGER.debug("Updating alerts collection for listener {}", listenerName);
        final MessageListenerImpl listener = listeners.get(listenerName);
        final MessageListenerImpl currentListener = currentListeners.get(listenerName);
        currentListener.updateAlerts(listener.getAlerts());
      } else {
        LOGGER.debug("Registering new listener to topic {}", listenerName);
        final MessageListenerImpl listener = listeners.get(listenerName);
        currentListeners.put(listenerName, listener);
        registerSubscriptionIntoContainer(listener, listenerName);
      }
    }

    // Unsubscribe and remove listeners from the container that are not longer required
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
      LOGGER.debug("Removing listener to topic {}", listenerName);
      listenerContainer.removeMessageListener(currentListeners.get(listenerName));
      currentListeners.remove(listenerName);
    }
  }

  private List<InternalAlert> getInternalAlerts() {
    LOGGER.debug("Searching active internal alerts ....");
    final Criteria typeCriteria = Criteria.where("type").is(AlertType.INTERNAL.name());
    final Criteria activeCriteria = Criteria.where("active").is(Boolean.TRUE);
    final Query query = new Query(typeCriteria.andOperator(activeCriteria));

    final List<InternalAlert> content = mongoOps.find(query, InternalAlert.class, "alert");
    LOGGER.debug("... and found {} alerts to monitor.", content.size());
    return content;
  }

  private String generateTopicName(final InternalAlert alert) {
    return AlertUtils.buildAlertDataTopic(alert);
  }

  private void registerSubscriptionIntoContainer(final MessageListener messageListener, final String topic) {
    listenerContainer.addMessageListener(messageListener, new ChannelTopic(topic));
    LOGGER.debug("Subscription to topic {} registered succesfully", topic);
  }

  private void checkFrozenAlerts() {
    LOGGER.info("Checking possible frozen sensors");

    // List of candidates to be frozen alerts. This list is retrieve from the frozen repository
    final List<InternalAlert> frozenAlerts = frozenRepository.checkFrozenAlerts();

    // List of real frozen alerts that should update the frozen timeout once the alarms will be
    // published.
    final List<InternalAlert> updatedFrozenAlerts = new ArrayList<InternalAlert>();

    for (final InternalAlert frozenAlert : frozenAlerts) {
      final InternalAlert aux = frozenAlertsCache.get(frozenAlert.getId());
      // If cache doesn't contains the alert means that this alert is deprecated in the repository
      // and must be ignored.
      if (aux != null) {
        // Each frozen alert retrieved from the repository doesn't have neither the expression value
        // filled in nor the trigger type (and them are mandatory fields to publish a new alarm).
        // These values are given from the frozenAlertsCache
        frozenAlert.setExpression(aux.getExpression());
        frozenAlert.setTrigger(aux.getTrigger());
        publishService.publishFrozenAlarm(frozenAlert);
        updatedFrozenAlerts.add(frozenAlert);
      }
    }

    LOGGER.info("Found and published {} frozen alarms", updatedFrozenAlerts.size());

    // and finally, updates the repository timeouts for each frozen alert
    frozenRepository.updateFrozenTimeouts(updatedFrozenAlerts);
  }

  private MessageListenerImpl createMessageListener(final String topicToListen) {
    final MessageListenerImpl aux = new MessageListenerImpl(topicToListen, publishService, frozenRepository);
    aux.setMetricsCounters(metricsCounters);
    return aux;
  }

}
