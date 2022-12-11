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
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

import javax.annotation.PostConstruct;

import org.sentilo.agent.alert.domain.InternalAlert;
import org.sentilo.agent.alert.listener.SensorAlertsRuleEngine;
import org.sentilo.agent.alert.listener.SensorAlertsRuleEngineFactory;
import org.sentilo.agent.alert.repository.FrozenRepository;
import org.sentilo.agent.alert.service.AlertService;
import org.sentilo.agent.alert.service.PublishService;
import org.sentilo.agent.alert.utils.AlertUtils;
import org.sentilo.common.enums.AlertType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.mongodb.core.MongoOperations;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

@Service
public class AlertServiceImpl implements AlertService {

  private static final Logger LOGGER = LoggerFactory.getLogger(AlertServiceImpl.class);

  @Autowired
  private MongoOperations mongoOps;

  @Autowired
  private FrozenRepository frozenRepository;

  @Autowired
  private PublishService publishService;

  @Autowired
  private SensorAlertsRuleEngineFactory factory;

  private boolean active;

  private final Map<String, InternalAlert> frozenAlertsCache = new HashMap<String, InternalAlert>();
  // lookup map between topics and rules engines
  private final Map<String, SensorAlertsRuleEngine> topicMapping = new ConcurrentHashMap<>();

  @PostConstruct
  public void init() {
    loadAndMonitorInternalAlerts();
  }

  @Override
  public SensorAlertsRuleEngine getRuleEngine(final String topic) {
    return topicMapping.get(topic);
  }

  @Override
  public void loadAndMonitorInternalAlerts() {
    frozenAlertsCache.clear();

    // First, retrieve the internal alerts defined in the Catalog repository
    final List<InternalAlert> internalAlerts = getInternalAlerts();

    if (CollectionUtils.isEmpty(internalAlerts)) {
      return;
    }

    // Therefore, groups the alerts by sensor and for each group registers a new
    // SensorAlertsRuleEngine which should be subscribe to get all observations published by this
    // sensor.
    LOGGER.info("Found {} internal alerts to monitor", internalAlerts.size());
    final Set<String> activeTopics = new HashSet<>();
    //@formatter:off
    internalAlerts.forEach(alert -> {
        // Every rule engine will have an unique identifier, defined by the channel name
        // /data/<provider>/<sensor>

        final String topic = AlertUtils.buildAlertDataTopic(alert);
        if (topicMapping.get(topic) == null) {
          topicMapping.put(topic, factory.build(topic));
        }

        topicMapping.get(topic).addAlert(alert);
        LOGGER.debug("Add alert {} to rule engine ", alert.getId(), topic);
        activeTopics.add(topic);

        if (alert.isFrozenType()) {
          frozenAlertsCache.put(alert.getId(), alert);
        }
    });
    //@formatter:on

    // Finally, remove deprecated rule engines ...
    if (!CollectionUtils.isEmpty(topicMapping)) {
      final List<String> topicsToRemove = topicMapping.keySet().stream().filter(e -> !activeTopics.contains(e)).collect(Collectors.toList());
      topicsToRemove.forEach(topic -> topicMapping.remove(topic));
    }

    // ... and register/updates frozen alerts to monitor on the repository
    if (!CollectionUtils.isEmpty(frozenAlertsCache)) {
      frozenRepository.synchronizeAlerts(frozenAlertsCache.values());
    }

    active = true;
  }

  /**
   * Indicates if internal alerts to monitor have been loaded from MongoDB
   *
   * @return
   */
  @Override
  public boolean isActive() {
    return active;
  }

  @Override
  public void checkFrozenAlerts() {
    LOGGER.info("Checking possible frozen sensors");

    // List of candidates to be frozen alerts. This list is retrieved from the frozen repository
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
        // nor the trigger type filled (and both are mandatory fields to publish a new alarm).
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

  private List<InternalAlert> getInternalAlerts() {
    LOGGER.debug("Searching active internal alerts ....");
    final Criteria typeCriteria = Criteria.where("type").is(AlertType.INTERNAL.name());
    final Criteria activeCriteria = Criteria.where("active").is(Boolean.TRUE);
    final Query query = new Query(typeCriteria.andOperator(activeCriteria));

    final List<InternalAlert> content = mongoOps.find(query, InternalAlert.class, "alert");
    LOGGER.debug("... and found {} alerts to monitor.", content.size());
    return content;
  }

}
