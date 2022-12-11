/*
 * Sentilo
 *  
 * Original version 1.4 Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de
 * Barcelona. Modified by Opentrends adding support for multitenant deployments and SaaS. 
 * Modifications on version 1.5 Copyright (C) 2015 Opentrends Solucions i Sistemes, S.L.
 * 
 *   
 * This program is licensed and may be used, modified and redistributed under the terms  of the 
 * European Public License (EUPL), either version 1.1 or (at your option) any later version as soon 
 * as they are approved by the European Commission.
 *   
 * Alternatively, you may redistribute and/or modify this program under the terms of the GNU Lesser 
 * General Public License as published by the Free Software Foundation; either  version 3 of the 
 * License, or (at your option) any later version. 
 *   
 * Unless required by applicable law or agreed to in writing, software distributed under the License 
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR  CONDITIONS OF ANY KIND, either express 
 * or implied. 
 *   
 * See the licenses for the specific language governing permissions, limitations and more details.
 *   
 * You should have received a copy of the EUPL1.1 and the LGPLv3 licenses along with this program; 
 * if not, you may find them at: 
 *   
 * https://joinup.ec.europa.eu/software/page/eupl/licence-eupl http://www.gnu.org/licenses/   and 
 * https://www.gnu.org/licenses/lgpl.txt
 */
package org.sentilo.agent.alert.listener;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import javax.annotation.PostConstruct;

import org.sentilo.agent.alert.domain.InternalAlert;
import org.sentilo.agent.alert.repository.FrozenRepository;
import org.sentilo.agent.alert.service.PublishService;
import org.sentilo.agent.alert.trigger.TriggerEvaluator;
import org.sentilo.agent.alert.trigger.TriggerResult;
import org.sentilo.common.domain.EventMessage;
import org.sentilo.common.domain.QueryFilterParams;
import org.sentilo.common.enums.AlertTriggerType;
import org.sentilo.common.lock.LockFactory;
import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.platform.client.core.PlatformClientOperations;
import org.sentilo.platform.client.core.domain.DataInputMessage;
import org.sentilo.platform.client.core.domain.ObservationsOutputMessage;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

/**
 * This class groups together all alerts defined over a sensor and is responsible for evaluating
 * each one every time a new sensor data arrived to agent.
 *
 * @see SensorAlertsRuleEngineFactory#build(String)
 */
@Component
@Scope(BeanDefinition.SCOPE_PROTOTYPE)
public class DefaultSensorAlertsRuleEngineImpl implements SensorAlertsRuleEngine {

  private final static String EVALUATE_VALUE_LOCK_NAME_PREFIX = SensorAlertsRuleEngine.class.getName() + ".evaluate";
  private final static String LOCK_NAME_PREFIX = SensorAlertsRuleEngine.class.getName();
  private final static String LOCK_NAME_SUFFIX = ".lock";

  @Autowired
  private LockFactory lockFactory;
  @Autowired
  private PublishService publishService;
  @Autowired
  private FrozenRepository frozenRepository;
  @Autowired
  private PlatformClientOperations platformClient;

  private final String name;
  private String distributedLockName;
  private String localLockName;

  // To improve
  private String provider;
  private String sensor;

  private final TriggerEvaluator triggerEvaluator;
  private List<InternalAlert> alerts;

  public DefaultSensorAlertsRuleEngineImpl(final String name) {
    this.name = name;
    triggerEvaluator = new TriggerEvaluator();
  }

  @PostConstruct
  public void init() {
    distributedLockName = EVALUATE_VALUE_LOCK_NAME_PREFIX + "." + name + "." + LOCK_NAME_SUFFIX;
    localLockName = LOCK_NAME_PREFIX + "." + name + "." + LOCK_NAME_SUFFIX;

    // name follows rule /data/{provider}/{sensor}
    final String[] tokens = name.split(SentiloConstants.SLASH);
    provider = tokens[2];
    sensor = tokens[3];
  }

  @Override
  public String getName() {
    return name;
  }

  @Override
  public void process(final EventMessage eventMessage) {
    final String value = eventMessage.getMessage();
    lockFactory.getLock(distributedLockName).lock();
    try {
      // Retrieve previous sensor value (only if there are alerts of type CHANGE o CHANGE_DELTA)
      if (isPreviousValueRequired()) {
        readAndSetPreviousValue(eventMessage);
      }

      // For each registered alert, the message value must be checked to validate that verifies all
      // alerts's restriction rules (for not frozen alerts)
      final List<InternalAlert> alertsToCheck = getAlerts();
      final List<InternalAlert> frozenAlerts = new ArrayList<InternalAlert>();

      if (!CollectionUtils.isEmpty(alertsToCheck)) {
        for (final InternalAlert alert : alertsToCheck) {
          if (AlertTriggerType.FROZEN.name().equals(alert.getTrigger().name())) {
            frozenAlerts.add(alert);
          } else {
            valueVerifiesRestriction(alert, value);
          }
        }
      }

      // Finally, updates the timeout for each frozen alert associated with this listener
      if (!CollectionUtils.isEmpty(frozenAlerts)) {
        frozenRepository.updateFrozenTimeouts(frozenAlerts);
      }
    } finally {
      lockFactory.getLock(distributedLockName).unlock();
    }
  }

  @Override
  public void addAlert(final InternalAlert alert) {
    lockFactory.getLock(localLockName).lock();
    try {
      if (alerts == null) {
        alerts = new ArrayList<InternalAlert>();
      }
      alerts.add(alert);
    } finally {
      lockFactory.getLock(localLockName).unlock();
    }
  }

  public void updateAlerts(final List<InternalAlert> newAlerts) {
    lockFactory.getLock(localLockName).lock();
    try {
      alerts = newAlerts;
    } finally {
      lockFactory.getLock(localLockName).unlock();
    }
  }

  public List<InternalAlert> getAlerts() {
    lockFactory.getLock(localLockName).lock();
    try {
      return alerts;
    } finally {
      lockFactory.getLock(localLockName).unlock();
    }
  }

  private boolean isPreviousValueRequired() {
    lockFactory.getLock(localLockName).lock();
    try {
      boolean isRequired = false;
      if (!CollectionUtils.isEmpty(alerts)) {
        isRequired = !alerts.stream()
            .filter(alert -> alert.getTrigger().equals(AlertTriggerType.CHANGE) || alert.getTrigger().equals(AlertTriggerType.CHANGE_DELTA))
            .collect(Collectors.toList()).isEmpty();
      }

      return isRequired;
    } finally {
      lockFactory.getLock(localLockName).unlock();
    }
  }

  /**
   * Verifies if the value parameter checks the restriction rule defined by the alert. Returns true
   * if the value checks the restriction rule (i.e. value must be rejected). Otherwise returns false
   * Moreover, if the value verifies the rule restriction then a new alarm is published
   *
   * @param alert
   * @param value
   * @return true/false
   */
  private boolean valueVerifiesRestriction(final InternalAlert alert, final String value) {
    final TriggerResult result = triggerEvaluator.evaluate(alert, value);
    if (result.triggerConditionChecked()) {
      publishService.publishAlarm(alert, result.getAlarmMessage());
    }

    return result.triggerConditionChecked();
  }

  private void readAndSetPreviousValue(final EventMessage eventMessage) {

    // Get, from API, the previous value published by this sensor, i.e. get the last value with a
    // timestamp lower than current event timestamp

    final ObservationsOutputMessage oom = platformClient.getDataOps()
        .getLastObservations(new DataInputMessage(provider, sensor, new QueryFilterParams(0l, eventMessage.getTime() - 1, 1)));

    if (oom != null && !CollectionUtils.isEmpty(oom.getObservations())) {
      final String previousValue = oom.getObservations().get(0).getValue();
      triggerEvaluator.setPreviousValue(previousValue);
    }
  }
}
