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
package org.sentilo.agent.alert.listener;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import org.sentilo.agent.alert.domain.InternalAlert;
import org.sentilo.agent.alert.repository.FrozenRepository;
import org.sentilo.agent.alert.service.PublishService;
import org.sentilo.agent.alert.trigger.TriggerEvaluator;
import org.sentilo.agent.alert.trigger.TriggerResult;
import org.sentilo.agent.common.listener.AbstractMessageListenerImpl;
import org.sentilo.common.domain.EventMessage;
import org.sentilo.common.enums.AlertTriggerType;
import org.springframework.util.Assert;
import org.springframework.util.CollectionUtils;

public class MessageListenerImpl extends AbstractMessageListenerImpl {

  private final Lock lock = new ReentrantLock();

  private final TriggerEvaluator triggerEvaluator;
  private PublishService publishService;
  private FrozenRepository frozenRepository;

  private List<InternalAlert> alerts;

  public MessageListenerImpl(final String name) {
    super(name);
    triggerEvaluator = new TriggerEvaluator();
  }

  public MessageListenerImpl(final String name, final PublishService publishService, final FrozenRepository frozenRepository) {
    this(name);
    Assert.notNull(publishService, "publishService must not be NULL");
    this.publishService = publishService;
    this.frozenRepository = frozenRepository;

  }

  public void doWithMessage(final EventMessage eventMessage) {
    final String value = eventMessage.getMessage();

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

    // Set value as previousValue in triggerEvaluator for the next iterator
    triggerEvaluator.setPreviousValue(value);

    // Finally, updates the timeout for each frozen alert associated with this listener
    if (!CollectionUtils.isEmpty(frozenAlerts)) {
      frozenRepository.updateFrozenTimeouts(frozenAlerts);
    }
  }

  public void updateAlerts(final List<InternalAlert> newAlerts) {
    lock.lock();
    try {
      alerts = newAlerts;
    } finally {
      lock.unlock();
    }
  }

  public List<InternalAlert> getAlerts() {
    lock.lock();
    try {
      return alerts;
    } finally {
      lock.unlock();
    }
  }

  public void addAlert(final InternalAlert alert) {
    lock.lock();
    try {
      if (alerts == null) {
        alerts = new ArrayList<InternalAlert>();
      }
      alerts.add(alert);
    } finally {
      lock.unlock();
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

}
