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

import org.sentilo.agent.alert.domain.InternalAlert;
import org.sentilo.agent.alert.service.PublishService;
import org.sentilo.agent.alert.utils.Constants;
import org.sentilo.agent.common.metrics.AgentMetricsCounter;
import org.sentilo.common.exception.RESTClientException;
import org.sentilo.platform.client.core.PlatformClientOperations;
import org.sentilo.platform.client.core.domain.AlarmInputMessage;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class PublishServiceImpl implements PublishService {

  private static final Logger LOGGER = LoggerFactory.getLogger(PublishServiceImpl.class);

  @Autowired
  private PlatformClientOperations platformClient;

  @Autowired
  private AgentMetricsCounter metricsCounters;

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.agent.alert.service.PublishService#publishFrozenAlarm(org.sentilo.agent.alert.
   * domain .InternalAlert)
   */
  public void publishFrozenAlarm(final InternalAlert alert) {
    final String errorMessage = String.format(Constants.TEMPLATE_FROZEN_ALARM, alert.getId(), alert.getSensorId(), alert.getExpression());
    publishAlarm(alert, errorMessage);
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.agent.alert.service.PublishService#publishAlarm(org.sentilo.agent.alert.domain.
   * InternalAlert, java.lang.String)
   */
  public void publishAlarm(final InternalAlert alert, final String message) {
    LOGGER.debug("Publishing new alarm, related to (alert, sensor, provider) ({}, {} , {}), with message:  {}  ", alert.getId(), alert.getSensorId(),
        alert.getProviderId(), message);

    // Publish directly via API REST
    final AlarmInputMessage alarmMessage = new AlarmInputMessage(alert.getId(), message);
    alarmMessage.setProviderId(alert.getProviderId());
    alarmMessage.setSensorId(alert.getSensorId());
    alarmMessage.setAlertType(alert.getTrigger().name());

    try {
      platformClient.getAlarmOps().publish(alarmMessage);
      metricsCounters.isRemoteServerConnectionOk(true);
      metricsCounters.incrementOutputEvents(1);
    } catch (final RESTClientException rce) {
      metricsCounters.isRemoteServerConnectionOk(false);
      throw rce;
    }
  }
}
