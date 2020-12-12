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
package org.sentilo.agent.alert.test.service;

import static org.mockito.Matchers.any;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.agent.alert.domain.InternalAlert;
import org.sentilo.agent.alert.service.impl.PublishServiceImpl;
import org.sentilo.agent.common.metrics.AgentMetricsCounter;
import org.sentilo.common.enums.AlertTriggerType;
import org.sentilo.common.exception.RESTClientException;
import org.sentilo.platform.client.core.PlatformClientOperations;
import org.sentilo.platform.client.core.domain.AlarmInputMessage;
import org.sentilo.platform.client.core.service.AlarmServiceOperations;

public class PublishServiceImplTest {

  private final String providerId = "providerTest";
  private final String sensorId = "sensorTest";
  private final String alertId = "alertTest";

  @Mock
  private PlatformClientOperations platformClient;

  @Mock
  private InternalAlert alert;

  @Mock
  private AlarmServiceOperations operations;

  @Mock
  private AgentMetricsCounter metricsCounters;

  @InjectMocks
  private PublishServiceImpl publishService;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    when(platformClient.getAlarmOps()).thenReturn(operations);

    when(alert.getProviderId()).thenReturn(providerId);
    when(alert.getSensorId()).thenReturn(sensorId);
    when(alert.getId()).thenReturn(alertId);
    when(alert.getTrigger()).thenReturn(AlertTriggerType.FROZEN);
  }

  @Test
  public void publishAlarm() {
    final String message = "mock message";

    publishService.publishAlarm(alert, message);

    verify(operations).publish(any(AlarmInputMessage.class));
    verify(metricsCounters).isRemoteServerConnectionOk(true);
    verify(metricsCounters).incrementOutputEvents(1);
  }

  @Test(expected = RESTClientException.class)
  public void exception_when_publicsAlarm() {
    doThrow(RESTClientException.class).when(operations).publish(any(AlarmInputMessage.class));

    final String message = "mock message";

    publishService.publishAlarm(alert, message);

    verify(operations).publish(any(AlarmInputMessage.class));
    verify(metricsCounters).isRemoteServerConnectionOk(false);
    verify(metricsCounters, times(0)).incrementOutputEvents(1);
  }

  @Test
  public void publishFrozenAlarm() {
    when(alert.getExpression()).thenReturn("20");

    publishService.publishFrozenAlarm(alert);

    verify(operations).publish(any(AlarmInputMessage.class));
    verify(metricsCounters).isRemoteServerConnectionOk(true);
    verify(metricsCounters).incrementOutputEvents(1);
  }
}
