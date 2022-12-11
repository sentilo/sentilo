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
package org.sentilo.agent.alert.test.listener;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.concurrent.locks.Lock;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.sentilo.agent.alert.domain.InternalAlert;
import org.sentilo.agent.alert.listener.DefaultSensorAlertsRuleEngineImpl;
import org.sentilo.agent.alert.repository.FrozenRepository;
import org.sentilo.agent.alert.service.PublishService;
import org.sentilo.common.domain.EventMessage;
import org.sentilo.common.enums.AlertTriggerType;
import org.sentilo.common.lock.LockFactory;
import org.sentilo.platform.client.core.PlatformClientOperations;
import org.sentilo.platform.client.core.domain.DataInputMessage;
import org.sentilo.platform.client.core.domain.Observation;
import org.sentilo.platform.client.core.domain.ObservationsOutputMessage;
import org.sentilo.platform.client.core.service.DataServiceOperations;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.test.util.ReflectionTestUtils;

public class SensorAlertsRuleEngineTest {

  @InjectMocks
  private DefaultSensorAlertsRuleEngineImpl ruleEngine = new DefaultSensorAlertsRuleEngineImpl("/data/mockProvider/mockSensor");

  @Mock
  private LockFactory lockFactory;
  @Mock
  private PublishService publishService;
  @Mock
  private FrozenRepository frozenRepository;
  @Mock
  private StringRedisTemplate redisTemplate;
  @Mock
  private EventMessage eventMessage;
  @Mock
  private Lock lock;
  @Mock
  private PlatformClientOperations platformClient;
  @Mock
  private DataServiceOperations dataOps;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    when(lockFactory.getLock(anyString())).thenReturn(lock);
    when(platformClient.getDataOps()).thenReturn(dataOps);
    ruleEngine.init();

    final InternalAlert alert1 = new InternalAlert("alertGT");
    alert1.setTrigger(AlertTriggerType.GT);
    alert1.setExpression("20");

    final InternalAlert alert2 = new InternalAlert("alertDELTA");
    alert2.setTrigger(AlertTriggerType.CHANGE_DELTA);
    // If percent change is greater than 25%, an alarm should be published
    alert2.setExpression("25");

    final InternalAlert alert3 = new InternalAlert("alertCHANGE");
    alert3.setTrigger(AlertTriggerType.CHANGE);

    ruleEngine.addAlert(alert1);
    ruleEngine.addAlert(alert2);
    ruleEngine.addAlert(alert3);

  }

  @Test
  public void init() {
    Assert.assertEquals("mockProvider", ReflectionTestUtils.getField(ruleEngine, "provider"));
    Assert.assertEquals("mockSensor", ReflectionTestUtils.getField(ruleEngine, "sensor"));
  }

  @Test
  public void publishAlarm() {
    when(eventMessage.getMessage()).thenReturn("36");

    ruleEngine.process(eventMessage);

    verify(publishService).publishAlarm(any(InternalAlert.class), any(String.class));
  }

  @Test
  public void publishTwoAlarms() {
    when(eventMessage.getMessage()).thenReturn("36");

    final InternalAlert alert4 = new InternalAlert("alertLT");
    alert4.setTrigger(AlertTriggerType.LT);
    alert4.setExpression("40");

    ruleEngine.addAlert(alert4);

    ruleEngine.process(eventMessage);

    verify(publishService, times(2)).publishAlarm(any(InternalAlert.class), any(String.class));
  }

  @Test
  public void publishTwiceChangeAlarms() {
    final Observation obs = Mockito.mock(Observation.class);
    final ObservationsOutputMessage oom = Mockito.mock(ObservationsOutputMessage.class);
    when(eventMessage.getMessage()).thenReturn("36", "16");
    when(dataOps.getLastObservations(any(DataInputMessage.class))).thenReturn(oom);
    when(oom.getObservations()).thenReturn(Collections.singletonList(obs));
    when(obs.getValue()).thenReturn("35", "36");

    // Process two messages with different values
    ruleEngine.process(eventMessage);
    ruleEngine.process(eventMessage);

    // 2 alarms will be published for value 36: one alarm of type GT and one of type CHANGE.
    // 2 alarms will be published for value 16: one of type CHANGE and one of type CHANGE_DELTA
    verify(publishService, times(4)).publishAlarm(any(InternalAlert.class), any(String.class));
  }
}
