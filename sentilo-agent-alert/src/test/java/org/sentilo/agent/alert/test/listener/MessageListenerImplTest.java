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
package org.sentilo.agent.alert.test.listener;

import static org.mockito.Matchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.agent.alert.domain.InternalAlert;
import org.sentilo.agent.alert.listener.MessageListenerImpl;
import org.sentilo.agent.alert.service.PublishService;
import org.sentilo.common.utils.AlertTriggerType;
import org.springframework.data.redis.connection.Message;
import org.springframework.data.redis.serializer.RedisSerializer;
import org.springframework.data.redis.serializer.StringRedisSerializer;
import org.springframework.test.util.ReflectionTestUtils;

public class MessageListenerImplTest {

  private MessageListenerImpl messageListener;
  private RedisSerializer<String> serializer;

  @Mock
  private PublishService publishService;
  @Mock
  private Message message;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    serializer = new StringRedisSerializer();
    messageListener = new MessageListenerImpl("test");
    ReflectionTestUtils.setField(messageListener, "publishService", publishService);

    final InternalAlert alert1 = new InternalAlert("alertGT");
    alert1.setTrigger(AlertTriggerType.GT);
    alert1.setExpression("20");

    final InternalAlert alert2 = new InternalAlert("alertDELTA");
    alert2.setTrigger(AlertTriggerType.CHANGE_DELTA);
    alert2.setExpression("40");

    messageListener.addAlert(alert1);
    messageListener.addAlert(alert2);
  }

  @Test
  public void publishAlert() {
    final String info = buildDataEventMessage();
    final String channel = "data:provider1:sensor1";

    when(message.getBody()).thenReturn(serializer.serialize(info));
    when(message.getChannel()).thenReturn(serializer.serialize(channel));

    messageListener.onMessage(message, null);

    verify(publishService).publishAlarm(any(InternalAlert.class), any(String.class));
  }

  @Test
  public void publishTwoAlerts() {
    final String info = buildDataEventMessage();
    final String channel = "data:provider1:sensor1";

    final InternalAlert alert3 = new InternalAlert("alertLT");
    alert3.setTrigger(AlertTriggerType.LT);
    alert3.setExpression("40");

    messageListener.addAlert(alert3);

    when(message.getBody()).thenReturn(serializer.serialize(info));
    when(message.getChannel()).thenReturn(serializer.serialize(channel));

    messageListener.onMessage(message, null);

    verify(publishService, times(2)).publishAlarm(any(InternalAlert.class), any(String.class));
  }

  private String buildDataEventMessage() {
    final String event =
        "{\"message\":\"36\",\"timestamp\":\"28/04/2014T11:37:44\",\"topic\":\"data:app_demo_provider:appdemo_sensor_test\",\"type\":\"DATA\",\"sensor\":\"appdemo_sensor_test\",\"provider\":\"app_demo_provider\"}";

    return event;
  }
}
