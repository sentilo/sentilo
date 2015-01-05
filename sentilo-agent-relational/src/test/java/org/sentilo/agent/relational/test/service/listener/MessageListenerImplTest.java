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
package org.sentilo.agent.relational.test.service.listener;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.agent.common.utils.Constants;
import org.sentilo.agent.relational.business.service.DataTrackService;
import org.sentilo.agent.relational.common.domain.Alarm;
import org.sentilo.agent.relational.common.domain.Observation;
import org.sentilo.agent.relational.common.domain.Order;
import org.sentilo.agent.relational.listener.MessageListenerImpl;
import org.sentilo.common.domain.EventMessage;
import org.sentilo.common.domain.SubscribeType;
import org.sentilo.common.parser.EventMessageConverter;
import org.springframework.data.redis.connection.Message;
import org.springframework.data.redis.serializer.RedisSerializer;
import org.springframework.data.redis.serializer.StringRedisSerializer;

public class MessageListenerImplTest {

  final String dsName = "provider1Ds";
  @Mock
  private DataTrackService dataTrackService;
  @Mock
  private Message message;

  private ArgumentCaptor<Order> order;
  private ArgumentCaptor<Observation> observation;
  private ArgumentCaptor<Alarm> alarm;

  private MessageListenerImpl listener;
  private RedisSerializer<String> serializer;
  private EventMessageConverter eventConverter;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    serializer = new StringRedisSerializer();
    eventConverter = new EventMessageConverter();
    order = ArgumentCaptor.forClass(Order.class);
    observation = ArgumentCaptor.forClass(Observation.class);
    alarm = ArgumentCaptor.forClass(Alarm.class);
  }

  @Test
  public void onDataMessage() {
    listener = new MessageListenerImpl(dsName, dataTrackService);

    final String channel = "/data/provider1/sensor1";
    final String value = eventConverter.marshall(buildDataEventMessage());
    final String pattern = Constants.DATA;

    when(message.getChannel()).thenReturn(serializer.serialize(channel));
    when(message.getBody()).thenReturn(serializer.serialize(value));

    listener.onMessage(message, serializer.serialize(pattern));

    verify(dataTrackService).save(observation.capture());

    assertEquals("sensor1", observation.getValue().getSensor());
    assertEquals("provider1", observation.getValue().getProvider());
    assertEquals("43", observation.getValue().getValue());

  }

  @Test
  public void onOrderMessage() {
    listener = new MessageListenerImpl(dsName, dataTrackService);

    final String channel = "/order/provider1/sensor1";
    final String value = eventConverter.marshall(buildOrderEventMessage());
    final String pattern = Constants.ORDER;

    when(message.getChannel()).thenReturn(serializer.serialize(channel));
    when(message.getBody()).thenReturn(serializer.serialize(value));

    listener.onMessage(message, serializer.serialize(pattern));

    verify(dataTrackService).save(order.capture());

    assertEquals("sensor1", order.getValue().getSensor());
    assertEquals("provider1", order.getValue().getProvider());
    assertEquals("test order message", order.getValue().getMessage());
  }

  @Test
  public void onAlarmMessage() {
    listener = new MessageListenerImpl(dsName, dataTrackService);

    final String channel = "/alarm/alarm1";
    final String value = eventConverter.marshall(buildAlarmEventMessage());
    final String pattern = Constants.ALARM;

    when(message.getChannel()).thenReturn(serializer.serialize(channel));
    when(message.getBody()).thenReturn(serializer.serialize(value));

    listener.onMessage(message, serializer.serialize(pattern));

    verify(dataTrackService).save(alarm.capture());

    assertEquals("alarm1", alarm.getValue().getAlarm());
    assertEquals("test alarm message", alarm.getValue().getMessage());

  }

  private EventMessage buildDataEventMessage() {
    final EventMessage event = new EventMessage();
    event.setProvider("provider1");
    event.setSensor("sensor1");
    event.setMessage("43");
    event.setTimestamp("09/09/2013T15:55:17");
    event.setType(SubscribeType.DATA.name());
    event.setTopic("/data/provider1/sensor1");

    return event;
  }

  private EventMessage buildOrderEventMessage() {
    final EventMessage event = new EventMessage();
    event.setProvider("provider1");
    event.setSensor("sensor1");
    event.setMessage("test order message");
    event.setTimestamp("09/09/2013T15:55:17");
    event.setSender("provider1");
    event.setType(SubscribeType.ORDER.name());
    event.setTopic("/order/provider1/sensor1");

    return event;
  }

  private EventMessage buildAlarmEventMessage() {
    final EventMessage event = new EventMessage();
    event.setAlert("alarm1");
    event.setMessage("test alarm message");
    event.setTimestamp("09/09/2013T15:55:17");
    event.setSender("app demo");
    event.setType(SubscribeType.ALARM.name());
    event.setTopic("/alarm/alarm1");

    return event;
  }

}
