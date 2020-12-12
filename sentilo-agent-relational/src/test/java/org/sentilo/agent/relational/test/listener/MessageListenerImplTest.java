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
package org.sentilo.agent.relational.test.listener;

import static org.junit.Assert.assertEquals;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import org.sentilo.agent.common.metrics.AgentMetricsCounter;
import org.sentilo.agent.common.utils.Constants;
import org.sentilo.agent.relational.domain.Alarm;
import org.sentilo.agent.relational.domain.Observation;
import org.sentilo.agent.relational.domain.Order;
import org.sentilo.agent.relational.listener.MessageListenerImpl;
import org.sentilo.agent.relational.service.DataTrackService;
import org.sentilo.common.converter.DefaultStringMessageConverter;
import org.sentilo.common.converter.StringMessageConverter;
import org.sentilo.common.domain.EventMessage;
import org.sentilo.common.enums.SubscribeType;
import org.sentilo.common.test.AbstractBaseTest;
import org.sentilo.common.utils.DateUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.dao.DataAccessException;
import org.springframework.data.redis.connection.Message;
import org.springframework.data.redis.serializer.RedisSerializer;
import org.springframework.data.redis.serializer.StringRedisSerializer;
import org.springframework.test.util.ReflectionTestUtils;

@RunWith(PowerMockRunner.class)
@PrepareForTest({MessageListenerImpl.class, LoggerFactory.class, ReflectionTestUtils.class})
public class MessageListenerImplTest extends AbstractBaseTest {

  final String dsName = "provider1Ds";
  @Mock
  private DataTrackService dataTrackService;
  @Mock
  private Message message;
  @Mock
  private AgentMetricsCounter metricsCounters;

  private ArgumentCaptor<Order> order;
  private ArgumentCaptor<Observation> observation;
  private ArgumentCaptor<Alarm> alarm;

  private MessageListenerImpl listener;
  private RedisSerializer<String> serializer;
  private StringMessageConverter eventConverter;

  private static Logger logger;

  @BeforeClass
  public static void setUpStatic() throws Exception {
    PowerMockito.mockStatic(LoggerFactory.class);
    logger = PowerMockito.mock(Logger.class);
    when(LoggerFactory.getLogger(anyString())).thenReturn(logger);
    when(LoggerFactory.getLogger(any(Class.class))).thenReturn(logger);
  }

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    serializer = new StringRedisSerializer();
    eventConverter = new DefaultStringMessageConverter();
    order = ArgumentCaptor.forClass(Order.class);
    observation = ArgumentCaptor.forClass(Observation.class);
    alarm = ArgumentCaptor.forClass(Alarm.class);
    listener = new MessageListenerImpl(dsName, dataTrackService);
    ReflectionTestUtils.setField(listener, "metricsCounters", metricsCounters);
  }

  @Test
  public void onDataMessage() {
    final String channel = "/data/provider1/sensor1";
    final String value = eventConverter.marshal(buildDataEventMessage());
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
    final String channel = "/order/provider1/sensor1";
    final String value = eventConverter.marshal(buildOrderEventMessage());
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
    final String channel = "/alarm/alarm1";
    final String value = eventConverter.marshal(buildAlarmEventMessage());
    final String pattern = Constants.ALARM;

    when(message.getChannel()).thenReturn(serializer.serialize(channel));
    when(message.getBody()).thenReturn(serializer.serialize(value));

    listener.onMessage(message, serializer.serialize(pattern));

    verify(dataTrackService).save(alarm.capture());

    assertEquals("alarm1", alarm.getValue().getAlarm());
    assertEquals("test alarm message", alarm.getValue().getMessage());
  }

  @Test
  public void validateEventMessage() {
    final String channel = "/alarm/alarm1";
    final String value = eventConverter.marshal(buildFakeAlarmEventMessage());
    final String pattern = Constants.ALARM;

    when(message.getChannel()).thenReturn(serializer.serialize(channel));
    when(message.getBody()).thenReturn(serializer.serialize(value));

    listener.onMessage(message, serializer.serialize(pattern));

    verify(dataTrackService, times(0)).save(alarm.capture());
    verify(logger).warn(anyString(), anyString(), anyString());
  }

  @Test
  public void dataAccessException() {
    final String channel = "/alarm/alarm1";
    final String value = eventConverter.marshal(buildAlarmEventMessage());
    final String pattern = Constants.ALARM;

    when(message.getChannel()).thenReturn(serializer.serialize(channel));
    when(message.getBody()).thenReturn(serializer.serialize(value));
    doThrow(new DataAccessException("mockMsg") {

      /**
       *
       */
      private static final long serialVersionUID = 1L;
    }).when(dataTrackService).save(alarm.capture());

    listener.onMessage(message, serializer.serialize(pattern));

    verify(dataTrackService).save(alarm.capture());
    verify(logger).error(anyString(), any(Exception.class));
  }

  private EventMessage buildDataEventMessage() {
    final EventMessage event = new EventMessage();
    event.setProvider("provider1");
    event.setSensor("sensor1");
    event.setMessage("43");
    event.setTimestamp("09/09/2013T15:55:17");
    event.setType(SubscribeType.DATA.name());
    event.setTopic("/data/provider1/sensor1");
    event.setTime(DateUtils.parseTimestamp(event.getTimestamp()));
    event.setPublishedAt(System.currentTimeMillis());

    return event;
  }

  private EventMessage buildOrderEventMessage() {
    final EventMessage event = new EventMessage();
    event.setProvider("provider1");
    event.setSensor("sensor1");
    event.setMessage("test order message");
    event.setTimestamp("09/09/2013T15:55:17");
    event.setPublisher("provider1");
    event.setType(SubscribeType.ORDER.name());
    event.setTopic("/order/provider1/sensor1");
    event.setTime(DateUtils.parseTimestamp(event.getTimestamp()));
    event.setPublishedAt(System.currentTimeMillis());

    return event;
  }

  private EventMessage buildAlarmEventMessage() {
    final EventMessage event = new EventMessage();
    event.setAlert("alarm1");
    event.setMessage("test alarm message");
    event.setTimestamp("09/09/2013T15:55:17");
    event.setPublisher("app demo");
    event.setType(SubscribeType.ALARM.name());
    event.setTopic("/alarm/alarm1");
    event.setTime(DateUtils.parseTimestamp(event.getTimestamp()));
    event.setPublishedAt(System.currentTimeMillis());

    return event;
  }

  private EventMessage buildFakeAlarmEventMessage() {
    final EventMessage event = new EventMessage();
    event.setAlert("alarm1");
    event.setTimestamp("09/09/2013T15:55:17");
    event.setPublisher("app demo");
    event.setType(SubscribeType.ALARM.name());
    event.setTopic("/alarm/alarm1");
    event.setTime(DateUtils.parseTimestamp(event.getTimestamp()));
    event.setPublishedAt(System.currentTimeMillis());

    return event;
  }
}
