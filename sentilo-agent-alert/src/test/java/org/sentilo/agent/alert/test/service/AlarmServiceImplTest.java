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
package org.sentilo.agent.alert.test.service;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Matchers.isA;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.agent.alert.domain.Alarm;
import org.sentilo.agent.alert.listener.MessageListenerImpl;
import org.sentilo.agent.alert.listener.MockMessageListenerImpl;
import org.sentilo.agent.alert.service.impl.AlarmServiceImpl;
import org.springframework.data.mongodb.core.MongoOperations;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.listener.ChannelTopic;
import org.springframework.data.redis.listener.RedisMessageListenerContainer;
import org.springframework.util.ReflectionUtils;

public class AlarmServiceImplTest {

  private AlarmServiceImpl alarmService;

  @Mock
  private MongoOperations mongoOps;
  @Mock
  private RedisMessageListenerContainer listenerContainer;
  @Mock
  private RedisTemplate<String, String> redisTemplate;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    alarmService = new AlarmServiceImpl();

    alarmService.setListenerContainer(listenerContainer);
    alarmService.setRedisTemplate(redisTemplate);
    alarmService.setMongoOps(mongoOps);
  }

  @SuppressWarnings("unchecked")
  @Test
  public void subscribeMockMessageListener() {
    when(mongoOps.find(any(Query.class), eq(Alarm.class))).thenReturn(Collections.EMPTY_LIST);

    alarmService.loadAndSubscribeToInternalAlarms();

    verify(mongoOps).find(any(Query.class), eq(Alarm.class));
    verify(listenerContainer).addMessageListener(isA(MockMessageListenerImpl.class), any(ChannelTopic.class));
  }

  @Test
  public void subscribeListener() throws Exception {
    final List<Alarm> alarms = getInternalAlarms();
    when(mongoOps.find(any(Query.class), eq(Alarm.class))).thenReturn(alarms);

    alarmService.loadAndSubscribeToInternalAlarms();

    verify(mongoOps).find(any(Query.class), eq(Alarm.class));
    verify(listenerContainer, times(1)).addMessageListener(isA(MessageListenerImpl.class), any(ChannelTopic.class));

  }

  @SuppressWarnings("unchecked")
  @Test
  public void subscribeDifferentListeners() throws Exception {
    final Field field = AlarmServiceImpl.class.getDeclaredField("currentListeners");
    ReflectionUtils.makeAccessible(field);
    final List<Alarm> alarms = getDifferentSensorsAlarms();
    when(mongoOps.find(any(Query.class), eq(Alarm.class))).thenReturn(alarms);

    alarmService.loadAndSubscribeToInternalAlarms();

    verify(mongoOps).find(any(Query.class), eq(Alarm.class));
    verify(listenerContainer, times(2)).addMessageListener(isA(MessageListenerImpl.class), any(ChannelTopic.class));
    Assert.assertTrue(((Map<String, MessageListenerImpl>) field.get(alarmService)).size() == 2);

  }

  @SuppressWarnings("unchecked")
  @Test
  public void updateAndRegisterListeners() throws Exception {
    final Field field = AlarmServiceImpl.class.getDeclaredField("currentListeners");
    ReflectionUtils.makeAccessible(field);
    final Map<String, MessageListenerImpl> listeners = new HashMap<String, MessageListenerImpl>();
    listeners.put("topic1", new MessageListenerImpl("topic1"));
    listeners.put("topic2", new MessageListenerImpl("topic2"));

    final Map<String, MessageListenerImpl> listeners2 = new HashMap<String, MessageListenerImpl>();
    listeners2.put("topic2", new MessageListenerImpl("topic2"));

    alarmService.updateAndRegisterListeners(listeners);
    Assert.assertTrue(((Map<String, MessageListenerImpl>) field.get(alarmService)).size() == listeners.size());

    alarmService.updateAndRegisterListeners(listeners2);
    Assert.assertTrue(((Map<String, MessageListenerImpl>) field.get(alarmService)).size() == listeners2.size());
  }

  private List<Alarm> getInternalAlarms() {
    final List<Alarm> alarms = new ArrayList<Alarm>();
    alarms.add(new Alarm("1"));
    alarms.add(new Alarm("2"));
    return alarms;
  }

  private List<Alarm> getDifferentSensorsAlarms() {
    final List<Alarm> alarms = new ArrayList<Alarm>();
    final Alarm alarm1 = new Alarm("1");
    alarm1.setProviderId("prov1");
    alarm1.setProviderId("sensor1");

    final Alarm alarm2 = new Alarm("1");
    alarm2.setProviderId("prov1");
    alarm2.setProviderId("sensor2");

    alarms.add(alarm1);
    alarms.add(alarm2);
    return alarms;
  }

}
