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
import static org.mockito.Matchers.anyCollectionOf;
import static org.mockito.Matchers.eq;
import static org.mockito.Matchers.isA;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Matchers;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.agent.alert.domain.InternalAlert;
import org.sentilo.agent.alert.event.CheckFrozenAlertEvent;
import org.sentilo.agent.alert.listener.MessageListenerImpl;
import org.sentilo.agent.alert.repository.FrozenRepository;
import org.sentilo.agent.alert.service.PublishService;
import org.sentilo.agent.alert.service.impl.AlertServiceImpl;
import org.sentilo.agent.common.listener.MockMessageListenerImpl;
import org.sentilo.common.enums.AlertTriggerType;
import org.springframework.data.mongodb.core.MongoOperations;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.data.redis.connection.MessageListener;
import org.springframework.data.redis.listener.ChannelTopic;
import org.springframework.data.redis.listener.RedisMessageListenerContainer;
import org.springframework.test.util.ReflectionTestUtils;

public class AlertServiceImplTest {

  @InjectMocks
  private AlertServiceImpl alertService;

  @Mock
  private MongoOperations mongoOps;
  @Mock
  private RedisMessageListenerContainer listenerContainer;
  @Mock
  private PublishService publishService;
  @Mock
  private FrozenRepository repository;
  @Mock
  private CheckFrozenAlertEvent frozenEvent;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
  }

  @SuppressWarnings("unchecked")
  @Test
  public void subscribeMockMessageListener() {
    when(mongoOps.find(any(Query.class), eq(InternalAlert.class), any(String.class))).thenReturn(Collections.EMPTY_LIST);

    alertService.loadAndMonitorInternalAlerts();

    verify(mongoOps).find(any(Query.class), eq(InternalAlert.class), any(String.class));
    verify(repository, times(0)).synchronizeAlerts(anyCollectionOf(InternalAlert.class));
    verify(listenerContainer).addMessageListener(isA(MockMessageListenerImpl.class), any(ChannelTopic.class));
  }

  @Test
  public void loadAndMonitorInternalAlerts() throws Exception {
    final List<InternalAlert> alerts = getInternalAlerts();
    when(mongoOps.find(any(Query.class), eq(InternalAlert.class), any(String.class))).thenReturn(alerts);

    alertService.loadAndMonitorInternalAlerts();

    verify(mongoOps).find(any(Query.class), eq(InternalAlert.class), any(String.class));
    verify(repository, times(0)).synchronizeAlerts(Matchers.anyCollectionOf(InternalAlert.class));
    verify(listenerContainer, times(alerts.size())).addMessageListener(isA(MessageListenerImpl.class), any(ChannelTopic.class));
  }

  @SuppressWarnings("unchecked")
  @Test
  public void loadAndMonitorInternalFrozenAlerts() throws Exception {
    final List<InternalAlert> alerts = getFrozenAlerts();
    when(mongoOps.find(any(Query.class), eq(InternalAlert.class), any(String.class))).thenReturn(alerts);

    alertService.loadAndMonitorInternalAlerts();

    final Object currentListeners = ReflectionTestUtils.getField(alertService, "currentListeners");

    verify(mongoOps).find(any(Query.class), eq(InternalAlert.class), any(String.class));
    verify(listenerContainer, times(2)).addMessageListener(isA(MessageListener.class), any(ChannelTopic.class));
    verify(repository).synchronizeAlerts(Matchers.anyCollectionOf(InternalAlert.class));
    Assert.assertTrue(((Map<String, MessageListenerImpl>) currentListeners).size() == 2);

  }

  @SuppressWarnings("unchecked")
  @Test
  public void updateAndRegisterListeners() throws Exception {
    final Map<String, MessageListenerImpl> listeners = new HashMap<String, MessageListenerImpl>();
    listeners.put("topic1", new MessageListenerImpl("topic1"));
    listeners.put("topic2", new MessageListenerImpl("topic2"));

    final Map<String, MessageListenerImpl> listeners2 = new HashMap<String, MessageListenerImpl>();
    listeners2.put("topic2", new MessageListenerImpl("topic2"));

    alertService.updateAndRegisterListeners(listeners);
    Object currentListeners = ReflectionTestUtils.getField(alertService, "currentListeners");
    Assert.assertTrue(((Map<String, MessageListenerImpl>) currentListeners).size() == listeners.size());

    alertService.updateAndRegisterListeners(listeners2);
    currentListeners = ReflectionTestUtils.getField(alertService, "currentListeners");
    Assert.assertTrue(((Map<String, MessageListenerImpl>) currentListeners).size() == listeners2.size());
  }

  @Test
  public void onApplicationEvent() {
    final List<InternalAlert> alerts = getFrozenAlerts();
    when(mongoOps.find(any(Query.class), eq(InternalAlert.class), any(String.class))).thenReturn(alerts);
    when(repository.checkFrozenAlerts()).thenReturn(alerts);

    alertService.loadAndMonitorInternalAlerts();
    alertService.onApplicationEvent(frozenEvent);

    verify(publishService).publishFrozenAlarm(any(InternalAlert.class));
    verify(repository).updateFrozenTimeouts(anyCollectionOf(InternalAlert.class));
  }

  private List<InternalAlert> getInternalAlerts() {
    final List<InternalAlert> alerts = new ArrayList<InternalAlert>();
    final InternalAlert alert1 = new InternalAlert("1");
    alert1.setProviderId("prov1");
    alert1.setSensorId("sensor1");
    alert1.setTrigger(AlertTriggerType.GT);

    final InternalAlert alert2 = new InternalAlert("2");
    alert2.setProviderId("prov2");
    alert2.setSensorId("sensor2");
    alert2.setTrigger(AlertTriggerType.LTE);

    alerts.add(alert1);
    alerts.add(alert2);
    return alerts;
  }

  private List<InternalAlert> getFrozenAlerts() {
    final List<InternalAlert> alerts = getInternalAlerts();
    alerts.get(0).setTrigger(AlertTriggerType.FROZEN);
    alerts.get(0).setExpression("20");

    return alerts;
  }

}
