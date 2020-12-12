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
package org.sentilo.platform.service.test.service;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.converter.DefaultStringMessageConverter;
import org.sentilo.common.converter.StringMessageConverter;
import org.sentilo.common.enums.SensorState;
import org.sentilo.common.enums.SubscribeType;
import org.sentilo.platform.common.domain.AlarmSubscription;
import org.sentilo.platform.common.domain.DataSubscription;
import org.sentilo.platform.common.domain.NotificationParams;
import org.sentilo.platform.common.domain.OrderSubscription;
import org.sentilo.platform.common.domain.Subscription;
import org.sentilo.platform.common.exception.ResourceNotFoundException;
import org.sentilo.platform.common.exception.ResourceOfflineException;
import org.sentilo.platform.common.service.AlarmService;
import org.sentilo.platform.common.service.ResourceService;
import org.sentilo.platform.service.dao.SentiloRedisTemplate;
import org.sentilo.platform.service.impl.SubscribeServiceImpl;
import org.sentilo.platform.service.listener.MessageListenerFactory;
import org.sentilo.platform.service.listener.MessageListenerImpl;
import org.sentilo.platform.service.utils.ChannelUtils;
import org.springframework.data.redis.listener.RedisMessageListenerContainer;
import org.springframework.data.redis.listener.Topic;
import org.springframework.util.CollectionUtils;

public class SubscribeServiceImplTest {

  @Mock
  private SentiloRedisTemplate sRedisTemplate;
  @Mock
  private RedisMessageListenerContainer listenerContainer;
  @Mock
  private DataSubscription dataSubscription;
  @Mock
  private AlarmSubscription alarmSubscription;
  @Mock
  private OrderSubscription orderSubscription;
  @Mock
  private MessageListenerFactory listenerFactory;
  @Mock
  private ResourceService resourceService;
  @Mock
  private AlarmService alarmService;

  @InjectMocks
  private SubscribeServiceImpl service;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    when(listenerFactory.getObject()).thenReturn(new MessageListenerImpl("mockName"));
  }

  @Test
  public void dataSubscribe() {
    final String provider = "prov1";
    final String sensor = "sensor1";
    when(dataSubscription.getType()).thenReturn(SubscribeType.DATA);
    when(dataSubscription.getSensorId()).thenReturn(sensor);
    when(dataSubscription.getProviderId()).thenReturn(provider);
    when(resourceService.getSensorState(eq(provider), eq(sensor))).thenReturn(SensorState.online);

    verifySubscribe(dataSubscription);
  }

  @Test(expected = ResourceNotFoundException.class)
  public void subscribeToUnknowSensor() {
    final String provider = "prov1";
    final String sensor = "sensor1";
    when(dataSubscription.getType()).thenReturn(SubscribeType.DATA);
    when(dataSubscription.getSensorId()).thenReturn(sensor);
    when(dataSubscription.getProviderId()).thenReturn(provider);
    when(resourceService.getSensorState(eq(provider), eq(sensor))).thenReturn(null);

    verifySubscribe(dataSubscription);
  }

  @Test(expected = ResourceOfflineException.class)
  public void subscribeToDisabledSensor() {
    final String provider = "prov1";
    final String sensor = "sensor1";
    when(dataSubscription.getType()).thenReturn(SubscribeType.DATA);
    when(dataSubscription.getSensorId()).thenReturn(sensor);
    when(dataSubscription.getProviderId()).thenReturn(provider);
    when(resourceService.getSensorState(eq(provider), eq(sensor))).thenReturn(SensorState.offline);

    verifySubscribe(dataSubscription);
  }

  @Test
  public void alarmSubscribe() {
    final String alertId = "alert1";
    when(alarmSubscription.getType()).thenReturn(SubscribeType.ALARM);
    when(alarmSubscription.getAlertId()).thenReturn(alertId);
    when(alarmService.getAlertOwner(eq(alertId))).thenReturn("mockOwner");
    when(resourceService.existsAlert(alertId)).thenReturn(true);
    when(resourceService.isAlertDisabled(alertId)).thenReturn(false);

    verifySubscribe(alarmSubscription);
  }

  @Test(expected = ResourceNotFoundException.class)
  public void subscribeToUnknowAlert() {
    final String alertId = "alert1";
    when(alarmSubscription.getType()).thenReturn(SubscribeType.ALARM);
    when(alarmSubscription.getAlertId()).thenReturn(alertId);
    when(alarmService.getAlertOwner(eq(alertId))).thenReturn("mockOwner");
    when(resourceService.existsAlert(alertId)).thenReturn(false);

    verifySubscribe(alarmSubscription);
  }

  @Test(expected = ResourceOfflineException.class)
  public void subscribeToDisabledAlert() {
    final String alertId = "alert1";
    when(alarmSubscription.getType()).thenReturn(SubscribeType.ALARM);
    when(alarmSubscription.getAlertId()).thenReturn(alertId);
    when(alarmService.getAlertOwner(eq(alertId))).thenReturn("mockOwner");
    when(resourceService.existsAlert(alertId)).thenReturn(true);
    when(resourceService.isAlertDisabled(alertId)).thenReturn(true);

    verifySubscribe(alarmSubscription);
  }

  @Test
  public void orderSubscribe() {
    when(orderSubscription.getType()).thenReturn(SubscribeType.ORDER);

    verifySubscribe(orderSubscription);
  }

  @Test
  public void sensorOrderSubscribe() {
    final String provider = "prov1";
    final String sensor = "sensor1";

    when(orderSubscription.getType()).thenReturn(SubscribeType.ORDER);
    when(orderSubscription.getSensorId()).thenReturn(sensor);
    when(orderSubscription.getOwnerEntityId()).thenReturn(provider);
    when(resourceService.getSensorState(eq(provider), eq(sensor))).thenReturn(SensorState.online);

    verifySubscribe(orderSubscription);
  }

  @Test(expected = ResourceNotFoundException.class)
  public void rejectOrderSubscription() {
    final String provider = "prov1";
    final String sensor = "sensor1";

    when(orderSubscription.getType()).thenReturn(SubscribeType.ORDER);
    when(orderSubscription.getSensorId()).thenReturn(sensor);
    when(orderSubscription.getOwnerEntityId()).thenReturn(provider);

    verifySubscribe(orderSubscription);
  }

  @Test(expected = ResourceNotFoundException.class)
  public void rejectAlarmSubscription() {
    final String alertId = "alert1";
    when(alarmSubscription.getType()).thenReturn(SubscribeType.ALARM);
    when(alarmSubscription.getAlertId()).thenReturn(alertId);
    when(alarmService.getAlertOwner(eq(alertId))).thenReturn(null);

    verifySubscribe(alarmSubscription);
  }

  @Test
  public void removeSubscriptionWithType() {
    initSubscription(dataSubscription);
    when(dataSubscription.getType()).thenReturn(SubscribeType.DATA);
    when(dataSubscription.hasResourceIdentified()).thenReturn(true);

    final Topic topic = ChannelUtils.getChannel(dataSubscription);
    final String key = service.getKeysBuilder().getSubscriptionKey(dataSubscription.getSourceEntityId());

    service.remove(dataSubscription);

    verify(sRedisTemplate).hDel(key, topic.getTopic());

  }

  @Test
  public void removeSubscriptionOfOneType() {
    initSubscription(dataSubscription);
    when(dataSubscription.getType()).thenReturn(SubscribeType.DATA);
    when(dataSubscription.hasResourceIdentified()).thenReturn(false);

    final String key = service.getKeysBuilder().getSubscriptionKey(dataSubscription.getSourceEntityId());
    final Set<String> topics = getTopics();
    final List<String> topicsToRemove = ChannelUtils.filterTopicsOfType(topics, dataSubscription.getType());

    when(sRedisTemplate.hKeys(key)).thenReturn(topics);

    service.remove(dataSubscription);

    verify(sRedisTemplate).hKeys(key);
    verify(sRedisTemplate).hDel(key, topicsToRemove.toArray(new String[0]));

  }

  @Test
  public void removeSubscriptionOfOneTypeWithoutSubscriptions() {
    initSubscription(dataSubscription);
    when(dataSubscription.getType()).thenReturn(SubscribeType.DATA);
    when(dataSubscription.hasResourceIdentified()).thenReturn(false);

    final String key = service.getKeysBuilder().getSubscriptionKey(dataSubscription.getSourceEntityId());
    final Set<String> topics = getEmptyTopics();
    final List<String> topicsToRemove = ChannelUtils.filterTopicsOfType(topics, dataSubscription.getType());

    when(sRedisTemplate.hKeys(key)).thenReturn(topics);

    service.remove(dataSubscription);

    verify(sRedisTemplate).hKeys(key);
    verify(sRedisTemplate, times(0)).hDel(key, topicsToRemove.toArray(new String[0]));

  }

  @Test
  public void removeSubscriptionWithoutType() {
    initSubscription(dataSubscription);
    when(dataSubscription.getType()).thenReturn(null);
    when(dataSubscription.hasResourceIdentified()).thenReturn(true);

    final String key = service.getKeysBuilder().getSubscriptionKey(dataSubscription.getSourceEntityId());

    service.remove(dataSubscription);

    verify(sRedisTemplate).del(key);

  }

  @Test
  public void getSubscriptions() {
    initSubscription(dataSubscription);
    final String key = service.getKeysBuilder().getSubscriptionKey(dataSubscription.getSourceEntityId());
    when(dataSubscription.getType()).thenReturn(SubscribeType.DATA);
    when(sRedisTemplate.hGetAll(key)).thenReturn(buildSubscriptions());

    // Retrieve all subscriptions that belongs to entity dataSubscription's source and type DATA
    final List<Subscription> subscriptions = service.get(dataSubscription);

    verify(sRedisTemplate).hGetAll(key);
    assertTrue(subscriptions.size() == buildSubscriptions().size());
    for (final Subscription subscription : subscriptions) {
      assertEquals("http://127.0.0.1/endpoint", subscription.getNotificationParams().getEndpoint());
    }

  }

  @Test
  public void getSubscriptionsWithoutSubscriptions() {
    initSubscription(dataSubscription);
    when(dataSubscription.getType()).thenReturn(SubscribeType.DATA);

    final String key = service.getKeysBuilder().getSubscriptionKey(dataSubscription.getSourceEntityId());
    final List<Subscription> subscriptions = service.get(dataSubscription);

    verify(sRedisTemplate).hGetAll(key);
    assertTrue(CollectionUtils.isEmpty(subscriptions));

  }

  private void initSubscription(final Subscription subscription) {
    final NotificationParams notificationParams = new NotificationParams("http://127.0.0.1/endpoint", "ABCDEFGH", 3, 5);
    when(subscription.getSourceEntityId()).thenReturn("prov1");
    when(subscription.getNotificationParams()).thenReturn(notificationParams);

  }

  private void verifySubscribe(final Subscription subscription) {
    final StringMessageConverter converter = new DefaultStringMessageConverter();
    initSubscription(subscription);
    final Topic topic = ChannelUtils.getChannel(subscription);
    final String key = service.getKeysBuilder().getSubscriptionKey(subscription.getSourceEntityId());

    service.subscribe(subscription);

    verify(sRedisTemplate).hSet(key, topic.getTopic(), converter.marshal(subscription.getNotificationParams()));
  }

  private Set<String> getTopics() {
    final Set<String> topics = new HashSet<String>();
    topics.add("/data/provider1/sensor1");
    topics.add("/data/provider2*");
    return topics;
  }

  private Set<String> getEmptyTopics() {
    return new HashSet<String>();
  }

  private Map<String, String> buildSubscriptions() {
    final Map<String, String> subscriptions = new HashMap<String, String>();
    subscriptions.put("/data/provider1/sensor1", "{\"endpoint\":\"http://127.0.0.1/endpoint\", \"secretCallbackKey\":\"ABCDEFGH\"}");
    subscriptions.put("/data/provider2*", "{\"endpoint\":\"http://127.0.0.1/endpoint\"}");
    return subscriptions;
  }
}
