/*
 * Sentilo
 *  
 * Original version 1.4 Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de
 * Barcelona. Modified by Opentrends adding support for multitenant deployments and SaaS. 
 * Modifications on version 1.5 Copyright (C) 2015 Opentrends Solucions i Sistemes, S.L.
 * 
 *   
 * This program is licensed and may be used, modified and redistributed under the terms  of the 
 * European Public License (EUPL), either version 1.1 or (at your option) any later version as soon 
 * as they are approved by the European Commission.
 *   
 * Alternatively, you may redistribute and/or modify this program under the terms of the GNU Lesser 
 * General Public License as published by the Free Software Foundation; either  version 3 of the 
 * License, or (at your option) any later version. 
 *   
 * Unless required by applicable law or agreed to in writing, software distributed under the License 
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR  CONDITIONS OF ANY KIND, either express 
 * or implied. 
 *   
 * See the licenses for the specific language governing permissions, limitations and more details.
 *   
 * You should have received a copy of the EUPL1.1 and the LGPLv3 licenses along with this program; 
 * if not, you may find them at: 
 *   
 * https://joinup.ec.europa.eu/software/page/eupl/licence-eupl http://www.gnu.org/licenses/   and 
 * https://www.gnu.org/licenses/lgpl.txt
 */
package org.sentilo.platform.service.test.subscriber;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.concurrent.locks.Lock;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.converter.DefaultStringMessageConverter;
import org.sentilo.common.converter.StringMessageConverter;
import org.sentilo.common.enums.SubscribeType;
import org.sentilo.common.lock.LockFactory;
import org.sentilo.common.utils.MessagingUtils;
import org.sentilo.platform.common.domain.NotificationParams;
import org.sentilo.platform.service.messaging.EventMessageListenerFactory;
import org.sentilo.platform.service.messaging.EventMessageListenerImpl;
import org.sentilo.platform.service.messaging.MessageListenerContainer;
import org.sentilo.platform.service.subscriber.SubscriberRegistry;
import org.sentilo.platform.service.subscriber.SubscriberRepository;
import org.springframework.data.redis.listener.ChannelTopic;
import org.springframework.data.redis.listener.Topic;

public class SubscriberRegistryTest {

  private final StringMessageConverter converter = new DefaultStringMessageConverter();

  @Mock
  private SubscriberRepository repository;
  @Mock
  private MessageListenerContainer listenerContainer;
  @Mock
  private EventMessageListenerFactory listenerFactory;
  @Mock
  private LockFactory lockProvider;
  @Mock
  private Lock lock;
  @Mock
  private NotificationParams notificationParams;
  @Mock
  private EventMessageListenerImpl listener;

  @InjectMocks
  private SubscriberRegistry registry;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    when(lockProvider.getLock(SubscriberRegistry.class.getName() + ".lock", false)).thenReturn(lock);
  }

  @Test
  public void loadSubscriptions() throws Exception {
    final String[] aStoredSubs = {"subs:mockProv1", "subs:mockApp2"};
    final Map<String, NotificationParams> subscriptions = buildSubscriptions();

    when(listenerContainer.isRunning()).thenReturn(Boolean.TRUE);
    when(repository.loadSubscriptions()).thenReturn(new HashSet<String>(Arrays.asList(aStoredSubs)));
    when(repository.loadSubscription("subs:mockProv1")).thenReturn(Collections.emptyMap());
    when(repository.loadSubscription("subs:mockApp2")).thenReturn(subscriptions);
    when(listenerFactory.getObject()).thenReturn(listener);

    registry.loadSubscriptions();

    verify(repository).loadSubscriptions();
    verify(repository).loadSubscription("subs:mockProv1");
    verify(repository).loadSubscription("subs:mockApp2");
    verify(repository).removeAllSubscriptions("subs:mockProv1");
    verify(repository).updateChannelDefinition("mockApp2", "/data/provider3*", "/data/provider3");
    for (final String channel : subscriptions.keySet()) {
      final String formattedChannel = MessagingUtils.formatTopicExpression(channel);
      verify(listenerContainer).addMessageListener(listener, new ChannelTopic(formattedChannel));
      verify(listener).addSubscription(new ChannelTopic(formattedChannel), subscriptions.get(channel));
    }

  }

  @Test
  public void add() throws Exception {
    final String subscriberId = "mockSubscriber";
    final Topic topic = new ChannelTopic("/data/mockProvider/mockSensor");
    when(listenerFactory.getObject()).thenReturn(listener);

    registry.add(subscriberId, topic, notificationParams);

    verify(listenerContainer).addMessageListener(listener, topic);
    verify(listener).addSubscription(topic, notificationParams);
    verify(repository).addSubscription(subscriberId, topic, notificationParams);
  }

  @Test
  public void removeSubscriberSubscriptions() throws Exception {
    final String subscriberId = "mockSubscriber";
    final String subscriptionKey = "subs:" + subscriberId;
    when(listenerFactory.getObject()).thenReturn(listener);
    when(repository.getSubscriptionKey(subscriberId)).thenReturn(subscriptionKey);

    registry.removeSubscriptions(subscriberId);

    verify(repository).getSubscriptionKey(subscriberId);
    verify(repository).removeAllSubscriptions(subscriptionKey);
    verify(listenerContainer).removeMessageListener(listener);
  }

  @Test
  public void removeSubscriptionsOfOneType() throws Exception {
    final String subscriberId = "mockSubscriber";
    final String subscriptionKey = "subs:" + subscriberId;
    final SubscribeType type = SubscribeType.ALARM;
    final Topic[] topicsToRemove = {new ChannelTopic("/alarm/provider2")};
    when(listenerFactory.getObject()).thenReturn(listener);
    when(repository.getSubscriptionKey(subscriberId)).thenReturn(subscriptionKey);
    when(repository.loadSubscription(subscriptionKey)).thenReturn(buildSubscriptions());

    registry.removeSubscriptions(subscriberId, type);

    verify(repository).loadSubscription(subscriptionKey);
    verify(repository).removeSubscriptions(subscriptionKey, Arrays.asList(topicsToRemove));
    verify(listenerContainer).removeMessageListener(listener, Arrays.asList(topicsToRemove));

  }

  @Test
  public void removeSubscriptions() throws Exception {
    final String subscriberId = "mockSubscriber";
    final String subscriptionKey = "subs:" + subscriberId;
    final Topic[] topicsToRemove = {new ChannelTopic("/alarm/provider2"), new ChannelTopic("/data/provider2")};
    when(listenerFactory.getObject()).thenReturn(listener);
    when(repository.getSubscriptionKey(subscriberId)).thenReturn(subscriptionKey);

    registry.removeSubscriptions(subscriberId, Arrays.asList(topicsToRemove));

    verify(repository).removeSubscriptions(subscriptionKey, Arrays.asList(topicsToRemove));
    verify(listenerContainer).removeMessageListener(listener, Arrays.asList(topicsToRemove));
  }

  private Map<String, NotificationParams> buildSubscriptions() {
    final Map<String, NotificationParams> subscriptions = new HashMap<String, NotificationParams>();
    subscriptions.put("/data/provider1/sensor1",
        unmarshallNotificationParams("{\"endpoint\":\"http://127.0.0.1/endpoint\", \"secretCallbackKey\":\"ABCDEFGH\"}"));
    subscriptions.put("/data/provider2", unmarshallNotificationParams("{\"endpoint\":\"http://127.0.0.1/endpoint\"}"));
    subscriptions.put("/data/provider3*", unmarshallNotificationParams("{\"endpoint\":\"http://127.0.0.1/endpoint\"}"));
    subscriptions.put("/alarm/provider2", unmarshallNotificationParams("{\"endpoint\":\"http://127.0.0.1/endpoint\"}"));
    return subscriptions;
  }

  private NotificationParams unmarshallNotificationParams(final String jsonValue) {
    return converter.unmarshal(jsonValue, NotificationParams.class);
  }
}
