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

import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Arrays;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.platform.common.domain.NotificationParams;
import org.sentilo.platform.service.subscriber.DefaultSubscriberRepository;
import org.springframework.data.redis.core.HashOperations;
import org.springframework.data.redis.core.SetOperations;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.data.redis.listener.ChannelTopic;
import org.springframework.data.redis.listener.Topic;

public class SubscriberRepositoryTest {

  @Mock
  private StringRedisTemplate redisTemplate;
  @Mock
  private SetOperations<String, String> setOps;
  @Mock
  private HashOperations<String, Object, Object> hashOps;

  @InjectMocks
  private DefaultSubscriberRepository repository;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void getSubscriptionKey() {
    final String subscriberKey = "mockApp";

    Assert.assertEquals("subs:mockApp", repository.getSubscriptionKey(subscriberKey));
  }

  @Test
  public void loadSubscriptions() {
    when(redisTemplate.opsForSet()).thenReturn(setOps);

    repository.loadSubscriptions();

    verify(setOps).members("_subs:registry");
  }

  @Test
  public void loadSubscription() {
    final String subsKey = "mockSubsKey";
    when(redisTemplate.opsForHash()).thenReturn(hashOps);

    repository.loadSubscription(subsKey);

    verify(hashOps).entries(subsKey);
  }

  @Test
  public void removeAllSubscriptions() {
    final String subsKey = "mockSubsKey";
    when(redisTemplate.opsForSet()).thenReturn(setOps);

    repository.removeAllSubscriptions(subsKey);

    verify(redisTemplate).delete(subsKey);
    verify(setOps).remove("_subs:registry", subsKey);
  }

  @Test
  public void removeSubscription() {
    final String subsKey = "mockSubsKey";
    final Topic topic1 = new ChannelTopic("/data/mockProvider/mockSensor/");
    final Topic topic2 = new ChannelTopic("/data/mockProvider/mockSensor2/");
    final Topic[] topics = {topic1, topic2};
    when(redisTemplate.opsForHash()).thenReturn(hashOps);

    repository.removeSubscriptions(subsKey, Arrays.asList(topics));

    verify(hashOps).delete(subsKey, topic1.getTopic(), topic2.getTopic());
  }

  @Test
  public void addSubscription() {
    final String subsKey = "subs:mockApp";
    final String entityName = "mockApp";
    final Topic topic = new ChannelTopic("/data/mockProvider/mockSensor/");
    final NotificationParams notificationParams = new NotificationParams();

    when(redisTemplate.opsForSet()).thenReturn(setOps);
    when(redisTemplate.opsForHash()).thenReturn(hashOps);

    repository.addSubscription(entityName, topic, notificationParams);

    verify(hashOps).put(eq(subsKey), eq(topic.getTopic()), anyString());
    verify(setOps).add("_subs:registry", subsKey);
  }

  @Test
  public void updateChannelDefinition() {
    final String subsKey = "subs:mockApp";
    final String subscriberId = "mockApp";
    final String oldChannel = "/data/mockProvider*";
    final String newChannel = "/data/mockProvider";
    final String notifParams = "{...}";
    when(redisTemplate.opsForHash()).thenReturn(hashOps);
    when(hashOps.get(subsKey, oldChannel)).thenReturn(notifParams);

    repository.updateChannelDefinition(subscriberId, oldChannel, newChannel);

    verify(hashOps).get(subsKey, oldChannel);
    verify(hashOps).delete(subsKey, oldChannel);
    verify(hashOps).put(subsKey, newChannel, notifParams);
  }

}
