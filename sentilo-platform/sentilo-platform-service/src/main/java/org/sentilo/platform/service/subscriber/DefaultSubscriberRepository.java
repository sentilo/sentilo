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
package org.sentilo.platform.service.subscriber;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.sentilo.common.converter.DefaultStringMessageConverter;
import org.sentilo.common.converter.StringMessageConverter;
import org.sentilo.platform.common.domain.NotificationParams;
import org.sentilo.platform.service.dao.SentiloKeysBuilder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.core.HashOperations;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.data.redis.listener.Topic;
import org.springframework.stereotype.Repository;
import org.springframework.util.CollectionUtils;

@Repository
public class DefaultSubscriberRepository implements SubscriberRepository {

  @Autowired
  private StringRedisTemplate redisTemplate;
  private SentiloKeysBuilder keysBuilder = new SentiloKeysBuilder();
  private final StringMessageConverter converter = new DefaultStringMessageConverter();

  private final String SUBS_REGISTRY_KEY = "_subs:registry";

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.platform.service.subscriber.SubscriberRepository#getSubscriptionKey(java.lang.
   * String)
   */
  @Override
  public String getSubscriptionKey(final String subscriberKey) {
    return keysBuilder.getSubscriptionKey(subscriberKey);
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.sentilo.platform.service.subscriber.SubscriberRepository#loadSubscriptions()
   */
  @Override
  public Set<String> loadSubscriptions() {
    return redisTemplate.opsForSet().members(SUBS_REGISTRY_KEY);
  }

  /*
   * (non-Javadoc)
   * 
   * @see
   * org.sentilo.platform.service.subscriber.SubscriberRepository#loadSubscription(java.lang.String)
   */
  @Override
  public Map<String, NotificationParams> loadSubscription(final String subscriptionKey) {
    final Map<String, NotificationParams> subscription = new HashMap<>();
    final HashOperations<String, String, String> hashOps = redisTemplate.opsForHash();
    final Map<String, String> entries = hashOps.entries(subscriptionKey);
    if (!CollectionUtils.isEmpty(entries)) {
      entries.forEach((k, v) -> subscription.put(k, converter.unmarshal(v, NotificationParams.class)));
    }

    return subscription;
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.platform.service.subscriber.SubscriberRepository#removeAllSubscriptions(java.lang.
   * String)
   */
  @Override
  public void removeAllSubscriptions(final String subscriptionKey) {
    redisTemplate.delete(subscriptionKey);
    redisTemplate.opsForSet().remove(SUBS_REGISTRY_KEY, subscriptionKey);
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.platform.service.subscriber.SubscriberRepository#removeSubscriptions(java.lang.
   * String, java.util.Collection)
   */
  @Override
  public void removeSubscriptions(final String subscriptionKey, final Collection<? extends Topic> topics) {
    final Object[] aTopics = topics.stream().map(topic -> topic.getTopic()).toArray();
    redisTemplate.opsForHash().delete(subscriptionKey, aTopics);
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.platform.service.subscriber.SubscriberRepository#addSubscription(java.lang.String,
   * java.lang.String, java.lang.String)
   */
  @Override
  public void addSubscription(final String subscriberId, final Topic topic, final NotificationParams notificationParams) {
    final String subscriptionKey = getSubscriptionKey(subscriberId);
    final String sNotifParams = converter.marshal(notificationParams);
    redisTemplate.opsForHash().put(subscriptionKey, topic.getTopic(), sNotifParams);
    redisTemplate.opsForSet().add(SUBS_REGISTRY_KEY, subscriptionKey);
  }

  public void updateChannelDefinition(final String subscriberId, final String oldChannel, final String newChannel) {
    final String subscriptionKey = getSubscriptionKey(subscriberId);
    final Object sNotifParams = redisTemplate.opsForHash().get(subscriptionKey, oldChannel);
    redisTemplate.opsForHash().delete(subscriptionKey, oldChannel);
    redisTemplate.opsForHash().put(subscriptionKey, newChannel, sNotifParams);
  }

}
