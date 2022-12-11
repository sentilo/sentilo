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
import java.util.Map;
import java.util.Set;

import org.sentilo.platform.common.domain.NotificationParams;
import org.springframework.data.redis.listener.Topic;

public interface SubscriberRepository {

  /**
   * Returns a collection with all subscription keys stored in Redis: each entry represents a
   * subscription key related to a subscriber.
   *
   * @return
   */
  Set<String> loadSubscriptions();

  Map<String, NotificationParams> loadSubscription(final String subscriptionKey);

  /**
   * Remove all subscriptions associated with key <code>subscriptionKey</code>
   *
   * @param subscriptionKey
   */
  void removeAllSubscriptions(final String subscriptionKey);

  /**
   * Remove subscriptions to given topics from subscription with key <code>subscriptionKey</code>
   *
   * @param subscriptionKey
   * @param topics
   */
  void removeSubscriptions(final String subscriptionKey, final Collection<? extends Topic> topics);

  void addSubscription(final String subscriberId, final Topic topic, final NotificationParams notificationParams);

  /**
   * Returns Redis subscription key related to subscriber with identifier <code>subscriberId</code>
   *
   * @param subscriberId Subscriber identifier
   * @return Redis subscription key related to given subscriber.
   */
  String getSubscriptionKey(final String subscriberId);

  /**
   * Given a channel subscription from subscriber <code>subscriberId</code>, updates its definition
   *
   * @param subscriberId Subscriber identifier
   * @param oldChannel Old channel
   * @param newChannel New Channel
   */
  void updateChannelDefinition(final String subscriberId, final String oldChannel, final String newChannel);
}
