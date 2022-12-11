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
package org.sentilo.platform.service.impl;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.sentilo.common.enums.SignalType;
import org.sentilo.common.enums.SubscribeType;
import org.sentilo.platform.common.domain.AlarmSubscription;
import org.sentilo.platform.common.domain.DataSubscription;
import org.sentilo.platform.common.domain.NotificationParams;
import org.sentilo.platform.common.domain.OrderSubscription;
import org.sentilo.platform.common.domain.Subscription;
import org.sentilo.platform.common.service.PublishService;
import org.sentilo.platform.common.service.ResourceService;
import org.sentilo.platform.common.service.SubscribeService;
import org.sentilo.platform.service.subscriber.SubscriberRegistry;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.listener.Topic;
import org.springframework.stereotype.Service;

@Service
public class SubscribeServiceImpl extends AbstractPlatformServiceImpl implements SubscribeService {

  private static final Logger LOGGER = LoggerFactory.getLogger(SubscribeServiceImpl.class);

  @Autowired
  private ResourceService resourceService;
  @Autowired
  private SubscriberRegistry subsRegistry;
  @Autowired
  private PublishService publishService;

  @Override
  public void subscribe(final Subscription subscription) {
    // The first step is to validate that the resource to which the subscription refers exists in
    // Sentilo. Otherwise an error is thrown
    checkTargetResourceState(subscription);

    final Topic topic = subscription.getTopic();
    subsRegistry.add(subscription.getSourceEntityId(), topic, subscription.getNotificationParams());

    // Finally, publish a signal to reload subscriptions in all instances
    publishService.publishInternalSignal(SignalType.RELOAD_SUBSCRIPTIONS);

    LOGGER.info("Entity {} now is subscribed to channel {}", subscription.getSourceEntityId(), topic.getTopic());
  }

  @Override
  public void remove(final Subscription subscription) {
    // Given a subscriber, remove a subscription means one of the following cases:
    // a) Remove all its subscriptions.
    // b) Remove subscription to a given channel
    // c) Remove all its subscriptions of a given type.

    final String subscriberId = subscription.getSourceEntityId();
    if (subscription.getType() == null) {
      subsRegistry.removeSubscriptions(subscriberId);
    } else if (subscription.hasResourceIdentified()) {
      final Topic subscriptionToRemove = subscription.getTopic();
      subsRegistry.removeSubscriptions(subscriberId, Collections.singletonList(subscriptionToRemove));
    } else {
      subsRegistry.removeSubscriptions(subscriberId, subscription.getType());
    }

    // Finally, publish a signal to reload subscriptions in all instances
    publishService.publishInternalSignal(SignalType.RELOAD_SUBSCRIPTIONS);
  }

  /**
   * Returns all subscriptions from a given subscriber. Optionally, subscriptions can also be
   * filtered by type.
   */
  @Override
  public List<Subscription> get(final Subscription subscription) {

    final String subscriberId = subscription.getSourceEntityId();
    final SubscribeType type = subscription.getType();
    if (type == null) {
      LOGGER.debug("Retrieving all active subscriptions for entity {}", subscriberId);
    } else {
      LOGGER.debug("Retrieving all active subscriptions of type {} for entity {}", type.name(), subscriberId);
    }

    final Map<String, NotificationParams> subscriptions = subsRegistry.loadSubscription(subscriberId, type);
    final List<Subscription> subscriptionsList =
        subscriptions.keySet().stream().map(topic -> Subscription.build(subscriberId, topic, subscriptions.get(topic))).collect(Collectors.toList());

    LOGGER.debug("Entity {} has {} active subscriptions", subscriberId, subscriptions.size());

    return subscriptionsList;
  }

  private void checkTargetResourceState(final Subscription subscription) {
    switch (subscription.getType()) {
      case DATA:
        resourceService.checkSensorState(((DataSubscription) subscription).getProviderId(), ((DataSubscription) subscription).getSensorId());
        break;
      case ORDER:
        resourceService.checkSensorState(((OrderSubscription) subscription).getOwnerEntityId(), ((OrderSubscription) subscription).getSensorId());
        break;
      case ALARM:
        resourceService.checkAlertState(((AlarmSubscription) subscription).getAlertId());
        break;
      default:
        throw new IllegalArgumentException("Unknown subscription type:" + subscription.getType());
    }
  }
}
