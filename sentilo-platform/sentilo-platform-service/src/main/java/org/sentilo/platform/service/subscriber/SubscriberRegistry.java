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

import java.time.Duration;
import java.time.Instant;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import javax.annotation.PostConstruct;

import org.sentilo.common.enums.SubscribeType;
import org.sentilo.common.lock.LockFactory;
import org.sentilo.common.utils.MessagingUtils;
import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.platform.common.domain.NotificationParams;
import org.sentilo.platform.service.messaging.EventMessageListener;
import org.sentilo.platform.service.messaging.EventMessageListenerFactory;
import org.sentilo.platform.service.messaging.MessageListenerContainer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.listener.ChannelTopic;
import org.springframework.data.redis.listener.Topic;
import org.springframework.data.util.Pair;
import org.springframework.lang.Nullable;
import org.springframework.stereotype.Component;
import org.springframework.util.Assert;
import org.springframework.util.CollectionUtils;

@Component
public class SubscriberRegistry {

  private static final Logger LOGGER = LoggerFactory.getLogger(SubscriberRegistry.class);
  private static final String SUBS_REGISTRY_LOCK = SubscriberRegistry.class.getName() + ".lock";

  @Autowired
  private SubscriberRepository repository;
  @Autowired
  private MessageListenerContainer listenerContainer;
  @Autowired
  private EventMessageListenerFactory listenerFactory;

  @Autowired
  private LockFactory lockFactory;

  private boolean storedSubscriptionsEnabled = false;

  private final Map<String, EventMessageListener> listeners = new HashMap<String, EventMessageListener>();

  @PostConstruct
  public void init() {
    loadSubscriptions();
    Assert.state(storedSubscriptionsEnabled, "API Server starts failed beacause registered subscriptions has not been loaded");
  }

  /**
   * This method registers a new subscription to given topic (if not exist) related to subscriber
   * <code>subscriberId</code>
   *
   * @param subscriberId
   * @param topic
   * @param params
   */
  public void add(final String subscriberId, final Topic topic, final NotificationParams notificationParams) {
    try {
      lockFactory.getLock(SUBS_REGISTRY_LOCK, false).lock();
      final boolean activated = activateSubscription(subscriberId, topic, notificationParams);
      if (activated) {
        // Persists subscription in Redis to be loaded when module starts
        repository.addSubscription(subscriberId, topic, notificationParams);
      }
    } finally {
      lockFactory.getLock(SUBS_REGISTRY_LOCK, false).unlock();
    }
  }

  /**
   * Remove all subscriptions for given subscriber
   *
   * @param subscriberId
   */
  public void removeSubscriptions(final String subscriberId) {
    lockFactory.getLock(SUBS_REGISTRY_LOCK, false).lock();
    try {
      final String subscriptionKey = repository.getSubscriptionKey(subscriberId);
      // Remove all subscriptions from both backend repository
      repository.removeAllSubscriptions(subscriptionKey);
      // ... and listeners container
      final Optional<EventMessageListener> listener = getListener(subscriberId);
      if (listener.isPresent()) {
        listenerContainer.removeMessageListener(listener.get());
      }
    } finally {
      lockFactory.getLock(SUBS_REGISTRY_LOCK, false).unlock();
    }
  }

  /**
   * Remove all subscriptions of the given type for subscriber <code>subscriberId</code>
   *
   * @param subscriberId
   * @param type
   */
  public void removeSubscriptions(final String subscriberId, final SubscribeType type) {
    lockFactory.getLock(SUBS_REGISTRY_LOCK, false).lock();
    try {
      final String subscriptionKey = repository.getSubscriptionKey(subscriberId);
      final Map<String, NotificationParams> subscriptionsChannels = repository.loadSubscription(subscriptionKey);
      final List<String> subscriptionsToRemove = MessagingUtils.filterTopicsOfType(subscriptionsChannels.keySet(), type);
      final Collection<Topic> topics = subscriptionsToRemove.stream().map(sTopic -> buildTopic(sTopic)).collect(Collectors.toList());
      _removeSubscriptions(subscriberId, topics);
    } finally {
      lockFactory.getLock(SUBS_REGISTRY_LOCK, false).unlock();
    }
  }

  /**
   * Remove subscriptions to given topics for subscriber <code>subscriberId</code>
   *
   * @param subscriberId
   * @param type
   */
  public void removeSubscriptions(final String subscriberId, final Collection<Topic> topics) {
    lockFactory.getLock(SUBS_REGISTRY_LOCK, false).lock();
    try {
      _removeSubscriptions(subscriberId, topics);
    } finally {
      lockFactory.getLock(SUBS_REGISTRY_LOCK, false).unlock();
    }
  }

  private void _removeSubscriptions(final String subscriberId, final Collection<Topic> topics) {
    if (CollectionUtils.isEmpty(topics)) {
      return;
    }

    final String subscriptionKey = repository.getSubscriptionKey(subscriberId);
    // Remove subscriptions to given topics from Redis
    repository.removeSubscriptions(subscriptionKey, topics);
    // and remove subscriptions from listener container
    final Optional<EventMessageListener> listener = getListener(subscriberId);
    if (listener.isPresent()) {
      listenerContainer.removeMessageListener(listener.get(), topics);
    }
  }

  /**
   * Returns a Map with all subscriptions registered for subscriber <code>subscriberId</code> Each
   * entry consist of a pair (key,value) where pair is the channel of the subscription and value are
   * the parameters for the notifications.
   *
   * @param subscriberId
   * @return
   */
  public Map<String, NotificationParams> loadSubscription(final String subscriberId, final @Nullable SubscribeType type) {
    final String subscriptionKey = repository.getSubscriptionKey(subscriberId);
    final Map<String, NotificationParams> subscriptions = repository.loadSubscription(subscriptionKey);
    if (type != null) {
      final List<String> filteredSubscriptions = MessagingUtils.filterTopicsOfType(subscriptions.keySet(), type);
      return filteredSubscriptions.stream().collect(Collectors.toMap(e -> e, e -> subscriptions.get(e)));
    } else {
      return subscriptions;
    }
  }

  /**
   * Method invoked when a signal of type RELOAD_SUBSCRIPTIONS is published. Clear current in-memory
   * subscriptions and load again subscriptions from back-end
   *
   * @see Signaltype.RELOAD_SUBSCRIPTIONS
   */
  public void reloadSubscriptions() {
    lockFactory.getLock(SUBS_REGISTRY_LOCK, false).lock();
    try {
      _loadSubscriptions();
    } finally {
      lockFactory.getLock(SUBS_REGISTRY_LOCK, false).unlock();
    }
  }

  // @Scheduled(initialDelay = 1000, fixedDelay = 30000)
  /**
   * Method invoked when registry starts. Load registered subscriptions from back-end and activated
   * each one. This load is mandatory, so if it fails, API Server is stopped.
   */
  public void loadSubscriptions() {

    final int maxRetries = 3;
    int currentRetries = 0;
    while (!storedSubscriptionsEnabled && currentRetries < maxRetries) {
      lockFactory.getLock(SUBS_REGISTRY_LOCK, false).lock();
      try {
        _loadSubscriptions();
      } finally {
        lockFactory.getLock(SUBS_REGISTRY_LOCK, false).unlock();
        currentRetries++;
      }
    }
  }

  private void _loadSubscriptions() {
    try {
      final Instant init = Instant.now();
      listenerContainer.disable();
      listenerContainer.clear();
      LOGGER.info("Init process to load subscriptions stored in Redis");
      final Set<String> storedSubscriptionsKeys = repository.loadSubscriptions();
      activateSubscriptions(storedSubscriptionsKeys);
      storedSubscriptionsEnabled = true;
      listenerContainer.enable();
      final Instant end = Instant.now();
      LOGGER.info("Load subscriptions process finished in {} ms", Duration.between(init, end).toMillis());
    } catch (final Exception e) {
      LOGGER.warn("An error has been raised while loading subscriptions from Redis. Load will be retried later: {}", e.getMessage(), e);
      storedSubscriptionsEnabled = false;
    }
  }

  private void activateSubscriptions(final Set<String> storedSubscriptionsKeys) {
    LOGGER.info("Found {} subscriptions stored in Redis", storedSubscriptionsKeys.size());

    if (CollectionUtils.isEmpty(storedSubscriptionsKeys)) {
      return;
    }

    for (final String subscriptionKey : storedSubscriptionsKeys) {
      // Each subscriptionKey represents an entity subscribed to N channels (in Redis is stored as a
      // hash).
      // For each hash entry, the key is the channel and the value stores the notification params.
      final Map<String, NotificationParams> storedSubscription = repository.loadSubscription(subscriptionKey);
      if (CollectionUtils.isEmpty(storedSubscription)) {
        // It corresponds to a registered subscription without context (channel, params, ... )
        repository.removeAllSubscriptions(subscriptionKey);
        continue;
      }

      final Set<String> channels = storedSubscription.keySet();
      final String listenerName = listenerNameFromSubscriptionKey(subscriptionKey);

      for (final String channel : channels) {
        // @Since Sentilo v2.0: migrates old styles subscriptions channels to new ones in Redis
        final Pair<Boolean, String> aux = isOldStyleTopicDefinition(channel);
        if (aux.getFirst()) {
          repository.updateChannelDefinition(listenerName, channel, aux.getSecond());
        }
        activateSubscription(listenerName, buildTopic(aux.getSecond()), storedSubscription.get(channel));
      }
    }
  }

  /**
   * Returns a pair <boolean, String> which indicates if topic follows old-style-rule to define a
   * topic (e.g. if topic ends with * to define a pattern)
   *
   * @param topic
   * @return Pair <boolean, String> where first elements indicates if topic follows old-style-rule
   *         and second contains formatted topic expressed in new style
   * @since 2.0
   */
  private Pair<Boolean, String> isOldStyleTopicDefinition(final String topic) {
    final String formattedTopic = MessagingUtils.formatTopicExpression(topic);

    return Pair.of(!topic.equals(formattedTopic), formattedTopic);
  }

  /**
   * Temporal utility to migrate old redis topic-style-definition to new one.
   *
   * @param channel
   * @return
   */
  private Topic buildTopic(final String channel) {
    final String formatedChannel = MessagingUtils.formatTopicExpression(channel);
    return new ChannelTopic(formatedChannel);
  }

  private String listenerNameFromSubscriptionKey(final String subscriptionKey) {
    // subscriptionKey follows the expression subs:<listenerName>
    final int pos = subscriptionKey.lastIndexOf(SentiloConstants.REDIS_KEY_TOKEN);
    return subscriptionKey.substring(pos + 1);
  }

  private boolean activateSubscription(final String listenerName, final Topic topic, final NotificationParams notificationParams) {

    boolean activated = false;
    final Optional<EventMessageListener> listener = getListener(listenerName);
    if (listener.isPresent()) {
      LOGGER.debug("Init process to enable subscription to topic {} for subscriber {}", topic.getTopic(), listenerName);

      listenerContainer.addMessageListener(listener.get(), topic);
      listener.get().addSubscription(topic, notificationParams);
      activated = true;
      LOGGER.info("Subscription to topic {} enabled and running for subscriber {}.", topic.getTopic(), listenerName);
    } else {
      LOGGER.warn("Subscription to topic {} for listener {} hasn't been activated. Review logs", topic.getTopic(), listenerName);
    }

    return activated;
  }

  private Optional<EventMessageListener> getListener(final String listenerName) {
    try {
      if (!listeners.containsKey(listenerName)) {
        listenerFactory.setListenerName(listenerName);
        final EventMessageListener listener = listenerFactory.getObject();
        listeners.put(listenerName, listener);
      }
    } catch (final Exception e) {
      LOGGER.warn("Has been unable to build EventMessageListener {}. ", listenerName, e);
    }
    return Optional.ofNullable(listeners.get(listenerName));
  }

}
