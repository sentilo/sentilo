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
package org.sentilo.common.listener;

import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.SmartLifecycle;
import org.springframework.data.redis.connection.Message;
import org.springframework.data.redis.connection.MessageListener;
import org.springframework.data.redis.connection.RedisInvalidSubscriptionException;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.listener.ChannelTopic;
import org.springframework.data.redis.listener.RedisMessageListenerContainer;
import org.springframework.data.redis.listener.Topic;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.util.ErrorHandler;

/**
 * The goal of this class is to validate that the connection used by the subscription is valid. To
 * achieve this, an auxiliary subscription to a topic named /MONITOR/{sentilo.process.name}_TOPIC is
 * created when the process starts.
 *
 * Then, periodically a ping message is published on this topic and an additional process verifies
 * that this message arrives to the process. If no data is received into some sort of delay this
 * means that the connection to Redis is broken and the related
 * {@link RedisMessageListenerContainer} should be restarted.
 *
 * This validation is complementary with the native control provided by
 * {@link RedisMessageListenerContainer#handleSubscriptionException}
 */
public abstract class RedisSubscriptionMonitor implements SmartLifecycle, MessageListener {

  private static final Logger LOGGER = LoggerFactory.getLogger(RedisSubscriptionMonitor.class);
  private static final int INITIAL_DELAY = 30000;
  private static final int FIXED_DELAY = 10000;
  private static final int MAX_PENDING_EVENTS = 3;

  /**
   * Count difference between the number of published ping messages and the number of these messages
   * which has been arrived to the agent
   */
  private int countPendingEvents = 0;
  private String processName;

  @Autowired
  private RedisMessageListenerContainer listenerContainer;

  @Autowired
  private RedisTemplate<String, String> redisTemplate;

  // whether the monitor is running (or not)
  private volatile boolean running = false;

  // Monitor message listener and topic

  private MessageListener monitorListener = null;
  private Topic monitorTopic = null;

  private final Lock lock = new ReentrantLock();

  private ErrorHandler redisMonitorErrorHandler = new RedisLoggingErrorHandler();

  public abstract String getProcessName();

  @Override
  public void start() {
    // Set a custom ErrorHandler to listenerContainer for customize message output
    listenerContainer.setErrorHandler(redisMonitorErrorHandler);

    if (listenerContainer.isActive() && listenerContainer.isRunning()) {
      processName = getProcessName();
      monitorListener = this;
      monitorTopic = new ChannelTopic(buildMonitorChannelName(processName));
      if (subscribeMonitor()) {
        running = true;
        LOGGER.info("RedisSubscriptionMonitor started for process {}", processName);
      }
    }
  }

  @Override
  public void stop() {
    if (isRunning()) {
      unsubscribeMonitor();
      running = false;
    }

    LOGGER.info("RedisSubscriptionMonitor stopped");
  }

  public void publishPing() {
    redisTemplate.convertAndSend(monitorTopic.getTopic(), "PING SUBSCRIPTION");
    lock.lock();
    try {
      countPendingEvents++;
    } finally {
      lock.unlock();
    }
  }

  public void onMessage(final Message message, final byte[] pattern) {
    lock.lock();
    try {
      countPendingEvents--;
    } finally {
      lock.unlock();
    }
  }

  @Scheduled(initialDelay = INITIAL_DELAY, fixedDelay = FIXED_DELAY)
  public void validateConnection() {
    if (!isRunning()) {
      start();
    }

    if (isRunning()) {
      if (countPendingEvents > MAX_PENDING_EVENTS) {
        LOGGER.warn("Number of pending PING messages [{}] is greater than maximum allowed value [{}]. Subscribe connection will be restarted",
            countPendingEvents, MAX_PENDING_EVENTS);
        restartListenerContainer();
      } else {
        publishPing();
      }
    }
  }

  @Override
  public boolean isRunning() {
    return running;
  }

  @Override
  public int getPhase() {
    // start the latest
    return Integer.MAX_VALUE;
  }

  @Override
  public boolean isAutoStartup() {
    return true;
  }

  @Override
  public void stop(final Runnable callback) {
    stop();
    callback.run();
  }

  private void restartListenerContainer() {
    LOGGER.info("Stopping listener container for process {}", processName);
    try {
      listenerContainer.stop();
    } catch (final Exception re) {
      LOGGER.warn("An error occurred while stopping listener container. If it is not running, we proceed to start it", re.getMessage());
    }

    // Clear count pending events value
    countPendingEvents = 0;

    LOGGER.info("Starting listener container");
    if (!listenerContainer.isRunning()) {
      listenerContainer.start();
    }
  }

  private boolean subscribeMonitor() {
    boolean subscriptionDone = true;
    try {
      LOGGER.debug("Adding subscription to topic {}", monitorTopic.getTopic());
      listenerContainer.addMessageListener(monitorListener, monitorTopic);
    } catch (final RedisInvalidSubscriptionException rise) {
      LOGGER.error("An error occurred while subscribing to topic {}", monitorTopic.getTopic(), rise);
      subscriptionDone = false;
    }

    return subscriptionDone;
  }

  private boolean unsubscribeMonitor() {
    boolean unsubscriptionDone = true;
    try {
      LOGGER.debug("Removing subscription from topic {}", monitorTopic.getTopic());
      listenerContainer.removeMessageListener(monitorListener, monitorTopic);
    } catch (final RedisInvalidSubscriptionException rise) {
      LOGGER.error("An error occurred while unsubscribing from topic {}", monitorTopic.getTopic(), rise);
      unsubscriptionDone = false;
    }

    return unsubscriptionDone;
  }

  private String buildMonitorChannelName(final String processName) {
    return "/MONITOR/" + processName.toUpperCase() + "_TOPIC";
  }

  private static class RedisLoggingErrorHandler implements ErrorHandler {

    @Override
    public void handleError(final Throwable throwable) {
      LOGGER.warn("An error has occurred while attempting to communicate with Redis {}", throwable.getMessage(), throwable);
    }

  }
}
