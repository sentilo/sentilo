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
package org.sentilo.platform.service.messaging;

import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;

import org.sentilo.common.domain.EventMessage;
import org.sentilo.common.factory.SentiloThreadFactory;
import org.sentilo.common.utils.MessagingUtils;
import org.sentilo.platform.common.event.InputMessageEvent;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.event.EventListener;
import org.springframework.data.redis.listener.RedisMessageListenerContainer;
import org.springframework.data.redis.listener.Topic;
import org.springframework.lang.Nullable;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;
import org.springframework.util.Assert;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ErrorHandler;

/**
 * Represents a listener container for events of type {@link InputMessageEvent}.
 *
 * Based on {@link RedisMessageListenerContainer} but in this case incoming events are published not
 * through Redis but through the same application instance.
 */
@Component
public class InputMessageListenerContainer implements MessageListenerContainer {

  private static final Logger LOGGER = LoggerFactory.getLogger(InputMessageListenerContainer.class);

  private static final int DEFAULT_NUM_MAX_WORKERS = 10;
  private static final int DEFAULT_NUM_MIN_WORKERS = 0;

  @Nullable
  private ExecutorService taskExecutor;
  @Nullable
  private ErrorHandler errorHandler;

  private DispatchMessageListener dispatcher;

  @Value("${sentilo.server.api.subs.container.workers.size.min:0}")
  private int numMinWorkers;

  @Value("${sentilo.server.api.subs.container.workers.size.max:10}")
  private int numMaxWorkers;

  // whether the task executor is self-managed or has been injected externally
  private volatile boolean manageExecutor = false;
  // whether the container has been initialized and could process events
  private volatile boolean active = false;
  // whether the container is running (or not)
  private volatile boolean running = false;

  // Pending queue to keep entry events while container is paused (e.g. while subscriptions are been
  // loading from Redis)
  private final Queue<InputMessageEvent> pendingEvents = new LinkedBlockingQueue<InputMessageEvent>();
  // lookup map between topics and listeners
  private final Map<String, Collection<EventMessageListener>> topicMapping = new ConcurrentHashMap<>();
  // lookup map between listeners and topics
  private final Map<EventMessageListener, Set<Topic>> listenerTopics = new ConcurrentHashMap<>();

  @Override
  public void clear() {
    topicMapping.clear();
    listenerTopics.clear();
  }

  @PostConstruct
  public void init() {

    if (numMaxWorkers == 0) {
      numMaxWorkers = DEFAULT_NUM_MAX_WORKERS;
    }

    if (numMinWorkers == 0) {
      numMinWorkers = DEFAULT_NUM_MIN_WORKERS;
    }

    if (numMinWorkers > numMaxWorkers) {
      LOGGER.info("Field numMinWorkers is greater that numMaxWorkers. Setting numMinWorkers to {}", numMaxWorkers);
      numMinWorkers = numMaxWorkers;
    }

    if (taskExecutor == null) {
      manageExecutor = true;
      taskExecutor = new ThreadPoolExecutor(numMinWorkers, numMaxWorkers, 60L, TimeUnit.SECONDS, new LinkedBlockingQueue<Runnable>(),
          new SentiloThreadFactory("MessagingGroup", "EventDispatcherWorker"));
    }

    if (dispatcher == null) {
      dispatcher = new DispatchMessageListener();
    }
  }

  @PreDestroy
  public void destroy() throws Exception {
    stop();

    if (manageExecutor) {
      taskExecutor.shutdown();
      // Pause 30 seconds maximum while submitted tasks finish
      final boolean finished = taskExecutor.awaitTermination(30, TimeUnit.SECONDS);
      LOGGER.debug("Stopped internally-managed task executor. All tasks finished? {}", finished);
    }
  }

  @Override
  @Async
  @EventListener
  public void onApplicationEvent(final InputMessageEvent event) {
    // Delegates message to dispatcher if container is active, otherwise add event to pending queue
    if (active) {
      dispatcher.onMessage(event.getEventMessage());
    } else {
      pendingEvents.add(event);
    }
  }

  @Override
  public void start() {
    if (!running) {
      running = true;
      LOGGER.debug("Started InputMessageListenerContainer");
    }
  }

  @Override
  public void stop() {
    active = false;
    if (isRunning()) {
      running = false;
    }

    LOGGER.debug("Stopped InputMessageListenerContainer");
  }

  @Override
  public boolean isRunning() {
    return running;
  }

  @Override
  public void disable() {
    active = false;
  }

  @Override
  public void enable() {
    active = true;
    processPendingEvents();
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

  @Override
  public int getPhase() {
    // start the latest
    return Integer.MAX_VALUE;
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.platform.service.messaging.MessageListenerContainer#removeMessageListener(org.
   * sentilo.platform.service.messaging.EventMessageListener)
   */
  @Override
  public void removeMessageListener(final EventMessageListener eventMessageListener) {
    final Set<Topic> topics = listenerTopics.get(eventMessageListener);
    removeListener(eventMessageListener, topics);

    listenerTopics.remove(eventMessageListener);
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.platform.service.messaging.MessageListenerContainer#removeMessageListener(org.
   * sentilo.platform.service.messaging.EventMessageListener,
   * org.springframework.data.redis.listener.Topic)
   */
  @Override
  public void removeMessageListener(final EventMessageListener eventMessageListener, final Topic topic) {
    removeMessageListener(eventMessageListener, Collections.singleton(topic));
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.platform.service.messaging.MessageListenerContainer#removeMessageListener(org.
   * sentilo.platform.service.messaging.EventMessageListener, java.util.Collection)
   */
  @Override
  public void removeMessageListener(final EventMessageListener eventMessageListener, final Collection<? extends Topic> topics) {
    removeListener(eventMessageListener, topics);
  }

  /**
   * Adds a message listener to the (potentially running) container. If the container is running,
   * the listener starts receiving (matching) messages as soon as possible.
   *
   * @param listener message listener
   * @param topic message topic
   */
  @Override
  public void addMessageListener(final EventMessageListener listener, final Topic topic) {
    addMessageListener(listener, Collections.singleton(topic));
  }

  /**
   * Adds a message listener to the (potentially running) container. If the container is running,
   * the listener starts receiving (matching) messages as soon as possible.
   *
   * @param listener message listener
   * @param topics message listener topic
   */
  public void addMessageListener(final EventMessageListener listener, final Collection<? extends Topic> topics) {
    addListener(listener, topics);
  }

  private void addListener(final EventMessageListener listener, final Collection<? extends Topic> topics) {
    Assert.notNull(listener, "a valid listener is required");
    Assert.notEmpty(topics, "at least one topic is required");

    // add listener mapping
    Set<Topic> set = listenerTopics.get(listener);
    if (set == null) {
      set = new HashSet<>();
      listenerTopics.put(listener, set);
    }
    set.addAll(topics);

    for (final Topic topic : topics) {
      Collection<EventMessageListener> collection = topicMapping.get(topic.getTopic());
      if (collection == null) {
        collection = new HashSet<>();
        topicMapping.put(topic.getTopic(), collection);
      }
      collection.add(listener);

      LOGGER.debug("Adding listener {} on channel {}", listener, topic.getTopic());
    }
  }

  private void removeListener(final EventMessageListener eventMessageListener, final Collection<? extends Topic> topics) {
    if (CollectionUtils.isEmpty(topics)) {
      return;
    }

    topics.forEach(topic ->
      {
        eventMessageListener.removeSubscription(topic);
        listenerTopics.get(eventMessageListener).remove(topic);
        if (topicMapping.containsKey(topic.getTopic())) {
          topicMapping.get(topic.getTopic()).remove(eventMessageListener);
        }
      });
  }

  /**
   * Process a message received from the provider.
   *
   * @param message
   */
  protected void processMessage(final EventMessageListener listener, final EventMessage message) {
    executeListener(listener, message);
  }

  /**
   * Execute the specified listener.
   *
   * @see #handleListenerException
   */
  protected void executeListener(final EventMessageListener listener, final EventMessage message) {
    try {
      listener.onMessage(message);
    } catch (final Throwable ex) {
      handleListenerException(ex);
    }
  }

  @Override
  public final boolean isActive() {
    return active;
  }

  /**
   * Handle the given exception that arose during listener execution.
   * <p>
   * The default implementation logs the exception at error level. This can be overridden in
   * subclasses.
   *
   * @param ex the exception to handle
   */
  protected void handleListenerException(final Throwable ex) {
    if (isActive()) {
      // Regular case: failed while active.
      // Invoke ErrorHandler if available.
      invokeErrorHandler(ex);
    } else {
      // Rare case: listener thread failed after container shutdown.
      // Log at debug level, to avoid spamming the shutdown logger.
      LOGGER.debug("Listener exception after container shutdown", ex);
    }
  }

  /**
   * Invoke the registered ErrorHandler, if any. Log at error level otherwise.
   *
   * @param ex the uncaught error that arose during message processing.
   * @see #setErrorHandler
   */
  protected void invokeErrorHandler(final Throwable ex) {
    if (errorHandler != null) {
      errorHandler.handleError(ex);
    } else {
      LOGGER.warn("Execution of message listener failed, and no ErrorHandler has been set.", ex);
    }
  }

  private void dispatchMessage(final Collection<EventMessageListener> listeners, final EventMessage message) {
    for (final EventMessageListener messageListener : listeners) {
      taskExecutor.execute(() -> processMessage(messageListener, message));
    }
  }

  /**
   * Process asynchronously pending events while container is active and there are events to be
   * processed.
   */
  private void processPendingEvents() {
    LOGGER.info("Container enabled. Number of pending events waiting to be processed: {}", pendingEvents.size());
    // @formatter:off
    taskExecutor.execute(() ->  {
      while (active && pendingEvents.size() > 0) {
        final InputMessageEvent entry = pendingEvents.poll();
        dispatcher.onMessage(entry.getEventMessage());
      }
      LOGGER.debug("Process of pending events finished.");
    });
    // @formatter:on
  }

  /**
   * Actual message dispatcher/multiplexer.
   */
  private class DispatchMessageListener {

    public void onMessage(final EventMessage message) {
      final Collection<EventMessageListener> listeners = new HashSet<EventMessageListener>();

      // Broadcast input message between all listeners subscribed to any of the channels derived
      // from input message topic
      final List<String> topicCandidates = MessagingUtils.buildCandidates(message.getTopic());
      topicCandidates.forEach(candidate ->
        {
          if (topicMapping.containsKey(candidate)) {
            listeners.addAll(topicMapping.get(candidate));
          }
        });

      if (!CollectionUtils.isEmpty(listeners)) {
        dispatchMessage(listeners, message);
      }
    }

  }

}
