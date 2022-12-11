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
package org.sentilo.platform.server.pool;

import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import org.sentilo.common.factory.SentiloThreadFactory;
import org.sentilo.platform.server.SentiloHttpRequestTask;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Component
public class ThreadPool {

  private static final Logger LOGGER = LoggerFactory.getLogger(ThreadPool.class);

  private ThreadPoolExecutor threadPool;

  @Value("${sentilo.server.api.thread.pool.queue-size}")
  private int queueSize;
  @Value("${sentilo.server.api.thread.pool.size.core}")
  private int initialCapacity;
  @Value("${sentilo.server.api.thread.pool.size.max}")
  private int maxCapacity;
  @Value("${sentilo.server.api.thread.pool.keep-alive-seconds}")
  private int shutdownSecondsTimeout;
  @Value("${sentilo.server.api.thread.pool.group-id}")
  private String groupId;
  @Value("${sentilo.server.api.thread.pool.group-name}")
  private String groupName;

  private WrapperBlockingQueue queue;

  public void initialize() {
    LOGGER.info("Initializing thread pool.");
    debug();

    queue = new WrapperBlockingQueue(queueSize);

    threadPool = new ThreadPoolExecutor(initialCapacity, maxCapacity, shutdownSecondsTimeout, TimeUnit.SECONDS, queue,
        new SentiloThreadFactory(groupName, groupId));
    threadPool.prestartAllCoreThreads();

    threadPool.allowCoreThreadTimeOut(false);
    threadPool.setRejectedExecutionHandler((r, executor) -> queue.backdoorOffer(r));
    LOGGER.info("Thread pool initialized");
  }

  public void submit(final SentiloHttpRequestTask task) {
    threadPool.submit(task);
  }

  public void shutdownControlled() {
    if (threadPool != null) {
      try {
        threadPool.awaitTermination(shutdownSecondsTimeout, TimeUnit.SECONDS);
      } catch (final InterruptedException ex) {
        threadPool.shutdownNow();
        Thread.currentThread().interrupt();
      }
    }
  }

  public long getCurrentTasks() {
    // Return number of queued tasks more number of running tasks
    return threadPool.getQueue().size() + threadPool.getActiveCount();
  }

  public void shutdown() {
    if (threadPool != null) {
      threadPool.shutdown();
      try {
        if (!threadPool.awaitTermination(shutdownSecondsTimeout, TimeUnit.SECONDS)) {
          threadPool.shutdownNow();
        }
      } catch (final InterruptedException ex) {
        threadPool.shutdownNow();
        Thread.currentThread().interrupt();
      }
    }
  }

  public ThreadPoolExecutor getThreadPoolExecutor() {
    return threadPool;
  }

  public int getInitialCapacity() {
    return initialCapacity;
  }

  public void setInitialCapacity(final int initialCapacity) {
    this.initialCapacity = initialCapacity;
  }

  public int getMaxCapacity() {
    return maxCapacity;
  }

  public void setMaxCapacity(final int maxCapacity) {
    this.maxCapacity = maxCapacity;
  }

  public int getShutdownSecondsTimeout() {
    return shutdownSecondsTimeout;
  }

  public void setShutdownSecondsTimeout(final int shutdownSecondsTimeout) {
    this.shutdownSecondsTimeout = shutdownSecondsTimeout;
  }

  public String getGroupId() {
    return groupId;
  }

  public void setGroupId(final String groupId) {
    this.groupId = groupId;
  }

  public String getGroupName() {
    return groupName;
  }

  public void setGroupName(final String groupName) {
    this.groupName = groupName;
  }

  public int getQueueSize() {
    return queueSize;
  }

  public void setQueueSize(final int queueSize) {
    this.queueSize = queueSize;
  }

  private void debug() {
    if (LOGGER.isDebugEnabled()) {
      LOGGER.debug("thread.pool.group-id: {}", groupId);
      LOGGER.debug("thread.pool.group-name: {}", groupName);
      LOGGER.debug("thread.pool.size.core: {}", initialCapacity);
      LOGGER.debug("thread.pool.size.max: {}", maxCapacity);
      LOGGER.debug("thread.pool.queue.size: {}", queueSize);
      LOGGER.debug("thread.pool.keep-alive-seconds: {}", shutdownSecondsTimeout);
    }
  }
}
