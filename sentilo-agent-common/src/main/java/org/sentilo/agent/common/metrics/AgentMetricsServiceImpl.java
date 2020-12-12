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
package org.sentilo.agent.common.metrics;

import java.lang.reflect.Field;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.atomic.AtomicLong;

import org.sentilo.agent.common.repository.PendingEventsRepository;
import org.sentilo.agent.common.utils.Utils;
import org.sentilo.common.metrics.SentiloArtifactMetrics;
import org.sentilo.common.metrics.service.impl.SentiloArtifactMetricsServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.connection.jedis.JedisConnectionFactory;
import org.springframework.lang.Nullable;
import org.springframework.util.ReflectionUtils;

import redis.clients.jedis.Jedis;
import redis.clients.util.Pool;

public abstract class AgentMetricsServiceImpl extends SentiloArtifactMetricsServiceImpl implements AgentMetricsCounter {

  public static final String EVENTS_METRICS = "events";
  public static final String EVENTS_INPUT = "input";
  public static final String EVENTS_OUTPUT = "output";
  public static final String EVENTS_QUEUE_SIZE = "retry_queue_size";
  public static final String REMOTE_SERVER_CONN = "remote_server_connection";

  protected final AtomicLong inputEvents = new AtomicLong();
  protected final AtomicLong outputEvents = new AtomicLong();

  protected long previousInputEvents = 0;
  protected long previousOutputEvents = 0;
  protected long currentOutputEvents = 0;
  protected long currentInputEvents = 0;

  @Autowired(required = false)
  private PendingEventsRepository pendingEventRepository;

  @Autowired(required = false)
  @Nullable
  private JedisConnectionFactory jedisConnectionFactory;

  @Nullable
  private Pool<Jedis> pool;

  private boolean isRemoteServerConnectionOk = true;

  /*
   * (non-Javadoc)
   * 
   * @see org.sentilo.agent.common.metrics.AgentMetricsCounters#incrementInputEvents(long)
   */
  @Override
  public void incrementInputEvents(final long value) {
    inputEvents.getAndAdd(value);
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.sentilo.agent.common.metrics.AgentMetricsCounters#incrementOutputEvents(long)
   */
  @Override
  public void incrementOutputEvents(final long value) {
    outputEvents.getAndAdd(value);
  }

  public void isRemoteServerConnectionOk(final boolean isOk) {
    isRemoteServerConnectionOk = isOk;
  }

  protected Map<String, Object> collectCustomMetrics() {
    final Map<String, Object> metrics = new HashMap<String, Object>();
    metrics.put(EVENTS_METRICS, collectEventsMetrics());
    if (getRedisPool() != null) {
      metrics.put(REDIS_POOL_METRICS, collectRedisPoolMetrics());
    }
    return metrics;
  }

  protected Map<String, Object> collectRedisPoolMetrics() {
    final Map<String, Object> redisPoolMetrics = new HashMap<String, Object>();

    redisPoolMetrics.put(REDIS_POOL_NUM_ACTIVE, getRedisPool().getNumActive());
    redisPoolMetrics.put(REDIS_POOL_NUM_IDLE, getRedisPool().getNumIdle());
    redisPoolMetrics.put(REDIS_POOL_NUM_WAITERS, getRedisPool().getNumWaiters());
    redisPoolMetrics.put(REDIS_POOL_MAX_BORROW_TS, getRedisPool().getMaxBorrowWaitTimeMillis());
    redisPoolMetrics.put(REDIS_POOL_MEAN_BORROW_TS, getRedisPool().getMeanBorrowWaitTimeMillis());

    return redisPoolMetrics;
  }

  protected Map<String, Object> collectEventsMetrics() {
    final Map<String, Object> eventsMetrics = new HashMap<String, Object>();

    final long totalOutputEvents = outputEvents.get();
    final long totalInputEvents = inputEvents.get();

    // Current I/O events counters
    currentOutputEvents = totalOutputEvents - previousOutputEvents;
    currentInputEvents = totalInputEvents - previousInputEvents;

    previousOutputEvents = totalOutputEvents;
    previousInputEvents = totalInputEvents;

    eventsMetrics.put(EVENTS_INPUT, currentInputEvents);
    eventsMetrics.put(EVENTS_OUTPUT, currentOutputEvents);
    eventsMetrics.put(EVENTS_QUEUE_SIZE, getRetryQueueSize());
    if (addRemoteServerConnection()) {
      eventsMetrics.put(REMOTE_SERVER_CONN, getRemoteServerConnectionState());
    }

    return eventsMetrics;
  }

  @Override
  protected String getName() {
    return Utils.getAgentName();
  }

  protected long getRetryQueueSize() {
    return pendingEventRepository != null ? pendingEventRepository.pendingQueueSize() : 0;
  }

  protected boolean addRemoteServerConnection() {
    return true;
  }

  protected String getRemoteServerConnectionState() {
    return isRemoteServerConnectionOk ? STATUS_OK : STATUS_KO;
  }

  /**
   * Agent status may have the following values: - RED if remote server connection status is KO -
   * ORANGE if number of current input events is 0 - GREEN otherwise
   */
  protected ArtifactState getStatus(final SentiloArtifactMetrics artifactMetrics) {
    final String remoteServerConnectionState = getRemoteServerConnectionState();
    if (STATUS_KO.equals(remoteServerConnectionState)) {
      return ArtifactState.RED;
    } else if (currentInputEvents == 0) {
      return ArtifactState.ORANGE;
    } else {
      return ArtifactState.GREEN;
    }
  }

  @SuppressWarnings("unchecked")
  private Pool<Jedis> getRedisPool() {
    if (jedisConnectionFactory != null && pool == null) {
      final Field poolField = ReflectionUtils.findField(JedisConnectionFactory.class, "pool");
      ReflectionUtils.makeAccessible(poolField);
      pool = (Pool<Jedis>) ReflectionUtils.getField(poolField, jedisConnectionFactory);
    }

    return pool;
  }
}
