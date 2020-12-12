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
package org.sentilo.platform.server.metrics;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.sentilo.common.domain.PlatformActivity;
import org.sentilo.common.metrics.SentiloArtifactMetrics;
import org.sentilo.common.metrics.service.impl.SentiloArtifactMetricsServiceImpl;
import org.sentilo.platform.server.http.RequestListenerThread;
import org.sentilo.platform.server.pool.ThreadPool;
import org.sentilo.platform.service.monitor.CounterService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.connection.jedis.JedisConnectionFactory;
import org.springframework.stereotype.Component;
import org.springframework.util.ReflectionUtils;

import redis.clients.jedis.Jedis;
import redis.clients.util.Pool;

@Component
public class ApiServerMetricsServiceImpl extends SentiloArtifactMetricsServiceImpl {

  public static final String THREAD_POOL_METRICS = "thread_pool";
  public static final String TH_POOL_LISTENING = "is_listening";
  public static final String TH_POOL_IS_SHUTDOWN = "is_shutdown";
  public static final String TH_POOL_CORE_SIZE = "core_size";
  public static final String TH_POOL_MAX_SIZE = "max_size";
  public static final String TH_POOL_SIZE = "size";
  public static final String TH_POOL_ACTIVE = "active";
  public static final String TH_POOL_QUEUED = "queued";
  public static final String TH_POOL_COMPLETED = "completed";

  public static final String REQUESTS_METRICS = "requests";
  public static final String REQUESTS_INSTANCE_FIELD = "instance";
  public static final String REQUESTS_REQUESTS_FIELD = "requests";
  public static final String REQUESTS_TOTAL_FIELD = "total";
  public static final String REQUESTS_GET_FIELD = "get";
  public static final String REQUESTS_PUT_FIELD = "put";
  public static final String REQUESTS_PUSH_FIELD = "push";
  public static final String REQUESTS_GLOBAL_FIELD = "global";
  public static final String REQUESTS_DATA_FIELD = "data";
  public static final String REQUESTS_ALARM_FIELD = "alarm";
  public static final String REQUESTS_ORDER_FIELD = "order";

  /**
   * Stores in memory previous tenant counters: these counters allow us to calculate the requests at
   * the current iteration
   */
  private Map<String, PlatformActivity> previousTenantsCounters = new HashMap<String, PlatformActivity>();

  @Autowired
  private CounterService counterService;

  @Autowired
  private RequestListenerThread listenerThread;

  @Autowired
  private JedisConnectionFactory jedisConnectionFactory;

  private Pool<Jedis> redisPoolLib;

  @Override
  protected String getName() {
    return "api-server";
  }

  @Override
  protected Map<String, Object> collectCustomMetrics() {
    final Map<String, Object> metrics = new HashMap<String, Object>();
    metrics.put(REQUESTS_METRICS, collectCurrentRequestMetrics());
    metrics.put(THREAD_POOL_METRICS, collectThreadPoolMetrics());
    metrics.put(REDIS_POOL_METRICS, collectRedisPoolMetrics());

    return metrics;
  }

  /**
   * Returns current request metrics, i.e requests received by the platform from the previous
   * collect, grouped by event type
   * 
   * @return current request metrics
   */
  protected List<Object> collectCurrentRequestMetrics() {
    final List<Object> rqMetrics = new ArrayList<Object>();

    final List<PlatformActivity> currentTenantsCounters = counterService.getTenantCounters();
    // The first time collect is executed, previous counters not exist so this step is skipped to
    // evict collect only total counters
    // and no partial counters.
    if (!previousTenantsCounters.isEmpty()) {
      for (final PlatformActivity activity : currentTenantsCounters) {
        final PlatformActivity prevActivity = getPreviousTenantCounters(activity.getName());
        final Map<String, Object> tenantCounters = collectCurrentTenantCounters(activity, prevActivity);
        rqMetrics.add(tenantCounters);
      }
    }
    replacePreviousTenantsCounters(currentTenantsCounters);

    return rqMetrics;
  }

  @Override
  protected ArtifactState getStatus(final SentiloArtifactMetrics artifactMetrics) {
    return ArtifactState.GREEN;
  }

  private Map<String, Object> collectThreadPoolMetrics() {
    final Map<String, Object> threadPoolMetrics = new HashMap<String, Object>();

    // Metrics to collect from the connection thread pool are pool size, active threads, completed
    // tasks, queued tasks, ..
    final ThreadPool threadPool = listenerThread.getThreadPool();
    final boolean isListening = listenerThread.isListening();
    final boolean isShutdown = threadPool.getThreadPoolExecutor().isShutdown();
    final int corePoolSize = threadPool.getThreadPoolExecutor().getCorePoolSize();
    final int maximumPoolSize = threadPool.getThreadPoolExecutor().getMaximumPoolSize();
    final int activeThreads = threadPool.getThreadPoolExecutor().getActiveCount();
    final int queuedTasks = threadPool.getThreadPoolExecutor().getQueue().size();
    final long completedTasks = threadPool.getThreadPoolExecutor().getCompletedTaskCount();
    final int poolSize = threadPool.getThreadPoolExecutor().getPoolSize();

    threadPoolMetrics.put(TH_POOL_LISTENING, isListening);
    threadPoolMetrics.put(TH_POOL_IS_SHUTDOWN, isShutdown);
    threadPoolMetrics.put(TH_POOL_CORE_SIZE, corePoolSize);
    threadPoolMetrics.put(TH_POOL_MAX_SIZE, maximumPoolSize);
    threadPoolMetrics.put(TH_POOL_SIZE, poolSize);
    threadPoolMetrics.put(TH_POOL_ACTIVE, activeThreads);
    threadPoolMetrics.put(TH_POOL_QUEUED, queuedTasks);
    threadPoolMetrics.put(TH_POOL_COMPLETED, completedTasks);

    return threadPoolMetrics;
  }

  private Map<String, Object> collectCurrentTenantCounters(final PlatformActivity activity, final PlatformActivity prevActivity) {
    final Map<String, Object> tenantCounters = new HashMap<String, Object>();
    final Map<String, Object> requestTenantCounters = new HashMap<String, Object>();
    tenantCounters.put(REQUESTS_INSTANCE_FIELD, activity.getName());
    tenantCounters.put(REQUESTS_REQUESTS_FIELD, requestTenantCounters);
    requestTenantCounters.put(REQUESTS_GLOBAL_FIELD, collectGlobalCounters(activity, prevActivity));
    requestTenantCounters.put(REQUESTS_DATA_FIELD, collectDataCounters(activity, prevActivity));
    requestTenantCounters.put(REQUESTS_ALARM_FIELD, collectAlarmCounters(activity, prevActivity));
    requestTenantCounters.put(REQUESTS_ORDER_FIELD, collectOrderCounters(activity, prevActivity));

    return tenantCounters;
  }

  private void replacePreviousTenantsCounters(final List<PlatformActivity> currentTenantsCounters) {
    previousTenantsCounters.clear();
    for (final PlatformActivity activity : currentTenantsCounters) {
      previousTenantsCounters.put(activity.getName(), activity);
    }
  }

  private PlatformActivity getPreviousTenantCounters(final String name) {
    return previousTenantsCounters.containsKey(name) ? previousTenantsCounters.get(name) : new PlatformActivity();
  }

  private Map<String, Object> collectGlobalCounters(final PlatformActivity activity, final PlatformActivity prevActivity) {
    final Map<String, Object> counters = new HashMap<String, Object>();
    counters.put(REQUESTS_TOTAL_FIELD, activity.getTotalRequests() - prevActivity.getTotalRequests());
    counters.put(REQUESTS_GET_FIELD, activity.getTotalGetRequests() - prevActivity.getTotalGetRequests());
    counters.put(REQUESTS_PUT_FIELD, activity.getTotalPutRequests() - prevActivity.getTotalPutRequests());
    counters.put(REQUESTS_PUSH_FIELD, activity.getTotalPushRequests() - prevActivity.getTotalPushRequests());
    return counters;
  }

  private Map<String, Object> collectDataCounters(final PlatformActivity activity, final PlatformActivity prevActivity) {
    final Map<String, Object> counters = new HashMap<String, Object>();
    counters.put(REQUESTS_TOTAL_FIELD, activity.getTotalObs() - prevActivity.getTotalObs());
    counters.put(REQUESTS_GET_FIELD, activity.getTotalGetObs() - prevActivity.getTotalGetObs());
    counters.put(REQUESTS_PUT_FIELD, activity.getTotalPutObs() - prevActivity.getTotalPutObs());
    counters.put(REQUESTS_PUSH_FIELD, activity.getTotalPushObs() - prevActivity.getTotalPushObs());
    return counters;
  }

  private Map<String, Object> collectOrderCounters(final PlatformActivity activity, final PlatformActivity prevActivity) {
    final Map<String, Object> counters = new HashMap<String, Object>();
    counters.put(REQUESTS_TOTAL_FIELD, activity.getTotalOrders() - prevActivity.getTotalOrders());
    counters.put(REQUESTS_GET_FIELD, activity.getTotalGetOrders() - prevActivity.getTotalGetOrders());
    counters.put(REQUESTS_PUT_FIELD, activity.getTotalPutOrders() - prevActivity.getTotalPutOrders());
    counters.put(REQUESTS_PUSH_FIELD, activity.getTotalPushOrders() - prevActivity.getTotalPushOrders());
    return counters;
  }

  private Map<String, Object> collectAlarmCounters(final PlatformActivity activity, final PlatformActivity prevActivity) {
    final Map<String, Object> counters = new HashMap<String, Object>();
    counters.put(REQUESTS_TOTAL_FIELD, activity.getTotalAlarms() - prevActivity.getTotalAlarms());
    counters.put(REQUESTS_GET_FIELD, activity.getTotalGetAlarms() - prevActivity.getTotalGetAlarms());
    counters.put(REQUESTS_PUT_FIELD, activity.getTotalPutAlarms() - prevActivity.getTotalPutAlarms());
    counters.put(REQUESTS_PUSH_FIELD, activity.getTotalPushAlarms() - prevActivity.getTotalPushAlarms());
    return counters;
  }

  private Map<String, Object> collectRedisPoolMetrics() {
    final Map<String, Object> redisPoolMetrics = new HashMap<String, Object>();

    redisPoolMetrics.put(REDIS_POOL_NUM_ACTIVE, getRedisPool().getNumActive());
    redisPoolMetrics.put(REDIS_POOL_NUM_IDLE, getRedisPool().getNumIdle());
    redisPoolMetrics.put(REDIS_POOL_NUM_WAITERS, getRedisPool().getNumWaiters());
    redisPoolMetrics.put(REDIS_POOL_MAX_BORROW_TS, getRedisPool().getMaxBorrowWaitTimeMillis());
    redisPoolMetrics.put(REDIS_POOL_MEAN_BORROW_TS, getRedisPool().getMeanBorrowWaitTimeMillis());

    return redisPoolMetrics;
  }

  @SuppressWarnings("unchecked")
  private Pool<Jedis> getRedisPool() {
    if (jedisConnectionFactory != null && redisPoolLib == null) {
      final Field poolField = ReflectionUtils.findField(JedisConnectionFactory.class, "pool");
      ReflectionUtils.makeAccessible(poolField);
      redisPoolLib = (Pool<Jedis>) ReflectionUtils.getField(poolField, jedisConnectionFactory);
    }

    return redisPoolLib;
  }
}
