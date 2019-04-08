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
package org.sentilo.platform.service.monitor;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import org.sentilo.common.domain.PlatformActivity;
import org.sentilo.common.domain.PlatformPerformance;
import org.sentilo.common.utils.DateUtils;
import org.sentilo.platform.service.monitor.MonitorConstants.MetricKeyType;
import org.sentilo.platform.service.utils.PubSubConstants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;

@Service
public class MetricServiceImpl extends AbstractMetricsServiceImpl implements MetricService {

  private static final Logger LOGGER = LoggerFactory.getLogger(MetricServiceImpl.class);

  private final Lock lock = new ReentrantLock();

  @Autowired
  private CounterService counterServiceImpl;

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.platform.service.monitor.MetricService#getGlobalPerformance()
   */
  public Collection<PlatformPerformance> getGlobalPerformance() {
    LOGGER.debug("Getting the platform global performance");
    final Collection<PlatformActivity> tenantPlatformActivity = counterServiceImpl.getTenantCounters();
    final Collection<PlatformPerformance> globalPerformance = new ArrayList<PlatformPerformance>();
    for (final PlatformActivity activity : tenantPlatformActivity) {
      globalPerformance.add(getPerformance(activity));
    }

    LOGGER.debug("Returning the current performance for {} sites", globalPerformance.size());
    return globalPerformance;
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.platform.service.monitor.MetricService#computeGlobalPerformance()
   */
  @Scheduled(initialDelay = 10000, fixedRate = 30000)
  public void computeGlobalPerformance() {
    // Compute the current performance for the master instance
    // and for each tenant registered into it.
    LOGGER.debug("Init the process to compute the current global performance");
    final long currentTs = System.currentTimeMillis();
    final Set<String> counters = getCountersKeys();

    for (final String counterKey : counters) {
      computePerformance(counterKey, currentTs);
    }
    LOGGER.debug("Finished process. Running time (millis) = {}", System.currentTimeMillis() - currentTs);
  }

  /**
   * Get the performance stored in Redis and associated with the site fixed by the activity
   * attribute
   *
   * @param activity
   * @return
   */
  private PlatformPerformance getPerformance(final PlatformActivity activity) {
    final String avgsHashKey = buildAvgsHashKey(activity);
    final Map<Object, Object> avgsHash = getAvgsHash(avgsHashKey);

    final float instantAvg = Float.valueOf(getHashValue(avgsHash, MonitorConstants.INSTANT_AVG_FIELD, "0"));
    final float maxDailyAvg = Float.valueOf(getHashValue(avgsHash, MonitorConstants.MAX_DAILY_AVG_FIELD, "0"));
    final float maxAvg = Float.valueOf(getHashValue(avgsHash, MonitorConstants.MAX_AVG_FIELD, "0"));
    final long ts = Long.valueOf(getHashValue(avgsHash, MonitorConstants.TIMESTAMP_FIELD, "0"));

    return new PlatformPerformance(activity, instantAvg, maxDailyAvg, maxAvg, ts);
  }

  /**
   * Computes the performance's metrics for the site related to the key <code>counterKey</code>
   *
   * @param counterKey Redis key of a counter hash related to a Sentilo site (either master or
   *        tenant site)
   */
  private void computePerformance(final String counterKey, final Long currentTs) {
    final String avgsHashKey = buildAvgHashKey(counterKey);
    // First step is to get the counters stored in Redis, more particularly the requests field
    final String totalRequests = getFieldValue(counterKey, MonitorConstants.TOTAL_REQUESTS_FIELD);
    // At second place is to get the last avg stored in Redis
    final Map<Object, Object> avgsHash = getAvgsHash(avgsHashKey);

    // Finally, compute new instantAvg.
    final Long prevTotalRequests = Long.valueOf(getHashValue(avgsHash, MonitorConstants.TOTAL_REQUESTS_FIELD, "0"));
    final long prevTs = Long.parseLong(getHashValue(avgsHash, MonitorConstants.TIMESTAMP_FIELD, "0"));
    final long deltaTsSeconds = (currentTs - prevTs) / 1000;
    final Float instantAvg = computeInstantAvg(Long.parseLong(totalRequests == null ? "0" : totalRequests), prevTotalRequests, deltaTsSeconds);
    // Compare with max and daily avgs
    upgradeAvgsIfNeedBe(avgsHash, instantAvg, currentTs, prevTs);
    // and stored it
    storeAvgsHash(avgsHashKey, avgsHash, instantAvg, totalRequests == null ? "0" : totalRequests, currentTs);

  }

  private void storeAvgsHash(final String avgsHashKey, final Map<Object, Object> avgsHash, final Float instantAvg, final Object totalRequests,
      final long currentTs) {
    lock.lock();
    try {
      avgsHash.put(MonitorConstants.INSTANT_AVG_FIELD, Float.toString(instantAvg));
      avgsHash.put(MonitorConstants.TOTAL_REQUESTS_FIELD, totalRequests);
      avgsHash.put(MonitorConstants.TIMESTAMP_FIELD, Long.toString(currentTs));
      redisTemplate.opsForHash().putAll(avgsHashKey, avgsHash);
    } finally {
      lock.unlock();
    }
  }

  private Map<Object, Object> getAvgsHash(final String avgHashKey) {
    lock.lock();
    try {
      final Map<Object, Object> avgsHash = getHashContent(avgHashKey);
      return avgsHash == null ? new HashMap<Object, Object>() : avgsHash;
    } finally {
      lock.unlock();
    }
  }

  private void upgradeAvgsIfNeedBe(final Map<Object, Object> avgsHash, final Float instantAvg, final long currentTs, final long prevTs) {
    final Float maxDailyAvg = Float.valueOf(getHashValue(avgsHash, MonitorConstants.MAX_DAILY_AVG_FIELD, "0f"));
    final Float maxAvg = Float.valueOf(getHashValue(avgsHash, MonitorConstants.MAX_AVG_FIELD, "0f"));

    // 1. Upgrade max avg if necessary
    if (instantAvg > maxAvg) {
      avgsHash.put(MonitorConstants.MAX_AVG_FIELD, Float.toString(instantAvg));
      avgsHash.put(MonitorConstants.TIMESTAMP_MAX_AVG_FIELD, Long.toString(currentTs));
    }

    // 2. Perform daily average comparison
    final boolean sameDay = DateUtils.sameDay(prevTs, currentTs);

    if (!sameDay || sameDay && instantAvg > maxDailyAvg) {
      avgsHash.put(MonitorConstants.MAX_DAILY_AVG_FIELD, Float.toString(instantAvg));
    }
  }

  private Float computeInstantAvg(final Long totalRequests, final Long prevTotalRequests, final Long deltaTs) {
    final Long delta = prevTotalRequests != 0 ? totalRequests - prevTotalRequests : new Long(0);

    final BigDecimal eventsPerSecond = new BigDecimal(delta);
    return delta == 0 ? new Float(0) : eventsPerSecond.divide(new BigDecimal(deltaTs), 2, BigDecimal.ROUND_HALF_EVEN).floatValue();
  }

  private String buildAvgHashKey(final String counterHashKey) {
    return counterHashKey.replaceAll(MonitorConstants.COUNTERS_PREFIX, MonitorConstants.AVERAGES_PREFIX);
  }

  private String buildAvgsHashKey(final PlatformActivity activity) {
    final StringBuilder sb = new StringBuilder(MonitorConstants.AVERAGES_PREFIX);
    sb.append(PubSubConstants.REDIS_KEY_TOKEN);

    if (activity.isMaster()) {
      sb.append(MetricKeyType.master.name());
    } else {
      sb.append(MetricKeyType.tenant.name());
      sb.append(PubSubConstants.REDIS_KEY_TOKEN);
      sb.append(activity.getTenant());
    }

    return sb.toString();
  }

}
