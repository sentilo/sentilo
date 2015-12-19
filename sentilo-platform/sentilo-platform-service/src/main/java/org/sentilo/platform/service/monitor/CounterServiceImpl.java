/*
 * Sentilo
 *  
 * Original version 1.4 Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de Barcelona.
 * Modified by Opentrends adding support for multitenant deployments and SaaS. Modifications on version 1.5 Copyright (C) 2015 Opentrends Solucions i Sistemes, S.L.
 * 
 *   
 * This program is licensed and may be used, modified and redistributed under the
 * terms  of the European Public License (EUPL), either version 1.1 or (at your 
 * option) any later version as soon as they are approved by the European 
 * Commission.
 *   
 * Alternatively, you may redistribute and/or modify this program under the terms
 * of the GNU Lesser General Public License as published by the Free Software 
 * Foundation; either  version 3 of the License, or (at your option) any later 
 * version. 
 *   
 * Unless required by applicable law or agreed to in writing, software distributed
 * under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR 
 * CONDITIONS OF ANY KIND, either express or implied. 
 *   
 * See the licenses for the specific language governing permissions, limitations 
 * and more details.
 *   
 * You should have received a copy of the EUPL1.1 and the LGPLv3 licenses along 
 * with this program; if not, you may find them at: 
 *   
 *   https://joinup.ec.europa.eu/software/page/eupl/licence-eupl
 *   http://www.gnu.org/licenses/ 
 *   and 
 *   https://www.gnu.org/licenses/lgpl.txt
 */
package org.sentilo.platform.service.monitor;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.sentilo.common.domain.PlatformActivity;
import org.sentilo.common.utils.EventType;
import org.sentilo.platform.service.monitor.MonitorConstants.MetricKeyType;
import org.sentilo.platform.service.utils.PubSubConstants;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

@Service
public class CounterServiceImpl extends AbstractMetricsServiceImpl implements CounterService {

  /*
   * (non-Javadoc)
   * 
   * @see
   * org.sentilo.platform.service.monitor.CounterService#save(org.sentilo.platform.service.monitor
   * .CounterContext)
   */
  public void save(final CounterContext context) {
    // Write to Redis the metric counts: master, entity and, optionally, tenant counts
    // If request type is PUSH, i.e. is Sentilo who initiates the request, then requests counts
    // mustn't be incremented.

    // TODO : either batch update to improve performance when load is higher
    // or synchonize call to evict ghost reads
    incrementMasterCounts(context);
    incrementEntityCounts(context);
    if (StringUtils.hasText(context.getTenant())) {
      incrementTenantCounts(context);
    }
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.sentilo.platform.service.monitor.CounterService#getTenantCounters()
   */
  public List<PlatformActivity> getTenantCounters() {
    final List<PlatformActivity> globalPlatformActivity = new ArrayList<PlatformActivity>();
    final Set<String> counters = redisTemplate.keys(MonitorConstants.TENANT_COUNTERS_KEYS_PATTERN);
    counters.add(MonitorConstants.MASTER_COUNTERS_KEY);
    for (final String counterKey : counters) {
      globalPlatformActivity.add(getPlatformActivity(counterKey));
    }

    return globalPlatformActivity;
  }

  private PlatformActivity getPlatformActivity(final String counterKey) {
    final Map<Object, Object> countersHash = getHashContent(counterKey);

    final boolean isMaster = (MonitorConstants.MASTER_COUNTERS_KEY.equals(counterKey));
    final String tenant = (isMaster ? null : counterKey.split(PubSubConstants.REDIS_KEY_TOKEN)[2]);
    final Long totalRequests = getHashValueAsLong(countersHash, MonitorConstants.TOTAL_REQUESTS_FIELD);
    final Long totalObs = addHashValues(countersHash, EventType.DATA);
    final Long totalOrders = addHashValues(countersHash, EventType.ORDER);
    final Long totalAlarms = addHashValues(countersHash, EventType.ALARM);

    return new PlatformActivity(tenant, totalRequests, totalObs, totalOrders, totalAlarms, System.currentTimeMillis(), isMaster);
  }

  private Long addHashValues(final Map<Object, Object> hash, final EventType eventType) {
    final Long value1 = getHashValueAsLong(hash, buildHashField(RequestType.GET, eventType));
    final Long value2 = getHashValueAsLong(hash, buildHashField(RequestType.PUT, eventType));

    return value1 + value2;
  }

  protected void incrementMasterCounts(final CounterContext context) {
    final String hashKey = buildHashKey(context, MetricKeyType.MASTER);
    incrementCounter(hashKey, buildHashField(context), context.getTotal());
    if (context.getRequestType() != RequestType.PUSH) {
      incrementRequestsCount(context, hashKey);
    }
  }

  protected void incrementEntityCounts(final CounterContext context) {
    final String hashKey = buildHashKey(context, MetricKeyType.ENTITY);
    incrementCounter(hashKey, buildHashField(context), context.getTotal());
    if (context.getRequestType() != RequestType.PUSH) {
      incrementRequestsCount(context, hashKey);
    }
  }

  protected void incrementTenantCounts(final CounterContext context) {
    final String hashKey = buildHashKey(context, MetricKeyType.TENANT);
    incrementCounter(hashKey, buildHashField(context), context.getTotal());
    if (context.getRequestType() != RequestType.PUSH) {
      incrementRequestsCount(context, hashKey);
    }
  }

  protected void incrementRequestsCount(final CounterContext context, final String hashKey) {
    String hashFieldKey = buildHashField(MonitorConstants.TOTAL_REQUESTS_FIELD, context.getRequestType().name().toLowerCase());
    incrementCounter(hashKey, hashFieldKey, 1);
    incrementCounter(hashKey, MonitorConstants.TOTAL_REQUESTS_FIELD, 1);
  }

  protected void incrementCounter(final String hashKey, final String hashField, final int total) {
    redisTemplate.opsForHash().increment(hashKey, hashField, total);
  }

  private String buildHashKey(final CounterContext context, final MetricKeyType countKeyType) {
    final StringBuilder sb = new StringBuilder(MonitorConstants.COUNTERS_PREFIX + PubSubConstants.REDIS_KEY_TOKEN);
    switch (countKeyType) {
      case MASTER:
        sb.append(MetricKeyType.MASTER.name());
        break;
      case ENTITY:
        sb.append(MetricKeyType.ENTITY.name());
        sb.append(PubSubConstants.REDIS_KEY_TOKEN);
        sb.append(context.getEntity());
        break;
      case TENANT:
        sb.append(MetricKeyType.TENANT.name());
        sb.append(PubSubConstants.REDIS_KEY_TOKEN);
        sb.append(context.getTenant());
        break;
      default:
        break;
    }

    return sb.toString().toLowerCase();
  }

  private String buildHashField(final CounterContext context) {
    return buildHashField(context.getRequestType(), context.getDataType());
  }

  private String buildHashField(final RequestType requestType, final EventType eventType) {
    final String sRequestType = requestType.name().toLowerCase();
    final String sEventType = eventType.name().toLowerCase();

    return buildHashField(sEventType, sRequestType);
  }

  private String buildHashField(final String prefix, final String sRequestType) {
    return prefix + "_" + sRequestType;
  }

}
