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
package org.sentilo.web.catalog.metrics;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.atomic.AtomicLong;

import org.sentilo.common.metrics.SentiloArtifactMetrics;
import org.sentilo.common.metrics.service.impl.SentiloArtifactMetricsServiceImpl;
import org.sentilo.web.catalog.service.PlatformService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class CatalogMetricsServiceImpl extends SentiloArtifactMetricsServiceImpl implements CatalogMetricsCounter {

  public static final String REQUESTS_METRICS = "requests";
  public static final String REQUESTS_TOTAL_FIELD = "total";
  public static final String REQUESTS_TOTAL_BY_STATUS_FIELD = "total_by_status";

  @Autowired
  private PlatformService platformService;

  private ConcurrentMap<Integer, Integer> statusMetric = new ConcurrentHashMap<Integer, Integer>();
  private AtomicLong total = new AtomicLong();

  private Long prevTotalRequests = new Long(0);
  private Map<Integer, Integer> prevRequestCounters = new HashMap<Integer, Integer>();

  public void incrementTotalRequests() {
    total.incrementAndGet();
  }

  public void incrementStatusRequests(final Integer status) {
    final Integer statusCount = statusMetric.get(status);

    if (statusCount == null) {
      statusMetric.put(status, 1);
    } else {
      statusMetric.put(status, statusCount + 1);
    }
  }

  @Override
  protected Map<String, Object> collectCustomMetrics() {
    final Map<String, Object> catalogCustomMetrics = new HashMap<String, Object>();

    // Catalog custom metrics contains requests counters, grouped by response status code
    final Map<String, Object> requestsMetrics = new HashMap<String, Object>();
    requestsMetrics.put(REQUESTS_TOTAL_FIELD, deltaTotalRequests());
    requestsMetrics.put(REQUESTS_TOTAL_BY_STATUS_FIELD, collectRequestCounters());

    catalogCustomMetrics.put(REQUESTS_METRICS, requestsMetrics);

    return catalogCustomMetrics;
  }

  protected void saveArtifactMetrics(final SentiloArtifactMetrics catalogMetrics) {
    platformService.saveCatalogMetrics(catalogMetrics);
  }

  @Override
  protected String getName() {
    return "catalog-web";
  }

  @Override
  protected ArtifactState getStatus(final SentiloArtifactMetrics artifactMetrics) {
    return ArtifactState.GREEN;
  }

  private Long deltaTotalRequests() {
    final Long totalRequests = total.get();
    final Long deltaTotalRequests = totalRequests - prevTotalRequests;
    prevTotalRequests = totalRequests;
    return deltaTotalRequests;
  }

  private Map<Integer, Integer> collectRequestCounters() {
    final Map<Integer, Integer> deltaRequestCounters = new HashMap<Integer, Integer>();
    final Map<Integer, Integer> requestStatusCounters = statusMetric;

    for (final Integer status : requestStatusCounters.keySet()) {
      final Integer prevCount = prevRequestCounters.containsKey(status) ? prevRequestCounters.get(status) : Integer.valueOf(0);
      deltaRequestCounters.put(status, requestStatusCounters.get(status) - prevCount);
    }

    prevRequestCounters.clear();
    prevRequestCounters.putAll(requestStatusCounters);

    return deltaRequestCounters;
  }

}
