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
package org.sentilo.web.catalog.test.metrics;

import static org.mockito.Matchers.any;
import static org.mockito.Mockito.verify;

import java.util.Map;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.metrics.SentiloArtifactMetrics;
import org.sentilo.web.catalog.interceptor.MetricsInterceptor;
import org.sentilo.web.catalog.metrics.CatalogMetricsServiceImpl;
import org.sentilo.web.catalog.service.PlatformService;
import org.springframework.util.CollectionUtils;

import com.google.common.collect.ImmutableMap;

public class CatalogMetricsServiceImplTest {

  @Mock
  private PlatformService platformService;

  @Mock
  private MetricsInterceptor metricsInterceptor;

  @InjectMocks
  private CatalogMetricsServiceImpl service;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void saveArtifactMetrics() {
    service.collectAndSave();

    verify(platformService).saveCatalogMetrics(any());
  }

  @SuppressWarnings("unchecked")
  @Test
  public void collectCustomMetrics() {
    final Map<Integer, Integer> statusResponses_1 = ImmutableMap.of(200, 4, 404, 2, 500, 1, 401, 3);
    final Map<Integer, Integer> statusResponses_2 = ImmutableMap.of(200, 10, 404, 4, 500, 2, 401, 5);

    // First iteration
    simulateRequestAndResponse(statusResponses_1);
    service.collect();

    // Second iteration: to validate correct delta values returned by metrics
    simulateRequestAndResponse(statusResponses_2);
    final SentiloArtifactMetrics catalogMetrics = service.collect();

    final Map<String, Object> requestsMetrics = (Map<String, Object>) catalogMetrics.getMetrics().get("requests");
    final Map<Integer, Integer> totalRequestsByStatus = (Map<Integer, Integer>) requestsMetrics.get("total_by_status");
    Assert.assertTrue(catalogMetrics.getName().endsWith("catalog-web:metrics"));
    Assert.assertFalse(CollectionUtils.isEmpty(requestsMetrics));
    Assert.assertEquals(21L, requestsMetrics.get("total"));
    Assert.assertFalse(CollectionUtils.isEmpty(totalRequestsByStatus));
    Assert.assertEquals(Integer.valueOf(10), totalRequestsByStatus.get(200));
    Assert.assertEquals(Integer.valueOf(4), totalRequestsByStatus.get(404));
    Assert.assertEquals(Integer.valueOf(2), totalRequestsByStatus.get(500));
    Assert.assertEquals(Integer.valueOf(5), totalRequestsByStatus.get(401));

  }

  private void simulateRequestAndResponse(final Map<Integer, Integer> statusResponses) {
    for (final Integer status : statusResponses.keySet()) {
      final Integer requests = statusResponses.get(status);
      for (int i = 0; i < requests; i++) {
        service.incrementTotalRequests();
        service.incrementStatusRequests(status);
      }
    }
  }

}
