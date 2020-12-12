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
package org.sentilo.web.catalog.test.service;

import static org.mockito.Matchers.argThat;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentMatcher;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.exception.RESTClientException;
import org.sentilo.common.metrics.SentiloArtifactMetrics;
import org.sentilo.common.rest.RESTClient;
import org.sentilo.common.rest.RequestContext;
import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.web.catalog.domain.PlatformAdminInputMessage;
import org.sentilo.web.catalog.service.impl.PlatformServiceImpl;

public class PlatformServiceImplTest {

  @Mock
  private RESTClient restClient;

  @InjectMocks
  private PlatformServiceImpl service;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void getCurrentPlatformStats() {
    service.getCurrentPlatformStats();

    verify(restClient).get(argThat(new PathRequestContextMatcher("admin/stats")));
  }

  @Test
  public void getPlatformActivity() {
    service.getPlatformActivity();

    verify(restClient).get(argThat(new PathRequestContextMatcher("admin/activity")));
  }

  @Test
  public void getPlatformPerformance() {
    service.getPlatformPerformance();

    verify(restClient).get(argThat(new PathRequestContextMatcher("admin/performance")));
  }

  @Test
  public void saveResources() {
    final PlatformAdminInputMessage paim = new PlatformAdminInputMessage();
    service.saveResources(paim);

    verify(restClient).post(argThat(new PathRequestContextMatcher("admin/save")));
  }

  @Test
  public void deleteResources() {
    final PlatformAdminInputMessage paim = new PlatformAdminInputMessage();
    service.deleteResources(paim);

    verify(restClient).put(argThat(new PathRequestContextMatcher("admin/delete")));
  }

  @Test
  public void getActiveSubscriptions() {
    final String entityId = "mockId";
    final String path = String.format("admin/subscriptions/%s", entityId);
    service.getActiveSubscriptions(entityId);

    verify(restClient).get(argThat(new PathRequestContextMatcher(path)));
  }

  @Test
  public void saveCatalogMetrics() {
    final SentiloArtifactMetrics catalogMetrics = new SentiloArtifactMetrics(SentiloConstants.DEFAULT_CATALOG_ID);
    when(restClient.get(argThat(new PathRequestContextMatcher("admin/ping")))).thenReturn("pong");

    service.saveCatalogMetrics(catalogMetrics);

    verify(restClient).post(argThat(new PathRequestContextMatcher("admin/metrics")));
  }

  @SuppressWarnings("unchecked")
  @Test
  public void saveCatalogMetrics_whenPlatformIsKo() {
    final SentiloArtifactMetrics catalogMetrics = new SentiloArtifactMetrics(SentiloConstants.DEFAULT_CATALOG_ID);
    when(restClient.get(argThat(new PathRequestContextMatcher("admin/ping")))).thenThrow(RESTClientException.class);

    service.saveCatalogMetrics(catalogMetrics);

    verify(restClient, times(0)).post(argThat(new PathRequestContextMatcher("admin/metrics")));
  }

  @SuppressWarnings("unchecked")
  @Test
  public void getMetrics() {

    service.getMetrics();

    verify(restClient).get(argThat(new PathRequestContextMatcher("admin/metrics")));
  }

  class PathRequestContextMatcher extends ArgumentMatcher<RequestContext> {

    final String path;

    public PathRequestContextMatcher(final String path) {
      this.path = path;
    }

    @Override
    public boolean matches(final Object argument) {
      final RequestContext rc = (RequestContext) argument;
      return path.equals(rc.getPath());
    }

  }

}
