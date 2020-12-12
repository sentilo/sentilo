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
package org.sentilo.agent.metrics.monitor.test.repository;

import static org.mockito.Matchers.argThat;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentMatcher;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.agent.metrics.monitor.repository.batch.BatchProcessContext;
import org.sentilo.agent.metrics.monitor.repository.batch.BatchProcessResult;
import org.sentilo.agent.metrics.monitor.repository.batch.BatchProcessWorker;
import org.sentilo.common.exception.RESTClientException;
import org.sentilo.common.rest.RESTClient;
import org.sentilo.common.rest.RequestContext;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.util.CollectionUtils;

public class BatchProcessWorkerTest {

  @Mock
  private BatchProcessContext context;

  @Mock
  private RESTClient restClient;

  private BatchProcessWorker worker;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    when(context.getRestClient()).thenReturn(restClient);
    when(context.getNumMaxRetries()).thenReturn(3);
    when(context.getMetricsToProcess()).thenReturn(buildMockList(50));

    worker = new BatchProcessWorker(context);
  }

  @Test
  public void customParams() {
    final String index = "metrics-sentilo";
    final String pattern = "{now/M}";

    System.setProperty("elasticsearch.index.name", index);
    System.setProperty("elasticsearch.index.date.pattern", pattern);

    final BatchProcessWorker worker2 = new BatchProcessWorker(context);

    Assert.assertEquals("<" + index + "-" + pattern + ">", ReflectionTestUtils.getField(worker2, "indexName"));
  }

  @Test
  public void call() {
    final String response = "{\"errors\":false,\"took\":3}";
    when(restClient.post(argThat(new PathRequestContextMatcher()))).thenReturn(response);

    final BatchProcessResult result = worker.call();

    verify(restClient).post(argThat(new PathRequestContextMatcher()));
    Assert.assertTrue(CollectionUtils.isEmpty(result.getPendingMetrics()));
  }

  @SuppressWarnings("unchecked")
  @Test
  public void callWithErrors() {
    when(restClient.post(argThat(new PathRequestContextMatcher()))).thenThrow(RESTClientException.class);

    final BatchProcessResult result = worker.call();

    verify(restClient, times(context.getNumMaxRetries())).post(argThat(new PathRequestContextMatcher()));
    Assert.assertFalse(CollectionUtils.isEmpty(result.getPendingMetrics()));
    Assert.assertTrue(result.getPendingMetrics().size() == context.getMetricsToProcess().size());
  }

  @Test
  public void callWithPartialErrors() {
    final String response = "{\"errors\":true,\"took\":3,\"items\":[{\"create\":{\"status\":404}}]}";
    when(restClient.post(argThat(new PathRequestContextMatcher()))).thenReturn(response);

    final BatchProcessResult result = worker.call();

    verify(restClient, times(context.getNumMaxRetries())).post(argThat(new PathRequestContextMatcher()));
    Assert.assertFalse(CollectionUtils.isEmpty(result.getPendingMetrics()));
    Assert.assertTrue(result.getPendingMetrics().size() == 1);
  }

  private List<String> buildMockList(final long total) {
    final List<String> resources = new ArrayList<String>();
    for (int i = 0; i < total; i++) {
      final String metrics = new String("metrics");
      resources.add(metrics);
    }

    return resources;
  }

  class PathRequestContextMatcher extends ArgumentMatcher<RequestContext> {

    @Override
    public boolean matches(final Object argument) {
      final RequestContext rc = (RequestContext) argument;
      return "/_bulk".equals(rc.getPath());
    }

  }

}
