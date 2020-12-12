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
package org.sentilo.platform.server.test.metrics;

import static org.mockito.Mockito.when;

import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.metrics.SentiloArtifactMetrics;
import org.sentilo.platform.server.http.RequestListenerThread;
import org.sentilo.platform.server.metrics.ApiServerMetricsServiceImpl;
import org.sentilo.platform.server.pool.ThreadPool;
import org.sentilo.platform.service.monitor.CounterService;

import redis.clients.jedis.Jedis;
import redis.clients.jedis.JedisPool;
import redis.clients.util.Pool;

public class ApiServerMetricsServiceImplTest {

  @Mock
  private CounterService counterService;

  @Mock
  private RequestListenerThread listenerThread;

  @Mock
  private ThreadPool threadPool;

  @Mock
  private ThreadPoolExecutor threadPoolExecutor;

  @Mock
  private JedisPool redisPool;

  @Mock
  private Pool<Jedis> redisPoolLib;

  @InjectMocks
  private ApiServerMetricsServiceImpl service;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
  }

  @SuppressWarnings("unchecked")
  @Test
  public void collect() {
    when(listenerThread.getThreadPool()).thenReturn(threadPool);
    when(threadPool.getThreadPoolExecutor()).thenReturn(threadPoolExecutor);
    when(threadPoolExecutor.getQueue()).thenReturn(Mockito.mock(BlockingQueue.class));

    final SentiloArtifactMetrics metrics = service.collect();

    Assert.assertTrue(metrics.getMetrics().containsKey(ApiServerMetricsServiceImpl.REQUESTS_METRICS));
  }
}
