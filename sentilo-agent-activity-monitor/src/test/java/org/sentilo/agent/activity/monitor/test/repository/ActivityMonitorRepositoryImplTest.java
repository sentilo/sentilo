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
package org.sentilo.agent.activity.monitor.test.repository;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.List;
import java.util.concurrent.ExecutorService;

import org.junit.Assert;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import org.sentilo.agent.activity.monitor.repository.batch.BatchProcessMonitor;
import org.sentilo.agent.activity.monitor.repository.impl.ActivityMonitorRepositoryImpl;
import org.sentilo.common.domain.EventMessage;
import org.sentilo.common.rest.RESTClient;
import org.sentilo.common.rest.RequestContext;
import org.sentilo.common.test.AbstractBaseTest;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.test.util.ReflectionTestUtils;

@RunWith(PowerMockRunner.class)
@PrepareForTest({ActivityMonitorRepositoryImpl.class, LoggerFactory.class, ReflectionTestUtils.class})
public class ActivityMonitorRepositoryImplTest extends AbstractBaseTest {

  @InjectMocks
  private ActivityMonitorRepositoryImpl repository;

  @Mock
  private RESTClient restClient;

  @Mock
  private EventMessage event;

  @Mock
  private BatchProcessMonitor batchProcessMonitor;

  @Mock
  private ExecutorService workersManager;

  private static Logger logger;

  @BeforeClass
  public static void setUpStatic() throws Exception {
    PowerMockito.mockStatic(LoggerFactory.class);
    logger = PowerMockito.mock(Logger.class);
    when(LoggerFactory.getLogger(anyString())).thenReturn(logger);
    when(LoggerFactory.getLogger(any(Class.class))).thenReturn(logger);
  }

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);

    final String versionResponse =
        "{\"name\": \"node-a\",\"cluster_name\": \"elastic\", \"version\": {\"number\": \"2.3.1\"},\"tagline\": \"You Know, for Search\"}";
    when(restClient.get(eq(new RequestContext("/")))).thenReturn(versionResponse);

    repository.init();
  }

  @Test
  public void init() throws Exception {

    Assert.assertTrue(10 == (Integer) ReflectionTestUtils.getField(repository, "batchSize"));
    Assert.assertTrue(5 == (Integer) ReflectionTestUtils.getField(repository, "numMaxWorkers"));
    Assert.assertTrue(1 == (Integer) ReflectionTestUtils.getField(repository, "numMaxRetries"));
    Assert.assertNotNull(ReflectionTestUtils.getField(repository, "workersManager"));

    // New ActivityMonitorRepositoryImpl
    final ActivityMonitorRepositoryImpl newRepository = new ActivityMonitorRepositoryImpl();
    ReflectionTestUtils.setField(newRepository, "batchSize", Integer.valueOf(3));
    ReflectionTestUtils.setField(newRepository, "numMaxWorkers", Integer.valueOf(5));
    ReflectionTestUtils.setField(newRepository, "numMaxRetries", Integer.valueOf(1));
    ReflectionTestUtils.setField(newRepository, "restClient", restClient);
    newRepository.init();

    Assert.assertTrue(3 == (Integer) ReflectionTestUtils.getField(newRepository, "batchSize"));
    Assert.assertTrue(5 == (Integer) ReflectionTestUtils.getField(newRepository, "numMaxWorkers"));
    Assert.assertTrue(1 == (Integer) ReflectionTestUtils.getField(newRepository, "numMaxRetries"));
  }

  @SuppressWarnings("unchecked")
  @Test
  public void publishMessageToElasticSearch() throws Exception {
    final Object value = ReflectionTestUtils.getField(repository, "batchQueue");

    repository.publishMessageToElasticSearch(event);

    Assert.assertTrue(((List<EventMessage>) value).size() == 1);
  }

  @SuppressWarnings("unchecked")
  @Test
  public void clearBatchQueue() throws Exception {
    for (int i = 0; i < 12; i++) {
      repository.publishMessageToElasticSearch(event);
    }

    final Object value = ReflectionTestUtils.getField(repository, "batchQueue");

    Assert.assertTrue(((List<EventMessage>) value).size() == 2);
  }

  @Test
  public void flush() throws Exception {

    repository.flush();
    ReflectionTestUtils.setField(repository, "batchQueue", generateRandomList(EventMessage.class));
    repository.flush();

    verify(logger, times(2 * 2)).info(anyString());
    // verify(logger).info(anyString(), anyVararg());
  }

}
