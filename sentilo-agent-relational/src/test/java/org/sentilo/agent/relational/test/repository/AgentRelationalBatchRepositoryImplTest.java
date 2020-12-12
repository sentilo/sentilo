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
package org.sentilo.agent.relational.test.repository;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.tomcat.jdbc.pool.DataSource;
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
import org.sentilo.agent.common.metrics.AgentMetricsCounter;
import org.sentilo.agent.relational.domain.Alarm;
import org.sentilo.agent.relational.domain.Observation;
import org.sentilo.agent.relational.domain.Order;
import org.sentilo.agent.relational.repository.batch.BatchProcessMonitor;
import org.sentilo.agent.relational.repository.impl.AgentRelationalBatchRepositoryImpl;
import org.sentilo.common.test.AbstractBaseTest;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.jdbc.core.BatchPreparedStatementSetter;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.transaction.PlatformTransactionManager;

@RunWith(PowerMockRunner.class)
@PrepareForTest({AgentRelationalBatchRepositoryImpl.class, LoggerFactory.class, ReflectionTestUtils.class})
public class AgentRelationalBatchRepositoryImplTest extends AbstractBaseTest {

  @Mock
  private PlatformTransactionManager platformTransactionManager;

  @Mock
  private BatchProcessMonitor batchUpdateMonitor;

  @Mock
  private JdbcTemplate jdbcTemplate;

  @Mock
  private AgentMetricsCounter metricsCounters;

  @InjectMocks
  private AgentRelationalBatchRepositoryImpl repository;

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

    ReflectionTestUtils.setField(repository, "tablesPrefix", "sentilo");
    repository.init();
  }

  @Test(expected = IllegalStateException.class)
  public void koInit() {
    ReflectionTestUtils.setField(repository, "tablesPrefix", "$sentilo");
    repository.init();
  }

  @Test
  public void defaultInit() {
    Assert.assertTrue(10 == (Integer) ReflectionTestUtils.getField(repository, "batchSize"));
    Assert.assertTrue(5 == (Integer) ReflectionTestUtils.getField(repository, "numMaxWorkers"));
    Assert.assertTrue(1 == (Integer) ReflectionTestUtils.getField(repository, "numMaxRetries"));
    Assert.assertNotNull(ReflectionTestUtils.getField(repository, "workersManager"));
  }

  @Test
  public void init() {
    ReflectionTestUtils.setField(repository, "batchSize", 100);
    ReflectionTestUtils.setField(repository, "numMaxWorkers", 1);
    ReflectionTestUtils.setField(repository, "numMaxRetries", 3);
    repository.init();

    Assert.assertTrue(100 == (Integer) ReflectionTestUtils.getField(repository, "batchSize"));
    Assert.assertTrue(1 == (Integer) ReflectionTestUtils.getField(repository, "numMaxWorkers"));
    Assert.assertTrue(3 == (Integer) ReflectionTestUtils.getField(repository, "numMaxRetries"));
    Assert.assertNotNull(ReflectionTestUtils.getField(repository, "workersManager"));
  }

  @SuppressWarnings("rawtypes")
  @Test
  public void setDataSources() throws Exception {
    final Map<String, DataSource> dataSources = generateRandomMap(String.class, DataSource.class);

    repository.setDataSources(Collections.<String, DataSource>emptyMap());
    Assert.assertNull(ReflectionTestUtils.getField(repository, "batchQueues"));
    repository.setDataSources(dataSources);
    Assert.assertTrue(((Map) ReflectionTestUtils.getField(repository, "batchQueues")).size() == dataSources.size());
  }

  @SuppressWarnings("rawtypes")
  @Test
  public void saveAndFlush() throws Exception {
    final String mockDs = "mockDs";
    final Map<String, DataSource> dataSources = new HashMap<String, DataSource>();
    dataSources.put(mockDs, new DataSource());

    final Observation obs = new Observation();
    obs.setTargetDs(mockDs);
    final Order order = new Order();
    order.setTargetDs(mockDs);
    final Alarm alarm = new Alarm();
    alarm.setTargetDs(mockDs);

    ReflectionTestUtils.setField(repository, "batchSize", 2);
    repository.setDataSources(dataSources);

    repository.save(obs);
    repository.save(order);
    repository.save(alarm);

    Assert.assertTrue(((List) ((Map) ReflectionTestUtils.getField(repository, "batchQueues")).get(mockDs)).size() == 1);

    repository.flush();

    verify(jdbcTemplate, atLeast(1)).batchUpdate(anyString(), any(BatchPreparedStatementSetter.class));
  }

}
