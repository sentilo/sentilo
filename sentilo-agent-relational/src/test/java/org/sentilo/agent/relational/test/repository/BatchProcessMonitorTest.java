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
import static org.mockito.Matchers.anyListOf;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Collections;
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
import org.sentilo.agent.common.repository.PendingEventsRepository;
import org.sentilo.agent.relational.repository.batch.BatchProcessMonitor;
import org.sentilo.agent.relational.repository.batch.BatchProcessResult;
import org.sentilo.common.domain.EventMessage;
import org.sentilo.common.test.AbstractBaseTest;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.test.util.ReflectionTestUtils;

@RunWith(PowerMockRunner.class)
@PrepareForTest({BatchProcessMonitor.class, LoggerFactory.class, ReflectionTestUtils.class})
public class BatchProcessMonitorTest extends AbstractBaseTest {

  @Mock
  private PendingEventsRepository pendingEventRepository;

  @Mock
  private BatchProcessResult result;

  @Mock
  private AgentMetricsCounter metricsCounters;

  @InjectMocks
  private BatchProcessMonitor monitor;

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
  }

  @SuppressWarnings("rawtypes")
  @Test
  public void setDataSources() throws Exception {
    final Map<String, DataSource> dataSources = generateRandomMap(String.class, DataSource.class);

    monitor.setDataSources(Collections.<String, DataSource>emptyMap());
    Assert.assertTrue(((Map) ReflectionTestUtils.getField(monitor, "dsToMonitor")).isEmpty());
    monitor.setDataSources(dataSources);
    Assert.assertTrue(((Map) ReflectionTestUtils.getField(monitor, "dsToMonitor")).size() == dataSources.size());
  }

  @Test
  public void notifyBatchUpdateIsKo() throws Exception {
    when(result.getNumElementsPersisted()).thenReturn(0);
    when(result.getNumElementsToPersist()).thenReturn(10);

    monitor.notifyBatchUpdateIsDone(result);

    verify(pendingEventRepository).storePendingEvents(anyListOf(EventMessage.class));
    verify(metricsCounters).incrementOutputEvents(0);
    Assert.assertTrue(1 == (Long) ReflectionTestUtils.getField(monitor, "numTasksProcessed"));
    Assert.assertTrue(1 == (Long) ReflectionTestUtils.getField(monitor, "numTasksKo"));
    Assert.assertTrue(10 == (Long) ReflectionTestUtils.getField(monitor, "numPendingElements"));
    Assert.assertTrue(0 == (Long) ReflectionTestUtils.getField(monitor, "numTasksOk"));
  }

  @Test
  public void notifyBatchUpdateIsOk() throws Exception {
    when(result.getNumElementsPersisted()).thenReturn(10);

    monitor.notifyBatchUpdateIsDone(result);

    verify(pendingEventRepository, times(0)).storePendingEvents(anyListOf(EventMessage.class));
    verify(metricsCounters).incrementOutputEvents(10);
    Assert.assertTrue(1 == (Long) ReflectionTestUtils.getField(monitor, "numTasksProcessed"));
    Assert.assertTrue(0 == (Long) ReflectionTestUtils.getField(monitor, "numTasksKo"));
    Assert.assertTrue(0 == (Long) ReflectionTestUtils.getField(monitor, "numPendingElements"));
    Assert.assertTrue(1 == (Long) ReflectionTestUtils.getField(monitor, "numTasksOk"));
  }

  @Test
  public void writeState() {
    monitor.writeState();
    verify(logger).info(anyString());
  }

}
