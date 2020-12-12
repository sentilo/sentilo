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
package org.sentilo.agent.historian.test.repository;

import static org.mockito.Matchers.anyListOf;
import static org.mockito.Matchers.anyLong;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.List;

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
import org.sentilo.agent.historian.repository.batch.BatchProcessMonitor;
import org.sentilo.agent.historian.repository.batch.BatchProcessResult;
import org.sentilo.common.domain.EventMessage;
import org.sentilo.common.test.AbstractBaseTest;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.test.util.ReflectionTestUtils;

@RunWith(PowerMockRunner.class)
@PrepareForTest({LoggerFactory.class})
public class BatchProcessMonitorTest extends AbstractBaseTest {

  @InjectMocks
  private BatchProcessMonitor batchProcessMonitor;

  @Mock
  private PendingEventsRepository pendingEventRepository;

  @Mock
  private BatchProcessResult result;

  @Mock
  private AgentMetricsCounter metricsCounters;

  private static Logger logger;

  @BeforeClass
  public static void setUpStatic() throws Exception {
    PowerMockito.mockStatic(LoggerFactory.class);
    logger = PowerMockito.mock(Logger.class);
    when(LoggerFactory.getLogger(anyString())).thenReturn(logger);
    when(LoggerFactory.getLogger(BatchProcessMonitor.class)).thenReturn(logger);
  }

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void notifyOkBatchProcessIsDone() {
    final int elementsProcessed = 10;
    when(result.getNumElementsProcessed()).thenReturn(elementsProcessed);
    when(result.getPendingEvents()).thenReturn(Collections.<EventMessage>emptyList());

    batchProcessMonitor.notifyBatchProcessIsDone(result);

    verify(pendingEventRepository, times(0)).storePendingEvents(anyListOf(EventMessage.class));
    verify(metricsCounters).incrementOutputEvents(elementsProcessed);
    Assert.assertEquals(ReflectionTestUtils.getField(batchProcessMonitor, "numTasksKo"), new Long(0));
    Assert.assertEquals(ReflectionTestUtils.getField(batchProcessMonitor, "numTasksOk"), new Long(1));
    Assert.assertEquals(ReflectionTestUtils.getField(batchProcessMonitor, "numTasksProcessed"), new Long(1));
  }

  @Test
  public void notifyKoBatchProcessIsDone() throws Exception {
    final List<EventMessage> pendingEvents = generateRandomList(EventMessage.class);
    when(result.getPendingEvents()).thenReturn(pendingEvents);

    batchProcessMonitor.notifyBatchProcessIsDone(result);

    verify(pendingEventRepository).storePendingEvents(pendingEvents);
    verify(metricsCounters).incrementOutputEvents(0);
    Assert.assertEquals(ReflectionTestUtils.getField(batchProcessMonitor, "numTasksKo"), new Long(1));
    Assert.assertEquals(ReflectionTestUtils.getField(batchProcessMonitor, "numTasksOk"), new Long(0));
    Assert.assertEquals(ReflectionTestUtils.getField(batchProcessMonitor, "numTasksProcessed"), new Long(1));
  }

  @Test
  public void writeState() {
    batchProcessMonitor.writeState();

    verify(logger).info(" ---- BatchProcessMonitor ---- ");
    verify(logger).info(anyString(), anyLong(), anyLong(), anyLong(), anyLong());
  }
}
