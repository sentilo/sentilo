/*
 * Sentilo
 *  
 * Original version 1.4 Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de
 * Barcelona. Modified by Opentrends adding support for multitenant deployments and SaaS. 
 * Modifications on version 1.5 Copyright (C) 2015 Opentrends Solucions i Sistemes, S.L.
 * 
 *   
 * This program is licensed and may be used, modified and redistributed under the terms  of the 
 * European Public License (EUPL), either version 1.1 or (at your option) any later version as soon 
 * as they are approved by the European Commission.
 *   
 * Alternatively, you may redistribute and/or modify this program under the terms of the GNU Lesser 
 * General Public License as published by the Free Software Foundation; either  version 3 of the 
 * License, or (at your option) any later version. 
 *   
 * Unless required by applicable law or agreed to in writing, software distributed under the License 
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR  CONDITIONS OF ANY KIND, either express 
 * or implied. 
 *   
 * See the licenses for the specific language governing permissions, limitations and more details.
 *   
 * You should have received a copy of the EUPL1.1 and the LGPLv3 licenses along with this program; 
 * if not, you may find them at: 
 *   
 * https://joinup.ec.europa.eu/software/page/eupl/licence-eupl http://www.gnu.org/licenses/   and 
 * https://www.gnu.org/licenses/lgpl.txt
 */
package org.sentilo.agent.kafka.test.repository;

import static org.mockito.Matchers.anyListOf;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.argThat;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Assert;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import org.sentilo.agent.common.metrics.AgentMetricsCounter;
import org.sentilo.agent.common.repository.PendingEventsRepository;
import org.sentilo.agent.kafka.repository.ProcessMonitor;
import org.sentilo.agent.kafka.repository.ProcessResult;
import org.sentilo.common.domain.EventMessage;
import org.sentilo.common.test.AbstractBaseTest;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.test.util.ReflectionTestUtils;

@RunWith(PowerMockRunner.class)
@PrepareForTest({LoggerFactory.class})
public class ProcessMonitorTest extends AbstractBaseTest {

  @InjectMocks
  private ProcessMonitor processMonitor;

  @Mock
  private PendingEventsRepository pendingEventRepository;

  @Mock
  private ProcessResult result;

  @Mock
  private AgentMetricsCounter metricsCounters;

  private static Logger logger;

  @BeforeClass
  public static void setUpStatic() throws Exception {
    PowerMockito.mockStatic(LoggerFactory.class);
    logger = PowerMockito.mock(Logger.class);
    when(LoggerFactory.getLogger(anyString())).thenReturn(logger);
    when(LoggerFactory.getLogger(ProcessMonitor.class)).thenReturn(logger);
  }

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void notifyOkProcessIsDone() {
    when(result.getPendingEvent()).thenReturn(null);

    processMonitor.notifyProcessIsDone(result);

    verify(pendingEventRepository, times(0)).storePendingEvents(anyListOf(EventMessage.class));
    verify(metricsCounters).incrementOutputEvents(1);
    verify(metricsCounters).isRemoteServerConnectionOk(true);
    Assert.assertEquals(ReflectionTestUtils.getField(processMonitor, "numTasksKo"), new Long(0));
    Assert.assertEquals(ReflectionTestUtils.getField(processMonitor, "numTasksOk"), new Long(1));
  }

  @Test
  public void notifyKoProcessIsDone() {
    final EventMessage message = Mockito.mock(EventMessage.class);
    when(result.getPendingEvent()).thenReturn(message);

    processMonitor.notifyProcessIsDone(result);

    verify(pendingEventRepository).storePendingEvents(argThat(new EqualListSizeQueryMatcher<>(1)));
    verify(metricsCounters, times(0)).incrementOutputEvents(1);
    verify(metricsCounters).isRemoteServerConnectionOk(false);
    Assert.assertEquals(ReflectionTestUtils.getField(processMonitor, "numTasksKo"), new Long(1));
    Assert.assertEquals(ReflectionTestUtils.getField(processMonitor, "numTasksOk"), new Long(0));
  }

  @Test
  public void writeState() {
    processMonitor.writeState();

    verify(logger).info(anyString());
  }
}
