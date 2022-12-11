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
package org.sentilo.agent.common.test.scheduler;

import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.agent.common.scheduler.QueuePendingEventsProcessJob;
import org.sentilo.agent.common.scheduler.StreamPendingEventsProcessJob;
import org.sentilo.agent.common.service.AsyncQueuePendingEventService;
import org.sentilo.agent.common.service.AsyncStreamPendingEventService;
import org.springframework.test.util.ReflectionTestUtils;

public class PendingEventsProcessJobTest {

  private static final int ITEMS_TO_READ = 50;

  @Mock
  private AsyncStreamPendingEventService streamPendingEventService;

  @Mock
  private AsyncQueuePendingEventService queuePendingEventService;

  @InjectMocks
  private StreamPendingEventsProcessJob streamJob;

  @InjectMocks
  private QueuePendingEventsProcessJob queueJob;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    ReflectionTestUtils.setField(streamJob, "ITEMS_TO_READ", ITEMS_TO_READ);
    ReflectionTestUtils.setField(queueJob, "ITEMS_TO_READ", ITEMS_TO_READ);
  }

  @Test
  public void run_when_no_pending_events() {
    when(streamPendingEventService.readAndProcessPendingEvents(ITEMS_TO_READ)).thenReturn(0);
    when(queuePendingEventService.readAndProcessPendingEvents(ITEMS_TO_READ)).thenReturn(0);

    streamJob.run();
    queueJob.run();

    verify(streamPendingEventService).readAndProcessPendingEvents(ITEMS_TO_READ);
    verify(queuePendingEventService).readAndProcessPendingEvents(ITEMS_TO_READ);
  }

  @Test
  public void run() {
    when(streamPendingEventService.readAndProcessPendingEvents(ITEMS_TO_READ)).thenReturn(ITEMS_TO_READ, 40, 0);
    when(queuePendingEventService.readAndProcessPendingEvents(ITEMS_TO_READ)).thenReturn(ITEMS_TO_READ, 40, 0);

    streamJob.run();
    queueJob.run();

    verify(streamPendingEventService, times(3)).readAndProcessPendingEvents(ITEMS_TO_READ);
    verify(queuePendingEventService, times(3)).readAndProcessPendingEvents(ITEMS_TO_READ);
  }
}
