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
package org.sentilo.agent.location.test.listener;

import static org.mockito.Matchers.any;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.agent.common.metrics.AgentMetricsCounter;
import org.sentilo.agent.location.batch.AsyncCatalogResourceUpdater;
import org.sentilo.agent.location.listener.MessageListenerImpl;
import org.sentilo.common.domain.EventMessage;
import org.sentilo.common.domain.SensorLocationElement;
import org.sentilo.common.utils.DateUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.redis.connection.Message;

public class MessageListenerImplLoadTest {

  private static final Logger LOGGER = LoggerFactory.getLogger(MessageListenerImplLoadTest.class);
  private static final int NUMBER_EVENTS = 100;
  private static final int NUMBER_THREADS = 10;

  @Mock
  private AsyncCatalogResourceUpdater asyncResourceUpdater;
  @Mock
  private Message message;
  @Mock
  private AgentMetricsCounter metricsCounters;

  private Runnable runnable;

  @InjectMocks
  private final MessageListenerImpl listener = new MessageListenerImpl("TEST");

  String[] timestamps =
      {"06/10/2014T14:45:00CET", "06/10/2014T14:44:00CET", "06/10/2014T14:43:00CET", "06/10/2014T14:42:00CET", "06/10/2014T14:41:00CET",
          "06/10/2014T14:40:00CET", "06/10/2014T14:39:00CET", "06/10/2014T14:38:00CET", "06/10/2014T14:37:00CET", "06/10/2014T14:36:00CET"};

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    runnable = new ProcessRunnable(listener);
  }

  @Test
  public void load() {
    final List<EventMessage> events = getEvents();

    for (final EventMessage event : events) {
      listener.doWithMessage(event);
    }

    verify(asyncResourceUpdater, times(NUMBER_EVENTS)).addResourceToUpdate(any(SensorLocationElement.class));

  }

  @Test
  public void loadWithConcurrence() throws Exception {
    for (int i = 0; i < NUMBER_THREADS; i++) {
      final Thread thread = new Thread(runnable);
      thread.setName(Integer.toString(i));
      thread.setDaemon(false);
      thread.start();
    }

    // Wait 4 seconds until all threads are finish
    Thread.sleep(4000);

    verify(asyncResourceUpdater, atLeast(NUMBER_EVENTS)).addResourceToUpdate(any(SensorLocationElement.class));
  }

  private List<EventMessage> getEvents() {
    final List<EventMessage> events = new ArrayList<EventMessage>();
    for (int i = 0; i < NUMBER_EVENTS; i++) {
      final EventMessage event = new EventMessage();
      event.setProvider("TEST");
      event.setSensor("TEST-S0" + i);
      event.setTimestamp(DateUtils.toStringTimestamp(new Date()));
      event.setLocation("41.41437602127822 2.172379220901462");

      events.add(event);
    }

    return events;
  }

  public class ProcessRunnable implements Runnable {

    private MessageListenerImpl listener;

    public ProcessRunnable(final MessageListenerImpl listener) {
      this.listener = listener;
    }

    @Override
    public void run() {
      LOGGER.debug("Running thread {}", Thread.currentThread().getName());
      final List<EventMessage> events = getEvents();
      try {
        for (final EventMessage event : events) {
          listener.doWithMessage(event);
        }
      } finally {
        LOGGER.debug("Finished thread {} ", Thread.currentThread().getName());
      }
    }
  }
}
