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

import static org.mockito.Matchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.agent.common.scheduler.PendingEventsProcessJob;
import org.sentilo.agent.common.service.AsyncPendingEventService;
import org.sentilo.agent.common.utils.Constants;
import org.sentilo.common.converter.DefaultStringMessageConverter;
import org.sentilo.common.converter.StringMessageConverter;
import org.sentilo.common.domain.EventMessage;
import org.sentilo.common.enums.EventType;
import org.springframework.data.redis.core.SetOperations;
import org.springframework.data.redis.core.StringRedisTemplate;

public class PendingEventsProcessJobTest {

  private static final String AGENT_NAME = "mockAgent";

  private final StringMessageConverter converter = new DefaultStringMessageConverter();

  @Mock
  private StringRedisTemplate redisTemplate;

  @Mock
  private AsyncPendingEventService pendingEventService;

  @Mock
  private SetOperations<String, String> sOps;

  @InjectMocks
  private PendingEventsProcessJob job;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    when(redisTemplate.opsForSet()).thenReturn(sOps);

    System.setProperty(Constants.SENTILO_AGENT_NAME_ENV, AGENT_NAME);
  }

  @Test
  public void emptyQueue() {
    when(sOps.pop("mockagent:pending:events")).thenReturn("nil");

    job.run();

    verify(sOps).pop("mockagent:pending:events");
    verify(pendingEventService, times(0)).process(any(EventMessage.class));
  }

  @Test
  public void run() {
    final String jsonEvent = converter.marshal(buildMockDataEventMessage());
    when(sOps.pop("mockagent:pending:events")).thenReturn(jsonEvent);

    job.run();

    verify(sOps, times(200)).pop("mockagent:pending:events");
    verify(pendingEventService, times(200)).process(any(EventMessage.class));
  }

  @Test
  public void fewPendingEvents() {
    final String jsonEvent = converter.marshal(buildMockDataEventMessage());
    when(sOps.pop("mockagent:pending:events")).thenReturn(jsonEvent, jsonEvent, jsonEvent, jsonEvent, "nil");

    job.run();

    verify(sOps, times(5)).pop("mockagent:pending:events");
    verify(pendingEventService, times(4)).process(any(EventMessage.class));
  }

  private EventMessage buildMockDataEventMessage() {
    final EventMessage event = new EventMessage();
    event.setTopic("/data/provider");
    event.setType(EventType.DATA.name().toLowerCase());
    event.setTimestamp("21/11/2015T14:25:39");
    event.setMessage("21");
    return event;
  }

}
