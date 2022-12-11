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
package org.sentilo.agent.location.test.service;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyBoolean;
import static org.mockito.Matchers.anyInt;
import static org.mockito.Matchers.argThat;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.agent.common.metrics.AgentMetricsCounter;
import org.sentilo.agent.common.repository.PendingEventsRepository;
import org.sentilo.agent.location.service.ProcessSensorsLocationsServiceImpl;
import org.sentilo.common.converter.DefaultStringMessageConverter;
import org.sentilo.common.converter.StringMessageConverter;
import org.sentilo.common.domain.EventMessage;
import org.sentilo.common.enums.EventType;
import org.sentilo.common.exception.RESTClientException;
import org.sentilo.common.rest.RESTClient;
import org.sentilo.common.rest.RequestContext;
import org.sentilo.common.test.AbstractBaseTest;
import org.sentilo.common.utils.SentiloConstants;
import org.springframework.data.redis.core.SetOperations;
import org.springframework.data.redis.core.StringRedisTemplate;

public class ProcessSensorsLocationsServiceImplTest extends AbstractBaseTest {

  private final StringMessageConverter stringMsgConverter = new DefaultStringMessageConverter();

  @InjectMocks
  private ProcessSensorsLocationsServiceImpl service;

  @Mock
  private RESTClient restClient;

  @Mock
  private PendingEventsRepository pendingEventRepository;

  @Mock
  private AgentMetricsCounter metricsCounters;

  @Mock
  private StringRedisTemplate redisTemplate;

  @Mock
  private SetOperations<String, String> sOps;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    System.setProperty(SentiloConstants.SENTILO_AGENT_NAME_ENV, "mockAgent");
  }

  @After
  public void tearDown() {
    System.clearProperty(SentiloConstants.SENTILO_AGENT_NAME_ENV);
  }

  @Test
  public void readAndProcessPendingEvents_when_empty() {
    final List<String> pendingEvents = Collections.EMPTY_LIST;
    when(redisTemplate.opsForSet()).thenReturn(sOps);
    when(sOps.pop("mockagent:pending:events", 10)).thenReturn(pendingEvents);

    service.readAndProcessPendingEvents(10);

    verify(restClient, times(0)).put(any(RequestContext.class));
    verify(metricsCounters, times(0)).incrementOutputEvents(anyInt());
    verify(metricsCounters, times(0)).isRemoteServerConnectionOk(anyBoolean());
  }

  @Test
  public void readAndProcessPendingEvents() {
    final List<String> pendingEvents = getPendingEventList();
    when(redisTemplate.opsForSet()).thenReturn(sOps);
    when(sOps.pop("mockagent:pending:events", 10)).thenReturn(pendingEvents);

    service.readAndProcessPendingEvents(10);

    verify(restClient).put(any(RequestContext.class));
    verify(metricsCounters).incrementOutputEvents(2);
    verify(metricsCounters).isRemoteServerConnectionOk(true);
  }

  @Test
  public void error_when_readAndProcessPendingEvents() {
    final List<String> pendingEvents = getPendingEventList();
    when(redisTemplate.opsForSet()).thenReturn(sOps);
    when(sOps.pop("mockagent:pending:events", 10)).thenReturn(pendingEvents);
    doThrow(RESTClientException.class).when(restClient).put(any(RequestContext.class));

    service.readAndProcessPendingEvents(10);

    verify(restClient).put(any(RequestContext.class));
    verify(metricsCounters, times(0)).incrementOutputEvents(2);
    verify(metricsCounters).isRemoteServerConnectionOk(false);
    verify(pendingEventRepository).storePendingEvents(argThat(new EqualListSizeQueryMatcher<EventMessage>(2)));
  }

  private List<String> getPendingEventList() {
    final EventMessage ev1 = new EventMessage();
    ev1.setTopic("/data/provider1/sensor23");
    ev1.setType(EventType.DATA.name());
    ev1.setSensor("sensor23");
    ev1.setProvider("provider1");
    ev1.setMessage("45");
    ev1.setLocation("41.234536543 2.43526710");
    ev1.setTimestamp("06/03/2022T14:23:21");

    final EventMessage ev2 = new EventMessage();
    ev2.setTopic("/data/provider2/sensor2");
    ev2.setType(EventType.DATA.name());
    ev2.setSensor("sensor2");
    ev2.setProvider("provider2");
    ev2.setMessage("34");
    ev2.setLocation("41.234536543 2.43526710");
    ev2.setTimestamp("06/03/2022T14:23:21");

    return Arrays.asList(stringMsgConverter.marshal(ev1), stringMsgConverter.marshal(ev2));
  }

}
