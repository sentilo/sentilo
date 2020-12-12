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
package org.sentilo.agent.common.test.metrics;

import java.util.Map;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.agent.common.metrics.AgentMetricsServiceImpl;
import org.sentilo.agent.common.repository.PendingEventsRepository;
import org.sentilo.agent.common.utils.Constants;
import org.sentilo.common.metrics.SentiloArtifactMetrics;

public class AgentMetricsServiceImplTest {

  private final String agentName = "MOCK-AGENT";

  @Mock
  private PendingEventsRepository pendingEventRepository;

  @InjectMocks
  private AgentMetricsServiceImpl service = new MockAgentMetricsServiceImpl();

  @Before
  public void setUp() throws Exception {
    System.setProperty(Constants.SENTILO_AGENT_NAME_ENV, agentName);
    MockitoAnnotations.initMocks(this);
  }

  @After
  public void tearDown() {
    System.clearProperty(Constants.SENTILO_AGENT_NAME_ENV);
  }

  @SuppressWarnings("unchecked")
  @Test
  public void incrementInputAndOutputEvents() {
    service.incrementInputEvents(10);
    service.incrementOutputEvents(10);

    final SentiloArtifactMetrics agentMetrics = service.collect();

    Assert.assertTrue(agentMetrics.getMetrics().get(AgentMetricsServiceImpl.EVENTS_METRICS) != null);
    final Map<String, Object> events = (Map<String, Object>) agentMetrics.getMetrics().get(AgentMetricsServiceImpl.EVENTS_METRICS);
    Assert.assertEquals(10L, events.get(AgentMetricsServiceImpl.EVENTS_INPUT));
    Assert.assertEquals(10L, events.get(AgentMetricsServiceImpl.EVENTS_OUTPUT));
    Assert.assertTrue(events.containsKey(AgentMetricsServiceImpl.REMOTE_SERVER_CONN));
    Assert.assertEquals("001", agentMetrics.getMetrics().get("mock_backend_server_status"));
  }

  @SuppressWarnings("unchecked")
  @Test
  public void collectMetrics_without_remoteServer() {
    final AgentMetricsServiceImpl service2 = new MockAgentMetricsServiceImpl_without_remoteMetrics();
    final SentiloArtifactMetrics agentMetrics = service2.collect();

    Assert.assertTrue(agentMetrics.getMetrics().get(AgentMetricsServiceImpl.EVENTS_METRICS) != null);
    final Map<String, Object> events = (Map<String, Object>) agentMetrics.getMetrics().get(AgentMetricsServiceImpl.EVENTS_METRICS);
    Assert.assertFalse(events.containsKey(AgentMetricsServiceImpl.REMOTE_SERVER_CONN));
  }

  class MockAgentMetricsServiceImpl extends AgentMetricsServiceImpl {

    protected Map<String, Object> collectCustomMetrics() {
      final Map<String, Object> agentMetrics = super.collectCustomMetrics();

      agentMetrics.put("mock_backend_server_status", "001");

      return agentMetrics;
    }
  }

  class MockAgentMetricsServiceImpl_without_remoteMetrics extends AgentMetricsServiceImpl {

    @Override
    protected boolean addRemoteServerConnection() {
      return false;
    }

  }
}
