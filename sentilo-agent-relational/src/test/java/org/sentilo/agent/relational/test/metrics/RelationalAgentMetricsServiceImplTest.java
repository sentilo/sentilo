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
package org.sentilo.agent.relational.test.metrics;

import java.util.HashMap;
import java.util.Map;

import org.apache.tomcat.jdbc.pool.DataSource;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.sentilo.agent.common.utils.Constants;
import org.sentilo.agent.relational.metrics.RelationalAgentMetricsServiceImpl;
import org.sentilo.common.metrics.SentiloArtifactMetrics;
import org.springframework.test.util.ReflectionTestUtils;

public class RelationalAgentMetricsServiceImplTest {

  static final String AGENT_NAME = "mock-relational-agent";

  @InjectMocks
  private RelationalAgentMetricsServiceImpl service;

  @Before
  public void setUp() {
    System.setProperty(Constants.SENTILO_AGENT_NAME_ENV, AGENT_NAME);
    MockitoAnnotations.initMocks(this);
  }

  @After
  public void tearDown() {
    System.clearProperty(Constants.SENTILO_AGENT_NAME_ENV);
  }

  @Test
  public void collectCustomMetrics_without_dataSources() {
    final SentiloArtifactMetrics metrics = service.collect();
    Assert.assertFalse(metrics.getMetrics().containsKey(RelationalAgentMetricsServiceImpl.DS_METRICS));
    Assert.assertTrue(metrics.getName().endsWith(AGENT_NAME + ":metrics"));
  }

  @Test
  public void collectCustomMetrics() {
    final DataSource dataSource = Mockito.mock(DataSource.class);
    final Map<String, DataSource> dataSources = new HashMap<String, DataSource>();
    dataSources.put("mockDs", dataSource);
    ReflectionTestUtils.setField(service, "dataSources", dataSources);

    final SentiloArtifactMetrics metrics = service.collect();

    Assert.assertTrue(metrics.getMetrics().containsKey(RelationalAgentMetricsServiceImpl.DS_METRICS));
    Assert.assertTrue(metrics.getName().endsWith(AGENT_NAME + ":metrics"));
  }

}
