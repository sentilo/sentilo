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
package org.sentilo.agent.relational.test.jdbc;

import static org.mockito.Mockito.when;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.Map;

import javax.sql.DataSource;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.agent.relational.jdbc.SentiloRoutingDataSource;
import org.sentilo.agent.relational.utils.ThreadLocalProperties;

public class SentiloRoutingDataSourceTest {

  private final String dsName = "mockDs";

  private SentiloRoutingDataSource routingDataSource;
  @Mock
  private DataSource mockDataSource;
  @Mock
  private Connection mockConnection;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);

    routingDataSource = new SentiloRoutingDataSource();
    final Map<Object, Object> dataSources = new HashMap<Object, Object>();
    dataSources.put(dsName, mockDataSource);
    routingDataSource.setTargetDataSources(dataSources);
    routingDataSource.afterPropertiesSet();
  }

  @Test
  public void determineCurrentLookupKey() throws SQLException {
    ThreadLocalProperties.set(dsName);
    when(mockDataSource.getConnection()).thenReturn(mockConnection);
    final Connection connection = routingDataSource.getConnection();
    Assert.assertEquals(mockConnection, connection);
  }

  @Test(expected = IllegalStateException.class)
  public void nullConnection() throws SQLException {
    ThreadLocalProperties.set("");
    when(mockDataSource.getConnection()).thenReturn(mockConnection);
    routingDataSource.getConnection();
  }
}
