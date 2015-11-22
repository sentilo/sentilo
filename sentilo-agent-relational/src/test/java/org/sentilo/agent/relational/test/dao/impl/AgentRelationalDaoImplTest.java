/*
 * Sentilo
 *  
 * Original version 1.4 Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de Barcelona.
 * Modified by Opentrends adding support for multitenant deployments and SaaS. Modifications on version 1.5 Copyright (C) 2015 Opentrends Solucions i Sistemes, S.L.
 * 
 *   
 * This program is licensed and may be used, modified and redistributed under the
 * terms  of the European Public License (EUPL), either version 1.1 or (at your 
 * option) any later version as soon as they are approved by the European 
 * Commission.
 *   
 * Alternatively, you may redistribute and/or modify this program under the terms
 * of the GNU Lesser General Public License as published by the Free Software 
 * Foundation; either  version 3 of the License, or (at your option) any later 
 * version. 
 *   
 * Unless required by applicable law or agreed to in writing, software distributed
 * under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR 
 * CONDITIONS OF ANY KIND, either express or implied. 
 *   
 * See the licenses for the specific language governing permissions, limitations 
 * and more details.
 *   
 * You should have received a copy of the EUPL1.1 and the LGPLv3 licenses along 
 * with this program; if not, you may find them at: 
 *   
 *   https://joinup.ec.europa.eu/software/page/eupl/licence-eupl
 *   http://www.gnu.org/licenses/ 
 *   and 
 *   https://www.gnu.org/licenses/lgpl.txt
 */
package org.sentilo.agent.relational.test.dao.impl;

import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.Map;

import javax.sql.DataSource;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.agent.relational.domain.Alarm;
import org.sentilo.agent.relational.domain.Observation;
import org.sentilo.agent.relational.domain.Order;
import org.sentilo.agent.relational.repository.impl.AgentRelationalRepositoryImpl;
import org.springframework.test.util.ReflectionTestUtils;

@SuppressWarnings("deprecation")
public class AgentRelationalDaoImplTest {

  final String dsName = "dsMock";
  final String unknownDsName = "_dsMock";

  @Mock
  private DataSource dataSource;
  @Mock
  private Connection connection;
  @Mock
  private Alarm alarm;
  @Mock
  private Observation observation;
  @Mock
  private Order order;
  @Mock
  PreparedStatement ps;

  private AgentRelationalRepositoryImpl dao;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    dao = new AgentRelationalRepositoryImpl();
    injectTablesPrefixValue();
    dao.init();

    final Map<String, DataSource> dataSources = new HashMap<String, DataSource>();
    dataSources.put(dsName, dataSource);

    dao.setDataSources(dataSources);

    when(dataSource.getConnection()).thenReturn(connection);
    when(connection.prepareStatement(anyString())).thenReturn(ps);
  }

  @Test
  public void insertAlarm() throws SQLException {
    setUpMockAlarm();
    when(alarm.getTargetDs()).thenReturn(dsName);

    dao.save(alarm);

    verify(connection).close();
  }

  @Test
  public void noInsertAlarm() throws SQLException {
    setUpMockAlarm();
    when(alarm.getTargetDs()).thenReturn(unknownDsName);

    dao.save(alarm);

    verify(connection, times(0)).close();
  }

  @Test
  public void insertObservation() throws SQLException {
    setUpMockObservation();
    when(observation.getTargetDs()).thenReturn(dsName);

    dao.save(observation);

    verify(connection).close();
  }

  @Test
  public void noInsertObservation() throws SQLException {
    setUpMockObservation();
    when(observation.getTargetDs()).thenReturn(unknownDsName);

    dao.save(observation);

    verify(connection, times(0)).close();
  }

  @Test
  public void insertOrder() throws SQLException {
    setUpMockOrder();
    when(order.getTargetDs()).thenReturn(dsName);

    dao.save(order);

    verify(connection).close();
  }

  @Test
  public void noInsertOrder() throws SQLException {
    setUpMockOrder();
    when(order.getTargetDs()).thenReturn(unknownDsName);

    dao.save(order);

    verify(connection, times(0)).close();
  }

  @Test
  public void getNullJdbcTemplate() throws Exception {
    when(observation.getTargetDs()).thenReturn("");

    Assert.assertTrue(ReflectionTestUtils.invokeMethod(dao, "getJdbcTemplate", observation) == null);
  }

  @Test
  public void nullJdbcTemplateBecauseNoDatasources() throws Exception {
    dao.setDataSources(null);
    setUpMockObservation();
    when(observation.getTargetDs()).thenReturn(null);

    Assert.assertTrue(ReflectionTestUtils.invokeMethod(dao, "getJdbcTemplate", observation) == null);
  }

  private void setUpMockObservation() {
    when(observation.getSensor()).thenReturn("sensor1");
    when(observation.getProvider()).thenReturn("prov1");
    when(observation.getValue()).thenReturn("value1");
    when(observation.getTimestamp()).thenReturn("20130729T12:00:00");
    when(observation.getLocation()).thenReturn("41.394131342886126 2.14171439409256");
  }

  private void setUpMockAlarm() {
    when(alarm.getAlarm()).thenReturn("alarm1");
    when(alarm.getMessage()).thenReturn("test alarm message");
    when(alarm.getTimestamp()).thenReturn("20130729T12:00:00");
  }

  private void setUpMockOrder() {
    when(order.getSensor()).thenReturn("sensor1");
    when(order.getProvider()).thenReturn("prov1");
    when(order.getMessage()).thenReturn("test order message");
    when(order.getTimestamp()).thenReturn("20130729T12:00:00");
  }

  private void injectTablesPrefixValue() throws Exception {
    ReflectionTestUtils.setField(dao, "tables_prefix", "sentilo");
  }

}
