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
package org.sentilo.agent.relational.test.repository;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.agent.relational.repository.batch.BatchProcessCallback;
import org.sentilo.agent.relational.repository.batch.BatchProcessContext;
import org.sentilo.agent.relational.repository.batch.BatchProcessResult;
import org.sentilo.agent.relational.repository.batch.BatchProcessWorker;
import org.sentilo.common.domain.EventMessage;
import org.sentilo.common.enums.EventType;
import org.sentilo.common.test.AbstractBaseTest;
import org.springframework.dao.DataAccessException;
import org.springframework.jdbc.core.BatchPreparedStatementSetter;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.support.TransactionTemplate;

public class BatchProcessWorkerTest extends AbstractBaseTest {

  private final static int TOTAL = 50;

  @Mock
  private BatchProcessContext batchUpdateContext;

  @Mock
  private JdbcTemplate jdbcTemplate;
  @Mock
  private PlatformTransactionManager transactionManager;
  @Mock
  private BatchProcessCallback callback;

  private final int numMaxRetries = 3;
  private final String tablesPrefix = "sentilo";

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);

    when(batchUpdateContext.getCallback()).thenReturn(callback);
    when(batchUpdateContext.getJdbcTemplate()).thenReturn(jdbcTemplate);
    when(batchUpdateContext.getTablesPrefix()).thenReturn(tablesPrefix);
    when(batchUpdateContext.getNumMaxRetries()).thenReturn(numMaxRetries);
    when(batchUpdateContext.getTransactionManager()).thenReturn(transactionManager);
    when(batchUpdateContext.getDataToPersist()).thenReturn(buildMockList(TOTAL));
  }

  @Test
  public void constructor() {
    final BatchProcessWorker worker = new BatchProcessWorker(batchUpdateContext);

    Assert.assertTrue(((String) ReflectionTestUtils.getField(worker, "observationPs")).startsWith("insert into sentilo_observations"));
    Assert.assertTrue(((String) ReflectionTestUtils.getField(worker, "orderPs")).startsWith("insert into sentilo_orders"));
    Assert.assertTrue(((String) ReflectionTestUtils.getField(worker, "alarmPs")).startsWith("insert into sentilo_alarms"));
    Assert.assertEquals(numMaxRetries, ReflectionTestUtils.getField(worker, "numMaxRetries"));
    Assert.assertEquals(callback, ReflectionTestUtils.getField(worker, "callback"));
    Assert.assertEquals(jdbcTemplate, ReflectionTestUtils.getField(worker, "jdbcTemplate"));
    Assert.assertEquals(transactionManager,
        ((TransactionTemplate) ReflectionTestUtils.getField(worker, "transactionTemplate")).getTransactionManager());
  }

  @Test
  public void call() {
    final BatchProcessWorker worker = new BatchProcessWorker(batchUpdateContext);
    final BatchProcessResult result = worker.call();

    verify(callback).notifyBatchUpdateIsDone(result);
    verify(jdbcTemplate, times(3)).batchUpdate(anyString(), any(BatchPreparedStatementSetter.class));
    Assert.assertTrue(3 * TOTAL == result.getNumElementsPersisted());
  }

  @Test
  public void emptyCall() {
    when(batchUpdateContext.getDataToPersist()).thenReturn(Collections.<EventMessage>emptyList());
    final BatchProcessWorker worker = new BatchProcessWorker(batchUpdateContext);

    final BatchProcessResult result = worker.call();

    verify(callback).notifyBatchUpdateIsDone(result);
    verify(jdbcTemplate, times(0)).batchUpdate(anyString(), any(BatchPreparedStatementSetter.class));
  }

  @Test
  public void koCall() {
    final String alarmPs =
        "insert into sentilo_alarms (alert, alert_type, provider, component, component_type, sensor, sensor_type, location, message, timestamp, event_timestamp, published_at, publisher, tenant, publisher_tenant) values (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)";
    final BatchProcessWorker worker = new BatchProcessWorker(batchUpdateContext);
    doThrow(MockDataAccessException.class).when(jdbcTemplate).batchUpdate(eq(alarmPs), any(BatchPreparedStatementSetter.class));

    final BatchProcessResult result = worker.call();

    verify(callback).notifyBatchUpdateIsDone(result);
    verify(jdbcTemplate, times(8)).batchUpdate(anyString(), any(BatchPreparedStatementSetter.class));
    Assert.assertEquals(numMaxRetries, ReflectionTestUtils.getField(worker, "numRetries"));
  }

  private List<EventMessage> buildMockList(final long total) {
    // Mock list contains data, alarm and order events
    final List<EventMessage> resources = new ArrayList<EventMessage>();
    final String[] eventsTypes = {EventType.DATA.name(), EventType.ORDER.name(), EventType.ALARM.name()};
    for (final String eventsType : eventsTypes) {
      for (int i = 0; i < total; i++) {
        final EventMessage event = new EventMessage();
        event.setTime(System.currentTimeMillis());
        event.setType(eventsType);
        event.setTopic("/data/mockProvider/mockSensor");
        event.setMessage(Integer.toString(i));
        resources.add(event);
      }
    }

    return resources;
  }

  public class MockDataAccessException extends DataAccessException {

    private static final long serialVersionUID = 1L;
    private static final String MOCK_MESSAGE = "mock message";

    public MockDataAccessException() {
      super(MOCK_MESSAGE);
    }

  }
}
