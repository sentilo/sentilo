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
import org.sentilo.agent.relational.domain.Alarm;
import org.sentilo.agent.relational.domain.Data;
import org.sentilo.agent.relational.domain.Observation;
import org.sentilo.agent.relational.domain.Order;
import org.sentilo.agent.relational.repository.batch.BatchProcessCallback;
import org.sentilo.agent.relational.repository.batch.BatchProcessContext;
import org.sentilo.agent.relational.repository.batch.BatchProcessResult;
import org.sentilo.agent.relational.repository.batch.BatchProcessWorker;
import org.sentilo.agent.relational.utils.ThreadLocalProperties;
import org.sentilo.common.test.AbstractBaseTest;
import org.springframework.dao.DataAccessException;
import org.springframework.jdbc.core.BatchPreparedStatementSetter;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.support.TransactionTemplate;

public class BatchProcessWorkerTest extends AbstractBaseTest {

  @Mock
  private BatchProcessContext batchUpdateContext;

  @Mock
  private JdbcTemplate jdbcTemplate;
  @Mock
  private PlatformTransactionManager transactionManager;
  @Mock
  private BatchProcessCallback callback;

  private final int numMaxRetries = 3;
  private final String targetDs = "mockDs";
  private final String tablesPrefix = "sentilo";

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);

    when(batchUpdateContext.getCallback()).thenReturn(callback);
    when(batchUpdateContext.getJdbcTemplate()).thenReturn(jdbcTemplate);
    when(batchUpdateContext.getTablesPrefix()).thenReturn(tablesPrefix);
    when(batchUpdateContext.getNumMaxRetries()).thenReturn(numMaxRetries);
    when(batchUpdateContext.getTargetDs()).thenReturn(targetDs);
    when(batchUpdateContext.getTransactionManager()).thenReturn(transactionManager);
    when(batchUpdateContext.getDataToPersist()).thenReturn(buildMockList());
  }

  @Test
  public void constructor() {
    final BatchProcessWorker worker = new BatchProcessWorker(batchUpdateContext);

    Assert.assertTrue(((String) ReflectionTestUtils.getField(worker, "observationPs")).startsWith("insert into sentilo_observations"));
    Assert.assertTrue(((String) ReflectionTestUtils.getField(worker, "orderPs")).startsWith("insert into sentilo_orders"));
    Assert.assertTrue(((String) ReflectionTestUtils.getField(worker, "alarmPs")).startsWith("insert into sentilo_alarms"));
    Assert.assertEquals(targetDs, ReflectionTestUtils.getField(worker, "targetDs"));
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
    Assert.assertEquals(targetDs, ThreadLocalProperties.get());
  }

  @Test
  public void emptyCall() {
    when(batchUpdateContext.getDataToPersist()).thenReturn(Collections.<Data>emptyList());
    final BatchProcessWorker worker = new BatchProcessWorker(batchUpdateContext);

    final BatchProcessResult result = worker.call();

    verify(callback).notifyBatchUpdateIsDone(result);
    verify(jdbcTemplate, times(0)).batchUpdate(anyString(), any(BatchPreparedStatementSetter.class));
    Assert.assertEquals(targetDs, ThreadLocalProperties.get());
  }

  @Test
  public void koCall() {
    final String alarmPs = "insert into sentilo_alarms (alarm, message, timestamp, event_timestamp, published_at, publisher) values (?,?,?,?,?,?)";
    final BatchProcessWorker worker = new BatchProcessWorker(batchUpdateContext);
    doThrow(MockDataAccessException.class).when(jdbcTemplate).batchUpdate(eq(alarmPs), any(BatchPreparedStatementSetter.class));

    final BatchProcessResult result = worker.call();

    verify(callback).notifyBatchUpdateIsDone(result);
    verify(jdbcTemplate, times(8)).batchUpdate(anyString(), any(BatchPreparedStatementSetter.class));
    Assert.assertEquals(targetDs, ThreadLocalProperties.get());
    Assert.assertEquals(numMaxRetries, ReflectionTestUtils.getField(worker, "numRetries"));
  }

  @Test
  public void subList() {
    final List<Data> dataToPersist = buildMockList();

    final List<Data> orders = subList(dataToPersist, Order.class);
    final List<Data> alarms = subList(dataToPersist, Alarm.class);
    final List<Data> observations = subList(dataToPersist, Observation.class);

    Assert.assertTrue(orders.size() == 15);
    Assert.assertTrue(observations.size() == 10);
    Assert.assertTrue(alarms.size() == 5);
  }

  private List<Data> buildMockList() {
    final List<Data> mockElements = new ArrayList<Data>();

    for (int i = 0; i < 10; i++) {
      mockElements.add(new Observation());
    }

    for (int i = 0; i < 5; i++) {
      mockElements.add(new Alarm());
    }

    for (int i = 0; i < 15; i++) {
      mockElements.add(new Order());
    }

    return mockElements;
  }

  private List<Data> subList(final List<Data> elements, final Class<? extends Data> filterType) {
    final List<Data> result = new ArrayList<Data>();
    for (final Data data : elements) {

      if (filterType.isAssignableFrom(data.getClass())) {
        result.add(data);
      }
    }
    return result;
  }

  public class MockDataAccessException extends DataAccessException {

    private static final long serialVersionUID = 1L;
    private static final String MOCK_MESSAGE = "mock message";

    public MockDataAccessException() {
      super(MOCK_MESSAGE);
    }

  }
}
