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
package org.sentilo.agent.relational.repository.batch;

import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.stream.Collectors;

import org.sentilo.common.domain.EventMessage;
import org.sentilo.common.enums.EventType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.jdbc.core.BatchPreparedStatementSetter;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.transaction.support.TransactionTemplate;
import org.springframework.util.CollectionUtils;

public class BatchProcessWorker implements Callable<BatchProcessResult> {

  private static final Logger LOGGER = LoggerFactory.getLogger(BatchProcessWorker.class);
  private static final String INSERT_CMD_PREFIX = "insert into ";

  private String observationPs;
  private String orderPs;
  private String alarmPs;

  private final List<EventMessage> dataToPersist;
  private final JdbcTemplate jdbcTemplate;
  private final TransactionTemplate transactionTemplate;
  private final BatchProcessCallback callback;

  private int numRetries;
  private final int numMaxRetries;

  public BatchProcessWorker(final BatchProcessContext batchUpdateContext) {
    buildQueries(batchUpdateContext.getTablesPrefix());
    dataToPersist = batchUpdateContext.getDataToPersist();
    jdbcTemplate = batchUpdateContext.getJdbcTemplate();
    transactionTemplate = new TransactionTemplate(batchUpdateContext.getTransactionManager());
    numMaxRetries = batchUpdateContext.getNumMaxRetries();
    callback = batchUpdateContext.getCallback();
  }

  private void buildQueries(final String tablesPrefix) {
    observationPs = INSERT_CMD_PREFIX + tablesPrefix
        + "_observations (provider, component, component_type, sensor, sensor_type, location, value, timestamp, event_timestamp, published_at, publisher, tenant, publisher_tenant) values (?,?,?,?,?,?,?,?,?,?,?,?,?)";
    orderPs = INSERT_CMD_PREFIX + tablesPrefix
        + "_orders (provider, component, component_type, sensor, sensor_type, location, message, timestamp, event_timestamp, published_at, publisher, tenant, publisher_tenant) values (?,?,?,?,?,?,?,?,?,?,?,?,?)";
    alarmPs = INSERT_CMD_PREFIX + tablesPrefix
        + "_alarms (alert, alert_type, provider, component, component_type, sensor, sensor_type, location, message, timestamp, event_timestamp, published_at, publisher, tenant, publisher_tenant) values (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)";
  }

  @Override
  public BatchProcessResult call() {
    LOGGER.info("Init batch update process. Data elements to persist: {} ", dataToPersist.size());

    // Split dataToPersist in 3 groups: observations, alarms and orders
    // All run in a global transaction and every group will be persisted via a batch update call
    final List<EventMessage> orders = subList(dataToPersist, EventType.ORDER);
    final List<EventMessage> alarms = subList(dataToPersist, EventType.ALARM);
    final List<EventMessage> observations = subList(dataToPersist, EventType.DATA);

    final int numElementsPersisted = doBatchUpdate(orders, alarms, observations);
    LOGGER.info("Number of elements persisted: {}", numElementsPersisted);

    final BatchProcessResult result = new BatchProcessResult(dataToPersist, numElementsPersisted);

    callback.notifyBatchUpdateIsDone(result);
    return result;
  }

  private int doBatchUpdate(final List<EventMessage> orders, final List<EventMessage> alarms, final List<EventMessage> observations) {

    int numElementsPersisted = 0;
    LOGGER.debug("Orders to persist: {}", orders.size());
    LOGGER.debug("Alarms to persist: {}", alarms.size());
    LOGGER.debug("Observations to persist: {}", observations.size());

    try {
      numElementsPersisted = doBatchUpdateInTransacion(orders, alarms, observations);
    } catch (final Exception dae) {
      LOGGER.warn("Error executing batch update:", dae);
      if (numRetries < numMaxRetries) {
        numRetries++;
        doBatchUpdate(orders, alarms, observations);
      } else {
        LOGGER.error("Number of retries {} is greater or equals than the maximum number of retries configured {}. "
            + "Events to persist will be stored in the pending queue for further processing.", numRetries, numMaxRetries);
      }
    }

    return numElementsPersisted;
  }

  private Integer doBatchUpdateInTransacion(final List<EventMessage> orders, final List<EventMessage> alarms, final List<EventMessage> observations) {
    LOGGER.debug("Num of retry: {}", numRetries);
    return transactionTemplate.execute(status ->
      {
        final int ordersPersisted = CollectionUtils.isEmpty(orders) ? 0 : persistOrders(orders);
        final int alarmsPersisted = CollectionUtils.isEmpty(alarms) ? 0 : persistAlarms(alarms);
        final int observationsPersisted = CollectionUtils.isEmpty(observations) ? 0 : persistObservations(observations);

        LOGGER.debug("Orders inserted: {}", ordersPersisted);
        LOGGER.debug("Alarms inserted: {}", alarmsPersisted);
        LOGGER.debug("Observations inserted: {}", observationsPersisted);

        return ordersPersisted + alarmsPersisted + observationsPersisted;
      });
  }

  private int persistOrders(final List<EventMessage> orders) {

    final BatchPreparedStatementSetter bpss = new BatchPreparedStatementSetter() {

      @Override
      public void setValues(final PreparedStatement ps, final int i) throws SQLException {
        final EventMessage order = orders.get(i);
        int j = 1;
        ps.setString(j++, order.getProvider());
        ps.setString(j++, order.getComponent());
        ps.setString(j++, order.getComponentType());
        ps.setString(j++, order.getSensor());
        ps.setString(j++, order.getSensorType());
        ps.setString(j++, order.getLocation());
        ps.setString(j++, order.getMessage());
        ps.setString(j++, order.getTimestamp());
        ps.setTimestamp(j++, new java.sql.Timestamp(order.getTime()));
        ps.setTimestamp(j++, new java.sql.Timestamp(order.getPublishedAt()));
        ps.setString(j++, order.getPublisher());
        ps.setString(j++, order.getTenant());
        ps.setString(j++, order.getPublisherTenant());
      }

      @Override
      public int getBatchSize() {
        return orders.size();
      }
    };

    jdbcTemplate.batchUpdate(orderPs, bpss);

    return orders.size();
  }

  private int persistAlarms(final List<EventMessage> alarms) {
    final BatchPreparedStatementSetter bpss = new BatchPreparedStatementSetter() {

      @Override
      public void setValues(final PreparedStatement ps, final int i) throws SQLException {
        final EventMessage alarm = alarms.get(i);
        int j = 1;
        ps.setString(j++, alarm.getAlert());
        ps.setString(j++, alarm.getAlertType());
        ps.setString(j++, alarm.getProvider());
        ps.setString(j++, alarm.getComponent());
        ps.setString(j++, alarm.getComponentType());
        ps.setString(j++, alarm.getSensor());
        ps.setString(j++, alarm.getSensorType());
        ps.setString(j++, alarm.getLocation());
        ps.setString(j++, alarm.getMessage());
        ps.setString(j++, alarm.getTimestamp());
        ps.setTimestamp(j++, new java.sql.Timestamp(alarm.getTime()));
        ps.setTimestamp(j++, new java.sql.Timestamp(alarm.getPublishedAt()));
        ps.setString(j++, alarm.getPublisher());
        ps.setString(j++, alarm.getTenant());
        ps.setString(j++, alarm.getPublisherTenant());
      }

      @Override
      public int getBatchSize() {
        return alarms.size();
      }
    };

    jdbcTemplate.batchUpdate(alarmPs, bpss);

    return alarms.size();
  }

  private int persistObservations(final List<EventMessage> observations) {
    final BatchPreparedStatementSetter bpss = new BatchPreparedStatementSetter() {

      @Override
      public void setValues(final PreparedStatement ps, final int i) throws SQLException {
        final EventMessage observation = observations.get(i);
        int j = 1;
        ps.setString(j++, observation.getProvider());
        ps.setString(j++, observation.getComponent());
        ps.setString(j++, observation.getComponentType());
        ps.setString(j++, observation.getSensor());
        ps.setString(j++, observation.getSensorType());
        ps.setString(j++, observation.getLocation());
        ps.setString(j++, observation.getMessage());
        ps.setString(j++, observation.getTimestamp());
        ps.setTimestamp(j++, new java.sql.Timestamp(observation.getTime()));
        ps.setTimestamp(j++, new java.sql.Timestamp(observation.getPublishedAt()));
        ps.setString(j++, observation.getPublisher());
        ps.setString(j++, observation.getTenant());
        ps.setString(j++, observation.getPublisherTenant());
      }

      @Override
      public int getBatchSize() {
        return observations.size();
      }
    };

    jdbcTemplate.batchUpdate(observationPs, bpss);

    return observations.size();
  }

  private List<EventMessage> subList(final List<EventMessage> elements, final EventType eventType) {
    return elements.stream().filter(event -> event.getType().equals(eventType.name())).collect(Collectors.toList());
  }

}
