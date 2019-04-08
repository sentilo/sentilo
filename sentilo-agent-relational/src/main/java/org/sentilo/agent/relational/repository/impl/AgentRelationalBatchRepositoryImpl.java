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
package org.sentilo.agent.relational.repository.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import javax.annotation.PostConstruct;
import javax.annotation.Resource;

import org.apache.tomcat.jdbc.pool.DataSource;
import org.sentilo.agent.relational.domain.Alarm;
import org.sentilo.agent.relational.domain.Data;
import org.sentilo.agent.relational.domain.Observation;
import org.sentilo.agent.relational.domain.Order;
import org.sentilo.agent.relational.repository.AgentRelationalRepository;
import org.sentilo.agent.relational.repository.batch.BatchProcessContext;
import org.sentilo.agent.relational.repository.batch.BatchProcessMonitor;
import org.sentilo.agent.relational.repository.batch.BatchProcessWorker;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

@Repository
public class AgentRelationalBatchRepositoryImpl implements AgentRelationalRepository {

  private static final Logger LOGGER = LoggerFactory.getLogger(AgentRelationalBatchRepositoryImpl.class);

  private static final int DEFAULT_NUM_MAX_WORKERS = 5;
  private static final int DEFAULT_BATCH_SIZE = 10;
  private static final int DEFAULT_NUM_MAX_RETRIES = 1;

  @Value("${relational.tables.prefix}")
  private String tablesPrefix;

  @Value("${relational.batch.size}")
  private int batchSize;

  @Value("${relational.batch.workers.size}")
  private int numMaxWorkers;

  @Value("${relational.batch.max.retries}")
  private int numMaxRetries;

  @Autowired
  private PlatformTransactionManager platformTransactionManager;

  @Autowired
  private BatchProcessMonitor batchUpdateMonitor;

  private ExecutorService workersManager;

  private Map<String, List<Data>> batchQueues;
  private final Lock lock = new ReentrantLock();

  @Autowired
  private JdbcTemplate jdbcTemplate;

  @PostConstruct
  public void init() {
    if (!StringUtils.hasText(tablesPrefix) || tablesPrefix.startsWith("$")) {
      throw new IllegalStateException(
          "Field tables_prefix is not initialized. Review your properties configuration and confirm that property relational.tables.prefix is defined");
    }

    if (numMaxWorkers == 0) {
      numMaxWorkers = DEFAULT_NUM_MAX_WORKERS;
    }

    if (batchSize == 0) {
      batchSize = DEFAULT_BATCH_SIZE;
    }

    if (numMaxRetries == 0) {
      numMaxRetries = DEFAULT_NUM_MAX_RETRIES;
    }

    LOGGER.info("Initialize AgentRelationalBatchRepositoryImpl with the following properties: batchSize {},  numMaxRetries {} and numMaxWorkers {} ",
        batchSize, numMaxRetries, numMaxWorkers);

    // workersManager is a mixed ExcutorService between cached and fixed provided by Executors
    // class: it has a maximum number of threads(as Executors.newFixedThreadPool) and controls when
    // a thread is busy (as Executors.newCachedThreadPool)
    workersManager = new ThreadPoolExecutor(0, numMaxWorkers, 60L, TimeUnit.SECONDS, new LinkedBlockingQueue<Runnable>());
  }

  @Resource
  public void setDataSources(final Map<String, DataSource> dataSources) {
    // For every Ds this method creates and initializes a new queue and associates it with the
    // dataSource key.
    if (!CollectionUtils.isEmpty(dataSources)) {
      LOGGER.debug("Number of queues to register: {}", dataSources.size());
      batchQueues = new HashMap<String, List<Data>>();

      for (final String key : dataSources.keySet()) {
        LOGGER.debug("Registering queue for dataSource {}", key);
        batchQueues.put(key, new ArrayList<Data>());
      }
    }
  }

  @Override
  public void save(final Observation observation) {
    addToQueue(observation);
  }

  @Override
  public void save(final Alarm alarm) {
    addToQueue(alarm);
  }

  @Override
  public void save(final Order order) {
    addToQueue(order);
  }

  /**
   * Clear all batch queues and send pending data to persist.
   */
  public void flush() {
    LOGGER.info("Call to flush pending tasks");
    lock.lock();
    try {
      final Iterator<String> it = batchQueues.keySet().iterator();
      while (it.hasNext()) {
        final String targetDs = it.next();
        final List<Data> targetQueue = batchQueues.get(targetDs);
        if (!CollectionUtils.isEmpty(targetQueue)) {
          LOGGER.info("Flush {} elements to Ds {}", targetQueue.size(), targetDs);
          final BatchProcessContext context = buildBatchContext(targetQueue, targetDs);
          final BatchProcessWorker worker = new BatchProcessWorker(context);
          worker.call();
        }
      }
    } finally {
      lock.unlock();
      LOGGER.info("Flush process finished");
    }
  }

  private void addToQueue(final Data data) {
    List<Data> dataToPersist = null;
    lock.lock();
    try {
      if (StringUtils.hasText(data.getTargetDs()) && !CollectionUtils.isEmpty(batchQueues)) {
        final List<Data> targetQueue = batchQueues.get(data.getTargetDs());
        targetQueue.add(data);

        if (targetQueue.size() >= batchSize) {
          dataToPersist = targetQueue;
          batchQueues.put(data.getTargetDs(), new ArrayList<Data>());
        }
      } else {
        LOGGER.warn("Not found batch list for this targetDs {}. Data will not be persist.", data.getTargetDs());
      }
    } finally {
      lock.unlock();
      if (dataToPersist != null) {
        flushToRelational(buildBatchContext(dataToPersist, data.getTargetDs()));
      }
    }
  }

  private BatchProcessContext buildBatchContext(final List<Data> dataToPersist, final String targetDs) {
    return new BatchProcessContext(dataToPersist, jdbcTemplate, platformTransactionManager, numMaxRetries, tablesPrefix, targetDs,
        batchUpdateMonitor);
  }

  private void flushToRelational(final BatchProcessContext batchUpdateContext) {
    // Assign flush task to a busy worker
    workersManager.submit(new BatchProcessWorker(batchUpdateContext));
    LOGGER.debug("Scheduling batch update task for persist {} elements in dataSource {}", batchUpdateContext.getDataToPersist().size(),
        batchUpdateContext.getTargetDs());
  }

}
