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
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import javax.annotation.PostConstruct;

import org.sentilo.agent.relational.repository.RelationalAgentRepository;
import org.sentilo.agent.relational.repository.batch.BatchProcessContext;
import org.sentilo.agent.relational.repository.batch.BatchProcessMonitor;
import org.sentilo.agent.relational.repository.batch.BatchProcessWorker;
import org.sentilo.common.domain.EventMessage;
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
public class RelationalAgentRepositoryImpl implements RelationalAgentRepository {

  private static final Logger LOGGER = LoggerFactory.getLogger(RelationalAgentRepositoryImpl.class);

  private static final int DEFAULT_NUM_MAX_WORKERS = 5;
  private static final int DEFAULT_BATCH_SIZE = 10;
  private static final int DEFAULT_NUM_MAX_RETRIES = 1;

  @Value("${sentilo.agent.relational.tables.prefix:sentilo}")
  private String tablesPrefix;

  @Value("${sentilo.agent.batch.size:10}")
  private int batchSize;

  @Value("${sentilo.agent.batch.workers.size.min:0}")
  private int numMinWorkers;

  @Value("${sentilo.agent.batch.workers.size.max:5}")
  private int numMaxWorkers;

  @Value("${sentilo.agent.batch.max.retries:1}")
  private int numMaxRetries;

  @Autowired
  private PlatformTransactionManager platformTransactionManager;

  @Autowired
  private BatchProcessMonitor batchUpdateMonitor;

  private ExecutorService workersManager;

  private List<EventMessage> batchQueue = new ArrayList<EventMessage>();
  private final Lock lock = new ReentrantLock();

  @Autowired
  private JdbcTemplate jdbcTemplate;

  @PostConstruct
  public void init() {
    if (!StringUtils.hasText(tablesPrefix) || tablesPrefix.startsWith("$")) {
      throw new IllegalStateException(
          "Field tables_prefix is not initialized. Review your properties configuration and confirm that property sentilo.agent.relational.tables.prefix is defined");
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

    if (numMinWorkers > numMaxWorkers) {
      LOGGER.info("Field numMinWorkers is greater that numMaxWorkers. Setting numMinWorkers to {}", numMaxWorkers);
      numMinWorkers = numMaxWorkers;
    }

    // workersManager is a mixed ExcutorService between cached and fixed provided by Executors
    // class: it has a maximum number of threads(as Executors.newFixedThreadPool) and controls when
    // a thread is busy (as Executors.newCachedThreadPool)
    if (workersManager == null) {
      workersManager = new ThreadPoolExecutor(numMinWorkers, numMaxWorkers, 60L, TimeUnit.SECONDS, new LinkedBlockingQueue<Runnable>());
    }

    LOGGER.info("Initialize {} with the following properties: batchSize {},  numMaxRetries {}, numMinWorkers {} and numMaxWorkers {} ",
        this.getClass().getCanonicalName(), batchSize, numMaxRetries, numMinWorkers, numMaxWorkers);
  }

  @Override
  public void save(final EventMessage eventMessage) {
    addToQueue(eventMessage);
  }

  private void addToQueue(final EventMessage event) {
    List<EventMessage> eventsToPersist = null;
    lock.lock();
    try {
      batchQueue.add(event);

      if (batchQueue.size() >= batchSize) {
        eventsToPersist = batchQueue;
        batchQueue = new ArrayList<EventMessage>();
      }
    } finally {
      lock.unlock();
      if (eventsToPersist != null) {
        flushToRelational(buildBatchContext(eventsToPersist));
      }
    }
  }

  /**
   * Clear all batch queues and send pending data to persist.
   */
  @Override
  public void flush() {
    LOGGER.info("Call to flush pending tasks");
    lock.lock();
    try {
      if (!CollectionUtils.isEmpty(batchQueue)) {
        LOGGER.info("Flush {} elements", batchQueue.size());
        final BatchProcessContext context = buildBatchContext(batchQueue);
        final BatchProcessWorker worker = new BatchProcessWorker(context);
        worker.call();
      }
    } finally {
      lock.unlock();
      LOGGER.info("Flush process finished");
    }
  }

  private BatchProcessContext buildBatchContext(final List<EventMessage> eventsToPersist) {
    return new BatchProcessContext(eventsToPersist, jdbcTemplate, platformTransactionManager, numMaxRetries, tablesPrefix, batchUpdateMonitor);
  }

  private void flushToRelational(final BatchProcessContext batchUpdateContext) {
    // Assign flush task to a busy worker
    workersManager.submit(new BatchProcessWorker(batchUpdateContext));
    LOGGER.debug("Scheduling batch update task for persist {} elements", batchUpdateContext.getDataToPersist().size());
  }

}
