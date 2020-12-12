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
package org.sentilo.agent.metrics.monitor.repository.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import javax.annotation.PostConstruct;

import org.sentilo.agent.metrics.monitor.repository.MetricsMonitorRepository;
import org.sentilo.agent.metrics.monitor.repository.batch.BatchProcessContext;
import org.sentilo.agent.metrics.monitor.repository.batch.BatchProcessWorker;
import org.sentilo.common.converter.DefaultStringMessageConverter;
import org.sentilo.common.converter.StringMessageConverter;
import org.sentilo.common.rest.RESTClient;
import org.sentilo.common.rest.RequestContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Repository;
import org.springframework.util.CollectionUtils;

@Repository
public class MetricsMonitorRepositoryImpl implements MetricsMonitorRepository {

  private static final Logger LOGGER = LoggerFactory.getLogger(MetricsMonitorRepositoryImpl.class);

  private static final int DEFAULT_NUM_MAX_WORKERS = 5;
  private static final int DEFAULT_BATCH_SIZE = 1;
  private static final int DEFAULT_NUM_MAX_RETRIES = 1;

  @Value("${batch.size}")
  private int batchSize;

  @Value("${batch.workers.size}")
  private int numMaxWorkers;

  @Value("${batch.max.retries}")
  private int numMaxRetries;

  @Autowired
  private RESTClient restClient;

  private ExecutorService workersManager;

  private final Lock lock = new ReentrantLock();
  private List<String> batchQueue = new ArrayList<>();

  private String esVersion;

  @PostConstruct
  public void init() {

    if (numMaxWorkers == 0) {
      numMaxWorkers = DEFAULT_NUM_MAX_WORKERS;
    }

    if (batchSize == 0) {
      batchSize = DEFAULT_BATCH_SIZE;
    }

    if (numMaxRetries == 0) {
      numMaxRetries = DEFAULT_NUM_MAX_RETRIES;
    }

    // workersManager is a mixed ExcutorService between cached and fixed provided by Executors
    // class: it has a maximum number of threads(as Executors.newFixedThreadPool) and controls when
    // a thread is busy (as Executors.newCachedThreadPool)
    if (workersManager == null) {
      workersManager = new ThreadPoolExecutor(0, numMaxWorkers, 60L, TimeUnit.SECONDS, new LinkedBlockingQueue<Runnable>());
    }

    esVersion = getElasticVersion();

    LOGGER.info("Initialized ActivityMonitorRepositoryImpl with the following properties: batchSize {},  numMaxRetries {} and numMaxWorkers {} ",
        batchSize, numMaxRetries, numMaxWorkers);

  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.agent.metrics.monitor.repository.MetricsMonitorRepository#
   * publishMessageToElasticSearch(java.lang.String)
   */
  @Override
  public void publishMessageToElasticSearch(final String metrics) {
    addToQueue(metrics);
  }

  private void addToQueue(final String metrics) {
    List<String> metricsToIndex = null;
    lock.lock();
    try {
      batchQueue.add(metrics);

      if (batchQueue.size() >= batchSize) {
        metricsToIndex = batchQueue;
        batchQueue = new ArrayList<>();
      }
    } finally {
      lock.unlock();
      if (metricsToIndex != null) {
        flushToElasticSearch(new BatchProcessContext(metricsToIndex, restClient, numMaxRetries, esVersion));
      }
    }
  }

  private void flushToElasticSearch(final BatchProcessContext batchProcessContext) {
    // Assign flush task to a busy worker
    workersManager.submit(new BatchProcessWorker(batchProcessContext));
    LOGGER.debug("Scheduled batch process task for index {} elements in elasticsearch ", batchProcessContext.getMetricsToProcess().size());
  }

  /**
   * Clear batch queue and send to index pending events.
   */
  public void flush() {
    LOGGER.info("Call to flush pending tasks");
    lock.lock();
    try {
      if (!CollectionUtils.isEmpty(batchQueue)) {
        LOGGER.info("Flushing {} elements to elasticsearch", batchQueue.size());
        final BatchProcessContext context = new BatchProcessContext(batchQueue, restClient, numMaxRetries, esVersion);
        final BatchProcessWorker worker = new BatchProcessWorker(context);
        worker.call();
      }
    } finally {
      lock.unlock();
      LOGGER.info("Flush process finished");
    }
  }

  @SuppressWarnings({"unused", "rawtypes"})
  private String getElasticVersion() {
    final StringMessageConverter converter = new DefaultStringMessageConverter();
    final String response = restClient.get(new RequestContext("/"));
    final Map obj = (Map) converter.unmarshal(response, Map.class);
    final String version = (String) ((Map) obj.get("version")).get("number");
    return version;
  }
}
