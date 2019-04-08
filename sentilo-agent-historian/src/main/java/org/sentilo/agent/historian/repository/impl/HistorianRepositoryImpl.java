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
package org.sentilo.agent.historian.repository.impl;

import java.text.ParseException;
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

import org.sentilo.agent.historian.repository.HistorianRepository;
import org.sentilo.agent.historian.repository.batch.BatchProcessContext;
import org.sentilo.agent.historian.repository.batch.BatchProcessMonitor;
import org.sentilo.agent.historian.repository.batch.BatchProcessWorker;
import org.sentilo.agent.historian.utils.OpenTSDBValueConverter;
import org.sentilo.common.domain.EventMessage;
import org.sentilo.common.rest.RESTClient;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Repository;
import org.springframework.util.CollectionUtils;

@Repository
public class HistorianRepositoryImpl implements HistorianRepository {

  private static final Logger LOGGER = LoggerFactory.getLogger(HistorianRepositoryImpl.class);

  private static final int DEFAULT_NUM_MAX_WORKERS = 5;
  private static final int DEFAULT_BATCH_SIZE = 10;
  private static final int DEFAULT_NUM_MAX_RETRIES = 1;

  @Value("${batch.size}")
  private int batchSize;

  @Value("${batch.workers.size}")
  private int numMaxWorkers;

  @Value("${batch.max.retries}")
  private int numMaxRetries;

  @Autowired
  private RESTClient restClient;
  @Autowired
  private BatchProcessMonitor batchProcessMonitor;

  private ExecutorService workersManager;

  private final Lock lock = new ReentrantLock();
  private List<EventMessage> batchQueue = new ArrayList<EventMessage>();

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

    // workersManager is a mixed ExecutorService between cached and fixed provided by Executors
    // class: it has a maximum number of threads(as Executors.newFixedThreadPool) and controls when
    // a thread is busy (as Executors.newCachedThreadPool)
    if (workersManager == null) {
      workersManager = new ThreadPoolExecutor(0, numMaxWorkers, 60L, TimeUnit.SECONDS, new LinkedBlockingQueue<Runnable>());
    }

    LOGGER.info("Initialized HistorianRepositoryImpl with the following properties: batchSize {},  numMaxRetries {} and numMaxWorkers {} ", batchSize,
        numMaxRetries, numMaxWorkers);

  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.agent.historian.repository.ActivityMonitorRepository#
   * publishMessageToOpenTSDB(org.sentilo.common.domain.EventMessage)
   */
  public void publishMessageToOpenTSDB(final EventMessage event) {
    addToQueue(event);
  }

  private void addToQueue(final EventMessage event) {
    List<EventMessage> eventsToExport = null;
    lock.lock();
    try {

      // Let's see if the value is json
      if (OpenTSDBValueConverter.isComplexValue(event.getMessage())) {
        addComplexValueToQueue(event);
      } else {
        addSimpleValueToQueue(event);
      }

      // Complex json values can produce that batchQueue is greater than batchSize.
      // In this case we'll send batchSize to OpenTsdb and we'll store the rest for future.
      if (batchQueue.size() >= batchSize) {
        eventsToExport = batchQueue.subList(0, batchSize);
        batchQueue = new ArrayList<EventMessage>(batchQueue.subList(batchSize, batchQueue.size()));
      }

    } finally {
      lock.unlock();
      if (eventsToExport != null) {
        flushToOpenTSDB(new BatchProcessContext(eventsToExport, restClient, numMaxRetries, batchProcessMonitor));
      }
    }
  }

  private void flushToOpenTSDB(final BatchProcessContext batchProcessContext) {
    // Assign flush task to a busy worker
    workersManager.submit(new BatchProcessWorker(batchProcessContext));
    LOGGER.debug("Scheduling batch process task for put {} elements to OpenTSDB ", batchProcessContext.getEventsToProcess().size());
  }

  private void addComplexValueToQueue(final EventMessage event) {
    // Flatten JSON message into N measures
    final String metricName = OpenTSDBValueConverter.createMetricName(event);
    final Map<String, Object> unfoldValues = OpenTSDBValueConverter.extractMeasuresFromComplexType(metricName, event.getMessage());
    for (final Map.Entry<String, Object> e : unfoldValues.entrySet()) {
      final EventMessage newEvent = new EventMessage();
      BeanUtils.copyProperties(event, newEvent);
      newEvent.setTopic(e.getKey());
      newEvent.setMessage(e.getValue().toString());
      batchQueue.add(newEvent);
    }
  }

  private void addSimpleValueToQueue(final EventMessage event) {
    // The value should be long, float or boolean
    try {
      final Object numericValue = OpenTSDBValueConverter.getSimpleValue(event.getMessage());
      event.setMessage(numericValue.toString());
      batchQueue.add(event);

    } catch (final ParseException e) {
      // Probably String or some non-numeric value that we cannot store in OpenTSDB. Pass
      return;
    }
  }

  /**
   * Clear batch queue and send pending events.
   */
  public void flush() {
    LOGGER.debug("Call to flush pending tasks");
    lock.lock();
    try {
      if (!CollectionUtils.isEmpty(batchQueue)) {
        LOGGER.info("Flush {} elements to opentsdb", batchQueue.size());
        final BatchProcessContext context = new BatchProcessContext(batchQueue, restClient, numMaxRetries, batchProcessMonitor);
        final BatchProcessWorker worker = new BatchProcessWorker(context);
        worker.call();
      }
    } finally {
      lock.unlock();
      LOGGER.info("Flush process finished");
    }
  }

}
