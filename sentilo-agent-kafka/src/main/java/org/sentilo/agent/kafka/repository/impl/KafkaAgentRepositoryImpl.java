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
package org.sentilo.agent.kafka.repository.impl;

import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import javax.annotation.PostConstruct;

import org.sentilo.agent.kafka.repository.KafkaAgentRepository;
import org.sentilo.agent.kafka.repository.ProcessContext;
import org.sentilo.agent.kafka.repository.ProcessMonitor;
import org.sentilo.agent.kafka.repository.ProcessWorker;
import org.sentilo.common.domain.EventMessage;
// import org.sentilo.common.rest.RESTClient;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Repository;

@Repository
public class KafkaAgentRepositoryImpl implements KafkaAgentRepository {

  private static final Logger LOGGER = LoggerFactory.getLogger(KafkaAgentRepositoryImpl.class);

  private static final int DEFAULT_NUM_MIN_WORKERS = 0;
  private static final int DEFAULT_NUM_MAX_WORKERS = 5;
  private static final int DEFAULT_NUM_MAX_RETRIES = 1;

  @Value("${batch.workers.size.min}")
  private int numMinWorkers;

  @Value("${batch.workers.size.max}")
  private int numMaxWorkers;

  @Value("${batch.max.retries:0}")
  private int numMaxRetries;

  @Value("${kafka.request.timeout.ms}")
  private long kafkaTimeout;

  @Autowired
  private ProcessMonitor streamProcessMonitor;

  @Autowired
  private KafkaTemplate<String, String> kafkaTemplate;

  private ThreadPoolExecutor workersManager;

  @PostConstruct
  public void init() {

    if (numMinWorkers == 0) {
      numMinWorkers = DEFAULT_NUM_MIN_WORKERS;
    }

    if (numMaxWorkers == 0) {
      numMaxWorkers = DEFAULT_NUM_MAX_WORKERS;
    }

    if (numMaxRetries == 0) {
      numMaxRetries = DEFAULT_NUM_MAX_RETRIES;
    }

    // workersManager is a mixed ExecutorService between cached and fixed provided by Executors
    // class: it has a maximum number of threads(as Executors.newFixedThreadPool) and controls when
    // a thread is busy (as Executors.newCachedThreadPool)
    if (workersManager == null) {
      if (numMinWorkers > numMaxWorkers) {
        LOGGER.info("Property batch.workers.size.min is greater that batch.workers.size.max. Setting batch.workers.size.min to {}", numMaxWorkers);
        numMinWorkers = numMaxWorkers;
      }

      workersManager = new ThreadPoolExecutor(numMinWorkers, numMaxWorkers, 60L, TimeUnit.SECONDS, new LinkedBlockingQueue<Runnable>());
    }

    LOGGER.info("Initialized KafkaAgentRepositoryImpl with the following properties: numMaxRetries {} and numMaxWorkers {} ", numMaxRetries,
        numMaxWorkers);

  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.agent.kafka.repository.ActivityMonitorRepository#
   * publishMessageToKafka(org.sentilo.common.domain.EventMessage)
   */
  public void publishMessageToKafka(final EventMessage event) {

    final ProcessContext processContext = new ProcessContext(event, numMaxRetries, streamProcessMonitor, kafkaTemplate);

    workersManager.submit(new ProcessWorker(processContext));
    LOGGER.debug("Scheduling batch process task for put an element to Kafka.");
  }

  public void flush() {

    LOGGER.debug("Call to flush pending tasks");

    try {
      if (workersManager == null) {
        workersManager.awaitTermination(kafkaTimeout + 1000L, TimeUnit.MILLISECONDS);
      }
    } catch (final InterruptedException e) {
      LOGGER.error("Could not flush event(s) to Kafka correctly: thread timeout exceed.");
    } finally {
      LOGGER.info("Flush process finished");
    }

  }

  @Scheduled(initialDelay = 60000, fixedDelay = 60000 * 5)
  public void writeState() {
    LOGGER.info(" ---- ProcessMonitor ---- ");
    final StringBuilder sb = new StringBuilder("\n workersManager Pools size: {}");
    sb.append("\n workersManager Task queue size: {}");
    sb.append("\n workersManager Task Count: {}");
    sb.append("\n workersManager Completed Task Count: {}");
    LOGGER.info(sb.toString(), workersManager.getPoolSize(), workersManager.getQueue().size(), workersManager.getTaskCount(),
        workersManager.getCompletedTaskCount());
  }

}
