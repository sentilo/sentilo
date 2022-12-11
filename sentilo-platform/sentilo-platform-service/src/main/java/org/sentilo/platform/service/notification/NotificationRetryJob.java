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
package org.sentilo.platform.service.notification;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;

import org.sentilo.common.converter.DefaultStringMessageConverter;
import org.sentilo.common.converter.StringMessageConverter;
import org.sentilo.common.domain.EventMessage;
import org.sentilo.common.factory.SentiloThreadFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

@Component
public class NotificationRetryJob {

  private static final Logger LOGGER = LoggerFactory.getLogger(NotificationRetryJob.class);

  private final StringMessageConverter converter = new DefaultStringMessageConverter();

  @Value("${sentilo.server.api.retry.batch.worker.size:50}")
  private int batchWorkerSize;

  @Value("${sentilo.server.api.retry.workers.size:5}")
  private int numWorkers;

  @Value("${sentilo.server.api.retry.notifications:true}")
  private boolean retryNotificationsEnabled = true;

  @Autowired
  private NotificationDeliveryService deliveryService;

  @Autowired
  private NotificationRetryRepository repository;

  private ExecutorService workersManager;

  @PostConstruct
  public void init() {
    if (retryNotificationsEnabled) {
      LOGGER.info("Initialize NotificationRetryJob with the following properties: batchWorkerSize {} and numWorkers {}", batchWorkerSize, numWorkers);

      // workersManager is a mixed ExecutorService between cached and fixed provided by Executors
      // class: it has a maximum number of threads(as Executors.newFixedThreadPool) and controls
      // when a thread is busy (as Executors.newCachedThreadPool)
      workersManager = new ThreadPoolExecutor(numWorkers, numWorkers, 10L, TimeUnit.SECONDS, new LinkedBlockingQueue<Runnable>(),
          new SentiloThreadFactory("API Server", "Notification-retry-worker"));
    }
  }

  @PreDestroy
  public void shutdown() {
    if (retryNotificationsEnabled) {
      LOGGER.info("Initializing NotificationRetryJob shutdown");
      workersManager.shutdown();
      LOGGER.info("Shutdown finished");
    }
  }

  @Scheduled(initialDelay = 120000, fixedDelay = 60000)
  // @SchedulerLock(name =
  // "org.sentilo.platform.service.notification.NotificationRetryJob.retryNotifications")
  public void retryNotifications() {
    if (retryNotificationsEnabled) {
      LOGGER.debug("NotificationRetryJob pool state: {}", ((ThreadPoolExecutor) workersManager).toString());
      final long initTs = System.currentTimeMillis();
      // Get number of events to retry that should be delivered now
      long totalPendingEvents = repository.getTotalEventsToRetry(initTs);
      // Number of events which could be processed by each worker is limited to no penalize main
      // flow
      final long maxEventsToProcess = numWorkers * batchWorkerSize;

      while (totalPendingEvents > 0) {
        final long totalEventsToProcess = Math.min(totalPendingEvents, maxEventsToProcess);
        LOGGER.info("Number of events to retry: {}", totalEventsToProcess);
        final List<NotificationRetryEvent> retryEvents = repository.getEventsToRetry(initTs, totalEventsToProcess);

        // Distribute retryEvents load between the maximum number of workers to improve retry
        // process. Each worker will be responsible of publish a collection of retry events
        final List<List<NotificationRetryEvent>> workersEvents = distributeLoadBetweenWorkers(retryEvents);

        final CountDownLatch doneSignal = new CountDownLatch(workersEvents.size());
        final List<Future<List<NotificationRetryEvent>>> workersJobs = new ArrayList<Future<List<NotificationRetryEvent>>>();

        if (!CollectionUtils.isEmpty(workersEvents)) {
          for (final List<NotificationRetryEvent> workerEvents : workersEvents) {
            workersJobs.add(workersManager.submit(new NotificationRetryWorker(workerEvents, deliveryService, doneSignal)));
          }
        }

        // Await until all workers are finished and verify each partial result
        final List<NotificationRetryEvent> noProcessedEvents = new ArrayList<NotificationRetryEvent>(retryEvents);
        verifyResultWorkerJobs(doneSignal, workersJobs, noProcessedEvents);

        // Finally save no processed events to retry later
        repository.save(noProcessedEvents);

        LOGGER.info("Finished retry notifications process. Delivered events: [{}]  / Total events: [{}]. Running time (millis) = {}. ",
            retryEvents.size() - noProcessedEvents.size(), retryEvents.size(), System.currentTimeMillis() - initTs);

        totalPendingEvents = repository.getTotalEventsToRetry(initTs);
      }

    }
  }

  private void verifyResultWorkerJobs(final CountDownLatch doneSignal, final List<Future<List<NotificationRetryEvent>>> workersJobs,
      final List<NotificationRetryEvent> noProcessedEvents) {
    try {
      doneSignal.await();

      for (final Future<List<NotificationRetryEvent>> workerJob : workersJobs) {
        try {
          final List<NotificationRetryEvent> processedEventsJob = workerJob.get();
          noProcessedEvents.removeAll(processedEventsJob);
        } catch (final Exception e) {
          LOGGER.info("Error getting response from retry worker job. These events will be retried later");
        }
      }
    } catch (final InterruptedException ie) {
      // This error could be ignored
    }
  }

  /**
   * Generates N lists of retryEvents (N==numWorkers), and each one will be assigned to one worker.
   *
   * @param retryEvents
   * @return
   */
  private List<List<NotificationRetryEvent>> distributeLoadBetweenWorkers(final List<NotificationRetryEvent> retryEvents) {
    final List<List<NotificationRetryEvent>> workersEvents = new ArrayList<List<NotificationRetryEvent>>();

    for (int i = 0; i < retryEvents.size(); i++) {
      final NotificationRetryEvent retryEvent = retryEvents.get(i);
      // pos indicates to which worker should be assigned retryEvent
      final int pos = i % numWorkers;
      if (workersEvents.size() < pos + 1) {
        workersEvents.add(new ArrayList<NotificationRetryEvent>());
      }
      workersEvents.get(pos).add(retryEvent);
    }

    return workersEvents;
  }

  class NotificationRetryWorker implements Callable<List<NotificationRetryEvent>> {

    private List<NotificationRetryEvent> eventsToRetry;
    private List<NotificationRetryEvent> eventsProcessed;
    private NotificationDeliveryService deliveryService;
    private CountDownLatch doneSignal;

    public NotificationRetryWorker(final NotificationRetryEvent retryEvent, final NotificationDeliveryService deliveryService,
        final CountDownLatch doneSignal) {
      this(Collections.singletonList(retryEvent), deliveryService, doneSignal);
    }

    public NotificationRetryWorker(final List<NotificationRetryEvent> eventsToRetry, final NotificationDeliveryService deliveryService,
        final CountDownLatch doneSignal) {
      this.deliveryService = deliveryService;
      this.eventsToRetry = eventsToRetry;
      this.doneSignal = doneSignal;
      eventsProcessed = new ArrayList<NotificationRetryEvent>();
    }

    @Override
    public List<NotificationRetryEvent> call() {
      try {
        for (final NotificationRetryEvent notificationRetryEvent : eventsToRetry) {
          if (processNotificationRetry(notificationRetryEvent)) {
            eventsProcessed.add(notificationRetryEvent);
          }
        }
      } finally {
        doneSignal.countDown();
      }

      LOGGER.info("Worker: number of delivered events: [{}]  / Total number of events: [{}] ", eventsProcessed.size(), eventsToRetry.size());
      return eventsProcessed;
    }

    private boolean processNotificationRetry(final NotificationRetryEvent notificationRetryEvent) {
      boolean processedOk = true;
      try {
        updateRedeliveryFields(notificationRetryEvent);
        deliveryService.pushNotification(notificationRetryEvent);
      } catch (final Exception e) {
        LOGGER.warn("Error while processing notification retry. Retry will be attempted again later");
        processedOk = false;
      }

      return processedOk;
    }

    private void updateRedeliveryFields(final NotificationRetryEvent notificationRetryEvent) {
      final String currentEntity = notificationRetryEvent.getNotificationDeliveryContext().getEntity();
      notificationRetryEvent.incrementRetryCount();
      notificationRetryEvent.getNotificationDeliveryContext().setEntity(currentEntity + "_RetryProcessor");

      final EventMessage eventMessage = converter.unmarshal(notificationRetryEvent.getMessage(), EventMessage.class);
      eventMessage.setRetryAttempt(notificationRetryEvent.getRetryCount());
      notificationRetryEvent.setMessage(converter.marshal(eventMessage));
    }
  }

}
