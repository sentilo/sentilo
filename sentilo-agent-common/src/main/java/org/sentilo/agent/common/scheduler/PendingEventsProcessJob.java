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
package org.sentilo.agent.common.scheduler;

import java.util.concurrent.TimeUnit;

import org.sentilo.agent.common.service.AsyncPendingEventService;
import org.sentilo.common.converter.DefaultStringMessageConverter;
import org.sentilo.common.converter.StringMessageConverter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.annotation.Scheduled;

/**
 * Scheduled job that runs every X time (default 30 seconds) and read and process agent pending
 * events (both Stream PEL and queue PEL)
 *
 * In every execution reads all pending events in batches of size ITEMS_TO_READ to no penalize the
 * performance of the underlying agent (the processing of pending events must be done in a second
 * plane).
 */
public abstract class PendingEventsProcessJob {

  private static final Logger LOGGER = LoggerFactory.getLogger(PendingEventsProcessJob.class);

  @Value("${sentilo.agent.pending_events_job.batch:50}")
  private int ITEMS_TO_READ;

  protected final StringMessageConverter stringMsgConverter = new DefaultStringMessageConverter();

  protected abstract AsyncPendingEventService getService();

  protected abstract String getRepositoryType();

  /**
   * Read and process pending events from either stream o queue PEL (depending on the base
   * implementation).
   */
  @Scheduled(initialDelay = 60000, fixedDelayString = "${sentilo.agent.pending_events_job.delay:30000}")
  public void run() {
    // pending events are read and processed in groups of size ITEMS_TO_READ.
    int numReadings = 0;
    boolean emptyPendingEvents = false;

    while (!emptyPendingEvents) {
      final int pendingEventsProcessed = getService().readAndProcessPendingEvents(ITEMS_TO_READ);

      if (pendingEventsProcessed == 0) {
        emptyPendingEvents = true;
      } else {
        numReadings += pendingEventsProcessed;

        // before to continue with loop, add a delay of 100ms to liberate load in main workflow
        try {
          TimeUnit.MILLISECONDS.sleep(100);
        } catch (final InterruptedException e) {
          // Nothing to do
        }
      }
    }

    // source repository could be either streams or queue PEL, depending on job implementation
    LOGGER.info(" [{}]  - Pending events processed in this execution : [{}]", getRepositoryType(), numReadings);
  }

}
