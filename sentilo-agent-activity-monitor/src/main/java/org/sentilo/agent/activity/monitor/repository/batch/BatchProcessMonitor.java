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
package org.sentilo.agent.activity.monitor.repository.batch;

import org.sentilo.agent.common.metrics.AgentMetricsCounter;
import org.sentilo.agent.common.repository.PendingEventsRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

@Component
public class BatchProcessMonitor implements BatchProcessCallback {

  private static final Logger LOGGER = LoggerFactory.getLogger(BatchProcessMonitor.class);

  private long numTasksProcessed;
  private long numTasksOk;
  private long numTasksKo;
  private long numLostElements;

  @Autowired
  private PendingEventsRepository pendingEventRepository;

  @Autowired
  private AgentMetricsCounter metricsCounters;

  @Override
  public void notifyBatchProcessIsDone(final BatchProcessResult result) {
    numTasksProcessed++;
    metricsCounters.incrementOutputEvents(result.getNumElementsProcessed());

    if (!CollectionUtils.isEmpty(result.getPendingEvents())) {
      numTasksKo++;
      numLostElements += result.getNumElementsToProcess() - result.getNumElementsProcessed();
      pendingEventRepository.storePendingEvents(result.getPendingEvents());
      metricsCounters.isRemoteServerConnectionOk(false);
    } else {
      numTasksOk++;
      metricsCounters.isRemoteServerConnectionOk(true);
    }
  }

  @Scheduled(initialDelay = 60000, fixedDelay = 60000 * 10)
  public void writeState() {
    LOGGER.info(" ---- BatchProcessMonitor  ---- ");
    final StringBuilder sb = new StringBuilder("\n Num. tasks processed : {}");
    sb.append("\n Num tasks ok: {}");
    sb.append("\n Num tasks ko: {}");
    sb.append("\n Num lost elements: {}");
    LOGGER.info(sb.toString(), numTasksProcessed, numTasksOk, numTasksKo, numLostElements);
  }
}
