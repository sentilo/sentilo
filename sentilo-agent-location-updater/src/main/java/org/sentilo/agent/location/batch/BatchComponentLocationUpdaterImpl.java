/*
 * Sentilo
 *  
 * Original version 1.4 Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de
 * Barcelona. Modified by Opentrends adding support for multitenant deployments and SaaS. 
 * Modifications on version 1.5 Copyright (C) 2015 Opentrends Solucions i Sistemes, S.L.
 * 
 *   
 * This program is licensed and may be used, modified and redistributed under the terms  of the 
 * European Public License (EUPL), either version 1.1 or (at your option) any later version as soon 
 * as they are approved by the European Commission.
 *   
 * Alternatively, you may redistribute and/or modify this program under the terms of the GNU Lesser 
 * General Public License as published by the Free Software Foundation; either  version 3 of the 
 * License, or (at your option) any later version. 
 *   
 * Unless required by applicable law or agreed to in writing, software distributed under the License 
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR  CONDITIONS OF ANY KIND, either express 
 * or implied. 
 *   
 * See the licenses for the specific language governing permissions, limitations and more details.
 *   
 * You should have received a copy of the EUPL1.1 and the LGPLv3 licenses along with this program; 
 * if not, you may find them at: 
 *   
 * https://joinup.ec.europa.eu/software/page/eupl/licence-eupl http://www.gnu.org/licenses/   and 
 * https://www.gnu.org/licenses/lgpl.txt
 */
package org.sentilo.agent.location.batch;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import org.sentilo.agent.common.repository.PendingEventsRepository;
import org.sentilo.common.domain.EventMessage;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

/**
 * This implementation groups messages and stores them periodically in Redis to be processed later
 * in a collaborative manner.
 */
@Component
public class BatchComponentLocationUpdaterImpl implements AsyncComponentLocationUpdater {

  private static final Logger LOGGER = LoggerFactory.getLogger(BatchComponentLocationUpdaterImpl.class);

  @Autowired
  private PendingEventsRepository pendingEventRepository;

  private final Lock lock = new ReentrantLock();
  private final List<EventMessage> sensorMsgsLocations = new ArrayList<>();

  @Override
  public void add(final EventMessage eventMessage) {
    lock.lock();
    try {
      sensorMsgsLocations.add(eventMessage);
    } finally {
      lock.unlock();
    }
  }

  /**
   * At a fixed time ratio, sensor's locations are flushed and stored in the agent's PEL.
   */
  @Scheduled(initialDelay = 30000, fixedDelayString = "${sentilo.agent.location.flush.delay:10000}")
  public void save() {
    final List<EventMessage> eventsToPersist = cloneLocationList();
    if (!CollectionUtils.isEmpty(eventsToPersist)) {
      final List<EventMessage> eventsNoPersisted = pendingEventRepository.storePendingEvents(eventsToPersist);
      // Elements no persisted are added to the sensorMsgsLocations to be retried at the next step
      eventsNoPersisted.forEach(event -> add(event));
    }
  }

  private List<EventMessage> cloneLocationList() {
    lock.lock();
    try {
      final List<EventMessage> copy = new ArrayList<EventMessage>();
      copy.addAll(sensorMsgsLocations);
      sensorMsgsLocations.clear();
      return copy;
    } finally {
      lock.unlock();
    }
  }

  @Override
  public void flush() {
    LOGGER.debug("Call to flush pending tasks");
    try {
      save();
    } finally {
      LOGGER.info("Flush process finished");
    }

  }
}
