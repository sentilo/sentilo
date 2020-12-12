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
package org.sentilo.agent.location.batch;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import org.sentilo.agent.common.metrics.AgentMetricsCounter;
import org.sentilo.agent.location.parser.CatalogMessageConverter;
import org.sentilo.common.batch.BatchProcess;
import org.sentilo.common.domain.CatalogInputMessage;
import org.sentilo.common.domain.SensorLocationElement;
import org.sentilo.common.exception.RESTClientException;
import org.sentilo.common.rest.RESTClient;
import org.sentilo.common.rest.RequestContext;
import org.sentilo.common.utils.SentiloConstants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

@Component
public class ComponentLocationUpdater implements AsyncCatalogResourceUpdater, BatchProcess {

  private static final Logger LOGGER = LoggerFactory.getLogger(ComponentLocationUpdater.class);
  private final Lock lock = new ReentrantLock();

  @Autowired
  private RESTClient restClient;

  @Autowired
  private AgentMetricsCounter metricsCounters;

  /**
   * Set with the current updates awaiting to be sent to the Catalog
   */
  private Set<SensorLocationElement> currentUpdatesAwaiting;

  /**
   * Set with updates awaiting to be sent to the Catalog one more time
   */
  private Set<SensorLocationElement> oldUpdatesAwaiting;

  private CatalogMessageConverter parser;

  public ComponentLocationUpdater() {
    super();
    currentUpdatesAwaiting = new HashSet<SensorLocationElement>();
    oldUpdatesAwaiting = new HashSet<SensorLocationElement>();
    parser = new CatalogMessageConverter();
  }

  public void addResourceToUpdate(final SensorLocationElement resource) {
    lock.lock();
    try {
      // Only adds resource if it is not already present into the Set, i.e., if not exists resource
      // aux which aux € currentUpdatesAwaiting and aux.equals(resource)
      currentUpdatesAwaiting.add(resource);
    } finally {
      lock.unlock();
    }
  }

  @Scheduled(initialDelay = 30000, fixedDelay = 15000)
  public void process() {
    final List<SensorLocationElement> sortedListToUpdate = cloneAndSortLocations();
    LOGGER.debug("Start process to update {} locations.", sortedListToUpdate.size());
    if (!CollectionUtils.isEmpty(sortedListToUpdate)) {
      sendBatchUpdate(sortedListToUpdate);
    }
    LOGGER.debug("Process finished. Locations that has not been updated: {}", oldUpdatesAwaiting.size());
  }

  private List<SensorLocationElement> cloneAndSortLocations() {
    lock.lock();
    try {
      // Merge old and current locations in a list
      final Set<SensorLocationElement> copy = new HashSet<SensorLocationElement>();
      copy.addAll(oldUpdatesAwaiting);
      copy.addAll(currentUpdatesAwaiting);
      currentUpdatesAwaiting.clear();

      // ... and sort them by timestamp
      final List<SensorLocationElement> copyList = new ArrayList<SensorLocationElement>(copy);
      Collections.sort(copyList);

      return copyList;
    } finally {
      lock.unlock();
    }
  }

  private void sendBatchUpdate(final List<SensorLocationElement> listToUpdate) {
    final String path = SentiloConstants.API_TOKEN + SentiloConstants.SLASH + SentiloConstants.LOCATION_TOKEN;
    final CatalogInputMessage message = new CatalogInputMessage();
    message.setLocations(listToUpdate);

    final String body = parser.marshall(message);
    try {
      // Locations sent to the catalog are ordered by timestamp
      restClient.put(new RequestContext(path, body));
      oldUpdatesAwaiting.clear();
      metricsCounters.incrementOutputEvents(listToUpdate.size());
      metricsCounters.isRemoteServerConnectionOk(true);
    } catch (final RESTClientException ex) {
      // If update fails, save all resources to update in the oldUpdatesAwaiting list to be
      // forwarded at the next update call.
      LOGGER.warn("Components location update process has failed. It will be retried later", ex);
      oldUpdatesAwaiting.clear();
      oldUpdatesAwaiting.addAll(listToUpdate);
      metricsCounters.isRemoteServerConnectionOk(false);
    }
  }

}
