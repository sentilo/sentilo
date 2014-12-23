/*
 * Sentilo
 * 
 * Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de Barcelona.
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
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import org.sentilo.agent.location.parser.CatalogMessageConverter;
import org.sentilo.common.batch.BatchProcess;
import org.sentilo.common.domain.CatalogInputMessage;
import org.sentilo.common.domain.SensorLocationElement;
import org.sentilo.common.exception.RESTClientException;
import org.sentilo.common.rest.RESTClient;
import org.sentilo.common.utils.SentiloConstants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

@Component
public class ComponentLocationUpdater implements AsyncCatalogResourceUpdater, BatchProcess {

  private final Logger logger = LoggerFactory.getLogger(ComponentLocationUpdater.class);
  private final Lock lock = new ReentrantLock();

  @Autowired
  private RESTClient restClient;

  /**
   * Ordered set with the current updates awaiting to be sent to the Catalog: ordered is fixed by
   * the timestamp
   */
  private HashSet<SensorLocationElement> currentUpdatesAwaiting;

  /**
   * Ordered set with updates awaiting to be sent to the Catalog one more time: ordered is fixed by
   * the timestamp
   */
  private HashSet<SensorLocationElement> oldUpdatesAwaiting;

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

  @Scheduled(initialDelay = 120000, fixedDelay = 60000)
  public void process() {
    final List<SensorLocationElement> sortedListToUpdate = cloneSortAndClearLocations();

    logger.info("Start process to update {} locations.", sortedListToUpdate.size());

    if (sortedListToUpdate.size() != 0) {
      sendBatchUpdate(sortedListToUpdate);
    }

    logger.info("Process finished. Locations that has not been updated: {}", oldUpdatesAwaiting.size());
  }

  private List<SensorLocationElement> cloneSortAndClearLocations() {
    lock.lock();
    try {
      // Merge old and current locations in a list, sort them by timestamp,
      // and finally remove duplicate consecutive entries, i.e,
      // consecutive entries from the same sensor that does not modify the location
      final HashSet<SensorLocationElement> copy = new HashSet<SensorLocationElement>();
      copy.addAll(oldUpdatesAwaiting);
      copy.addAll(currentUpdatesAwaiting);
      currentUpdatesAwaiting.clear();

      final List<SensorLocationElement> copyList = new ArrayList<SensorLocationElement>(copy);
      Collections.sort(copyList);

      return removeDuplicateConsecutiveEntries(copyList);
    } finally {
      lock.unlock();
    }
  }

  private List<SensorLocationElement> removeDuplicateConsecutiveEntries(final List<SensorLocationElement> sourceOrderedList) {
    final List<SensorLocationElement> returnList = new ArrayList<SensorLocationElement>();

    // For every sensor, stores the current location
    final Map<String, String> sensorLastLocations = new HashMap<String, String>();

    for (final SensorLocationElement sensorLocation : sourceOrderedList) {
      final String key = sensorLocation.getProvider().concat(".").concat(sensorLocation.getSensor());
      final String previousSensorLocation = sensorLastLocations.get(key);
      if (previousSensorLocation == null || !sensorLocation.getLocation().equals(previousSensorLocation)) {
        returnList.add(sensorLocation);
        sensorLastLocations.put(key, sensorLocation.getLocation());
      }
    }

    return returnList;
  }

  private void sendBatchUpdate(final List<SensorLocationElement> listToUpdate) {
    final String path = SentiloConstants.API_TOKEN + SentiloConstants.SLASH + SentiloConstants.LOCATION_TOKEN;
    final CatalogInputMessage message = new CatalogInputMessage();
    message.setLocations(listToUpdate);

    final String body = parser.marshall(message);
    logger.debug("Message body of update call to catalog: {}", body);
    try {
      // Locations sent to the catalog are ordered by timestamp
      restClient.put(path, body);
      oldUpdatesAwaiting.clear();
    } catch (final RESTClientException ex) {
      // If update fails, save resources to update in the oldUpdatesAwaiting list to be sent at the
      // next update call
      oldUpdatesAwaiting.clear();
      oldUpdatesAwaiting.addAll(listToUpdate);
    }
  }

}
