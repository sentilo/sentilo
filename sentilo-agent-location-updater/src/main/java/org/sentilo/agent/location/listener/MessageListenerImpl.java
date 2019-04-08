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
package org.sentilo.agent.location.listener;

import org.sentilo.agent.common.listener.AbstractMessageListenerImpl;
import org.sentilo.agent.location.batch.AsyncCatalogResourceUpdater;
import org.sentilo.common.cache.LRUCache;
import org.sentilo.common.cache.impl.LRUCacheImpl;
import org.sentilo.common.domain.EventMessage;
import org.sentilo.common.domain.SensorLocationElement;
import org.sentilo.common.utils.SentiloConstants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

@Component
public class MessageListenerImpl extends AbstractMessageListenerImpl {

  private static final Logger LOGGER = LoggerFactory.getLogger(MessageListenerImpl.class);

  /** Cache to save, temporally, every pair <timestamp, location> for every sensor. */
  private LRUCache<String, String> sensorTimestampLocationsCache;

  private static final String KEY_TOKEN = SentiloConstants.SENTILO_INTERNAL_TOKEN;

  @Autowired
  private AsyncCatalogResourceUpdater asyncResourceUpdater;

  public MessageListenerImpl() {
    this("location listener");
  }

  public MessageListenerImpl(final String name) {
    super(name);
    sensorTimestampLocationsCache = new LRUCacheImpl<String, String>(10000);
  }

  @Override
  public void doWithMessage(final EventMessage eventMessage) {
    try {
      if (eventCouldBeProcessed(eventMessage)) {
        processLocation(eventMessage);
      }
    } catch (final Exception e) {
      LOGGER.error("Error processing message {} ", eventMessage, e);
    }
  }

  private boolean eventCouldBeProcessed(final EventMessage eventMessage) {
    // Returns true only if and only if provider, sensor, timestamp and location are filled in
    return StringUtils.hasText(eventMessage.getProvider()) && StringUtils.hasText(eventMessage.getSensor())
        && StringUtils.hasText(eventMessage.getLocation()) && StringUtils.hasText(eventMessage.getTimestamp());
  }

  private void processLocation(final EventMessage eventMessage) {
    final String key = buildCacheKey(eventMessage);
    final boolean eventAlreadyProcessed = sensorTimestampLocationsCache.get(key) != null ? true : false;

    // At any given instant, a sensor can only be in one location. This implies that an event must
    // be rejected if a previous event from the same sensor and the same instant has already been
    // processed.

    if (!eventAlreadyProcessed) {
      // Do call to catalog API to update sensor location
      updateLocation(eventMessage);
      // and update cache with the new event
      sensorTimestampLocationsCache.put(key, key);
    }

  }

  private void updateLocation(final EventMessage eventMessage) {
    // enqueue location to be update in a background process
    // This location object must contain the following info: provider, sensor, location and
    // timestamp
    LOGGER.trace("update location for event {}", eventMessage);
    final SensorLocationElement sensorLocation = new SensorLocationElement();
    sensorLocation.setProvider(eventMessage.getProvider());
    sensorLocation.setSensor(eventMessage.getSensor());
    sensorLocation.setLocation(eventMessage.getLocation());
    sensorLocation.setFromTsTime(eventMessage.getTime());

    asyncResourceUpdater.addResourceToUpdate(sensorLocation);
  }

  private String buildCacheKey(final EventMessage event) {
    return event.getProvider() + KEY_TOKEN + event.getSensor() + KEY_TOKEN + event.getTime();
  }

}
