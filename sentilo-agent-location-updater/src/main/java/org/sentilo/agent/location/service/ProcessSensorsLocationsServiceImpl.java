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
package org.sentilo.agent.location.service;

import java.util.ArrayList;
import java.util.List;

import org.sentilo.agent.common.metrics.AgentMetricsCounter;
import org.sentilo.agent.common.repository.PendingEventsRepository;
import org.sentilo.agent.common.service.impl.AsyncQueuePendingEventServiceImpl;
import org.sentilo.agent.location.parser.CatalogMessageConverter;
import org.sentilo.common.domain.CatalogInputMessage;
import org.sentilo.common.domain.EventMessage;
import org.sentilo.common.domain.SensorMessageLocation;
import org.sentilo.common.exception.RESTClientException;
import org.sentilo.common.rest.RESTClient;
import org.sentilo.common.rest.RequestContext;
import org.sentilo.common.utils.SentiloConstants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

/**
 * Agent location-updater uses the agent's PEL to centralize input locations and process them later
 * in a collaborative manner: PEL guarantees that each event is only read by one member of the agent
 * cluster.
 */
@Component
public class ProcessSensorsLocationsServiceImpl extends AsyncQueuePendingEventServiceImpl {

  private static final Logger LOGGER = LoggerFactory.getLogger(ProcessSensorsLocationsServiceImpl.class);

  @Autowired
  private RESTClient restClient;

  @Autowired
  private PendingEventsRepository pendingEventRepository;

  @Autowired
  private AgentMetricsCounter metricsCounters;

  private final CatalogMessageConverter catalogMessageConverter = new CatalogMessageConverter();

  @Override
  protected void processPendingEvents(final List<String> jsonEvents) {
    if (!CollectionUtils.isEmpty(jsonEvents)) {
      final List<EventMessage> eventsMessageList = new ArrayList<EventMessage>();
      jsonEvents.stream().forEach(event -> eventsMessageList.add(unmarshalEvent(event)));
      sendBatchUpdate(eventsMessageList);
    }
  }

  private void sendBatchUpdate(final List<EventMessage> source) {
    final String path = SentiloConstants.API_TOKEN + SentiloConstants.SLASH + SentiloConstants.LOCATION_TOKEN;
    final CatalogInputMessage message = new CatalogInputMessage();
    final List<SensorMessageLocation> sensorMsgLocations = new ArrayList<>();
    source.forEach(event -> sensorMsgLocations.add(new SensorMessageLocation(event)));
    message.setLocations(sensorMsgLocations);

    final String body = catalogMessageConverter.marshall(message);
    try {
      restClient.put(new RequestContext(path, body));
      metricsCounters.incrementOutputEvents(sensorMsgLocations.size());
      metricsCounters.isRemoteServerConnectionOk(true);
    } catch (final RESTClientException ex) {
      // If update fails, save all events to update in the pending events repository to be
      // forwarded later.
      LOGGER.warn("Components location update process has failed. It will be retried later", ex);
      metricsCounters.isRemoteServerConnectionOk(false);
      pendingEventRepository.storePendingEvents(source);
    }
  }
}
