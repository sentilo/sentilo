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
package org.sentilo.agent.kafka.service.impl;

import org.sentilo.agent.common.exception.IncompleteCatalogDataException;
import org.sentilo.agent.kafka.domain.CatalogAdditionalFields;
import org.sentilo.agent.kafka.repository.KafkaAgentRepository;
import org.sentilo.agent.kafka.service.CatalogService;
import org.sentilo.agent.kafka.service.KafkaAgentService;
import org.sentilo.common.domain.EventMessage;
import org.sentilo.common.enums.EventType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

@Component
public class KafkaAgentServiceImpl implements KafkaAgentService {

  private static final Logger LOGGER = LoggerFactory.getLogger(KafkaAgentServiceImpl.class);

  @Autowired
  private CatalogService catalogService;
  @Autowired
  private KafkaAgentRepository repository;

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.agent.kafka.service.KafkaAgentService#process(org.sentilo.common
   * .domain.EventMessage)
   */
  public void process(final EventMessage eventMessage) {
    final EventType type = EventType.valueOf(eventMessage.getType().toUpperCase());

    try {
      populateAdditionalFields(eventMessage, type);
      repository.publishMessageToKafka(eventMessage);
    } catch (final IncompleteCatalogDataException e) {
      LOGGER.info("Sensor {} not found in catalog", eventMessage.getSensor());
    }

  }

  private void populateAdditionalFields(final EventMessage eventMessage, final EventType eventType) {
    eventMessage.setSensorType(catalogService.getSensorType(eventMessage.getProvider(), eventMessage.getSensor()));

    // Depending on the event message type, some additional fields need to be populated
    if (eventType.equals(EventType.ALARM) && isInternalAlarm(eventMessage, eventType)) {
      final CatalogAdditionalFields additionalFields = catalogService.getAlertAdditionalFields(eventMessage.getAlert());
      if (additionalFields != null) {
        eventMessage.setComponent(additionalFields.getComponentId());
      }
    }
  }

  private boolean isInternalAlarm(final EventMessage eventMessage, final EventType eventType) {
    return eventType.equals(EventType.ALARM) && StringUtils.hasText(eventMessage.getProvider()) && StringUtils.hasText(eventMessage.getSensor());
  }

}
