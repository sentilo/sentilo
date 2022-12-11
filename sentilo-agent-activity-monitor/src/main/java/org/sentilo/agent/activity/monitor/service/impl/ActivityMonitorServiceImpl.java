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
package org.sentilo.agent.activity.monitor.service.impl;

import java.util.Arrays;
import java.util.List;

import javax.annotation.PostConstruct;

import org.sentilo.agent.activity.monitor.domain.CatalogAdditionalFields;
import org.sentilo.agent.activity.monitor.repository.ActivityMonitorRepository;
import org.sentilo.agent.activity.monitor.service.ActivityMonitorService;
import org.sentilo.agent.activity.monitor.service.CatalogService;
import org.sentilo.common.domain.EventMessage;
import org.sentilo.common.enums.EventType;
import org.sentilo.common.utils.SentiloConstants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

@Component
public class ActivityMonitorServiceImpl implements ActivityMonitorService {

  private static final Logger LOGGER = LoggerFactory.getLogger(ActivityMonitorServiceImpl.class);

  @Autowired
  private CatalogService catalogService;
  @Autowired
  private ActivityMonitorRepository repository;

  private boolean filterByTenant = false;
  private String tenantFilter;
  private List<String> tenantsFilterList;

  @PostConstruct
  public void init() {
    tenantFilter = System.getProperty("sentilo.tenant.filter");
    filterByTenant = StringUtils.hasText(tenantFilter);
    if (filterByTenant) {
      tenantsFilterList = Arrays.asList(tenantFilter.split(","));
    }
    LOGGER.info("Filter by tenant enabled? {} --> {}", filterByTenant, tenantFilter);
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.agent.activity.monitor.service.ActivityMonitorService#process(org.sentilo.common
   * .domain.EventMessage)
   */
  public void process(final EventMessage eventMessage) {
    final EventType type = EventType.valueOf(eventMessage.getType().toUpperCase());

    // If filter by tenant is enabled, checks if tenantsFilterList contains eventMessage's tenant.
    // Otherwise rejects event and no process it
    if (filterByTenant && !tenantsFilterList.contains(eventMessage.getTenant())) {
      return;
    }

    switch (type) {
      case DATA:
        processDataEvent(eventMessage);
        break;
      case ORDER:
        processOrderEvent(eventMessage);
        break;
      case ALARM:
        processAlarmEvent(eventMessage);
        break;
      default:
        break;
    }
  }

  private void processDataEvent(final EventMessage eventMessage) {
    populateAdditionalFields(eventMessage, EventType.DATA);
    publishMessageToElasticSearch(eventMessage);
  }

  private void processOrderEvent(final EventMessage eventMessage) {
    populateAdditionalFields(eventMessage, EventType.ORDER);
    publishMessageToElasticSearch(eventMessage);
  }

  private void processAlarmEvent(final EventMessage eventMessage) {
    populateAdditionalFields(eventMessage, EventType.ALARM);
    publishMessageToElasticSearch(eventMessage);
  }

  private void populateAdditionalFields(final EventMessage eventMessage, final EventType eventType) {
    // Depending on the event message type, some additional fields need to be populated before send
    // it to index
    if (eventType.equals(EventType.ALARM) && isInternalAlarm(eventMessage, eventType) && !isGhostSensorAlarm(eventMessage, eventType)) {
      eventMessage.setSensorType(catalogService.getSensorType(eventMessage.getProvider(), eventMessage.getSensor()));
      final CatalogAdditionalFields additionalFields = catalogService.getAdditionalFields(eventMessage.getAlert());
      if (additionalFields != null) {
        eventMessage.setLocation(additionalFields.getLocation());
        eventMessage.setComponent(additionalFields.getComponentId());
      }
    }

  }

  private boolean isGhostSensorAlarm(final EventMessage eventMessage, final EventType eventType) {
    return eventType.equals(EventType.ALARM) && SentiloConstants.GHOST_SENSOR_ALERT.equals(eventMessage.getAlert());
  }

  private boolean isInternalAlarm(final EventMessage eventMessage, final EventType eventType) {
    return eventType.equals(EventType.ALARM) && StringUtils.hasText(eventMessage.getProvider()) && StringUtils.hasText(eventMessage.getSensor());
  }

  private void publishMessageToElasticSearch(final EventMessage eventMessage) {
    // Elasticsearch needs location formatted as "latitude,longitude"
    if (StringUtils.hasText(eventMessage.getLocation())) {
      eventMessage.setLocation(eventMessage.getLocation().replace(' ', ','));
    }

    repository.publishMessageToElasticSearch(eventMessage);
  }

}
