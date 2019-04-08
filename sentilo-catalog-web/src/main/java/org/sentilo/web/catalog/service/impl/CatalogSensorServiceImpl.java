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
package org.sentilo.web.catalog.service.impl;

import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.sentilo.common.domain.CatalogSensor;
import org.sentilo.common.enums.SensorState;
import org.sentilo.web.catalog.converter.ApiConverter;
import org.sentilo.web.catalog.domain.Component;
import org.sentilo.web.catalog.domain.Sensor;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.service.CatalogSensorService;
import org.sentilo.web.catalog.service.ComponentService;
import org.sentilo.web.catalog.service.SensorService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

@Service
public class CatalogSensorServiceImpl implements CatalogSensorService {

  private static final Logger LOGGER = LoggerFactory.getLogger(CatalogSensorServiceImpl.class);

  @Autowired
  private SensorService sensorService;

  @Autowired
  private ComponentService componentService;

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.web.catalog.service.CatalogSensorService#getSensorsByProvider(java.lang.String,
   * java.util.Map)
   */
  public List<CatalogSensor> getSensorsByProvider(final String providerId, final Map<String, String> filterParams) {
    LOGGER.debug("Get catalogSensor's list for provider {}", providerId);
    List<CatalogSensor> catalogSensors = Collections.<CatalogSensor>emptyList();
    final List<Sensor> sensors = sensorService.search(buildSensorsFilter(providerId, filterParams)).getContent();
    if (!CollectionUtils.isEmpty(sensors)) {
      final List<Component> components = componentService.search(buildComponentsFilter(providerId, filterParams)).getContent();
      catalogSensors = ApiConverter.convertToCatalogSensorList(sensors, components);
    }

    return catalogSensors;
  }

  private SearchFilter buildSensorsFilter(final String providerId, final Map<String, String> filterParams) {
    final SearchFilter filter = new SearchFilter();
    filter.addAndParam("providerId", providerId);

    if (StringUtils.hasText(filterParams.get("type"))) {
      filter.addAndParam("type", filterParams.get("type"));
    }

    // Finally, only active sensors should be returned
    filter.addAndParam("state", SensorState.online);

    return filter;
  }

  private SearchFilter buildComponentsFilter(final String providerId, final Map<String, String> filterParams) {
    final SearchFilter filter = new SearchFilter();
    filter.addAndParam("providerId", providerId);

    if (StringUtils.hasText(filterParams.get("component"))) {
      filter.addAndParam("name", filterParams.get("component"));
    }

    if (StringUtils.hasText(filterParams.get("componentType"))) {
      filter.addAndParam("componentType", filterParams.get("componentType"));
    }

    return filter;
  }

}
