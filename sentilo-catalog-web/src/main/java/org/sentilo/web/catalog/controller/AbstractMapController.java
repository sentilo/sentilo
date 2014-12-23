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
package org.sentilo.web.catalog.controller;

import java.security.Principal;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.sentilo.common.domain.QueryFilterParams;
import org.sentilo.common.utils.DateUtils;
import org.sentilo.common.utils.SentiloUtils;
import org.sentilo.platform.client.core.domain.Observation;
import org.sentilo.web.catalog.domain.Component;
import org.sentilo.web.catalog.domain.ComponentType;
import org.sentilo.web.catalog.domain.Sensor;
import org.sentilo.web.catalog.domain.SensorType;
import org.sentilo.web.catalog.dto.InfoBoxDTO;
import org.sentilo.web.catalog.dto.ObservationDTO;
import org.sentilo.web.catalog.format.datetime.LocalDateFormatter;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.service.ComponentService;
import org.sentilo.web.catalog.service.ComponentTypesService;
import org.sentilo.web.catalog.service.SensorService;
import org.sentilo.web.catalog.service.SensorTypesService;
import org.sentilo.web.catalog.utils.LastUpdateMessageBuilder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.MessageSource;
import org.springframework.ui.Model;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

public class AbstractMapController {

  @Autowired
  private ComponentService componentService;

  @Autowired
  private MessageSource messageSource;

  @Autowired
  private SensorService sensorService;

  @Autowired
  private LocalDateFormatter localDateFormat;

  @Autowired
  private ComponentTypesService componentTypesService;

  @Autowired
  private SensorTypesService sensorTypesService;

  @RequestMapping(value = "/{id}/lastOb", method = RequestMethod.GET)
  @ResponseBody
  public InfoBoxDTO getLastObservations(@PathVariable final String id, @RequestParam(required = false) final String to,
      @RequestParam(required = false) final String from, final Principal principal, final Model model) {

    final Component component = getComponentService().find(new Component(id));

    // First, we must find sensors associated with this component
    final SearchFilter filter = new SearchFilter();
    filter.addAndParam("componentId", id);
    // If user is not logged in, then he only can view public components
    if (principal == null) {
      filter.addAndParam("publicAccess", Boolean.TRUE);
    }
    final List<Sensor> sensors = getSensorService().search(filter).getContent();

    // Second, for each sensor retrieve its last observation
    final List<ObservationDTO> lastObservationsList = new ArrayList<ObservationDTO>();

    // If search must be filtered by timestamp therefore from param must be fill in (to param is
    // optional).
    final QueryFilterParams filterParams =
        (StringUtils.hasText(from) ? new QueryFilterParams(DateUtils.stringToDate(from), DateUtils.stringToDate(to), 1) : new QueryFilterParams(1));

    final List<Long> updatedTimestamps = new ArrayList<Long>();
    updatedTimestamps.add(0l);

    for (final Sensor sensor : sensors) {
      final Observation observation = getSensorService().getLastObservation(sensor, filterParams);
      translateAndEscapeSensorType(sensor);
      lastObservationsList.add(new ObservationDTO(sensor, observation));

      // Be careful: every string timestamp is formatted based on the local time zone (see
      // @TranslateTimestamp).
      // Therefore, to convert it to milliseconds a localDateFormatter must be used
      if (observation != null) {
        updatedTimestamps.add(getLocalDateFormat().parseLocalTime(observation.getTimestamp()));
      }

      // sort timestamps in descending order
      Collections.sort(updatedTimestamps, Collections.reverseOrder());

    }

    return new InfoBoxDTO(component, lastObservationsList, LastUpdateMessageBuilder.buildMessage(messageSource, updatedTimestamps.get(0)));
  }

  protected Map<String, String> getIconMap() {
    final List<ComponentType> componentTypes = getComponentTypesService().findAll();
    final Map<String, String> images = new HashMap<String, String>();
    for (final ComponentType componentType : componentTypes) {
      images.put(componentType.getId(), componentType.getIcon());
    }
    return images;
  }

  /**
   * Replace the sensorType id value of a sensor for the sensorType name, because it is a more
   * friendly name.
   * 
   * @param sensor
   */
  protected void translateAndEscapeSensorType(final Sensor sensor) {
    final SensorType type = sensorTypesService.find(new SensorType(sensor.getType()));
    if (type != null) {
      sensor.setType(type.getName());
    }
  }

  protected SearchFilter buildSearchFilter(final String componentType, final String[] bounds) {
    return buildSearchFilter(componentType, bounds, null, false);
  }

  protected SearchFilter buildSearchFilter(final String componentType, final String[] bounds, final Principal principal,
      final boolean filterByAccessType) {
    final SearchFilter filter = new SearchFilter();

    // If user is not logged in and filterByAccessType param is activated, then only public
    // components must be returned
    if (filterByAccessType && principal == null) {
      filter.addAndParam("publicAccess", Boolean.TRUE);
    }

    if (StringUtils.hasText(componentType)) {
      filter.addAndParam("componentType", componentType);
    }

    if (!SentiloUtils.arrayIsEmpty(bounds)) {
      filter.setMapBounds(bounds);
    }

    return filter;
  }

  public ComponentService getComponentService() {
    return componentService;
  }

  public MessageSource getMessageSource() {
    return messageSource;
  }

  public SensorService getSensorService() {
    return sensorService;
  }

  public LocalDateFormatter getLocalDateFormat() {
    return localDateFormat;
  }

  public ComponentTypesService getComponentTypesService() {
    return componentTypesService;
  }

  public SensorTypesService getSensorTypesService() {
    return sensorTypesService;
  }
}
