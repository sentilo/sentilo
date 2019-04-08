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
import org.sentilo.web.catalog.domain.Provider;
import org.sentilo.web.catalog.domain.Sensor;
import org.sentilo.web.catalog.domain.SensorSubstate;
import org.sentilo.web.catalog.domain.SensorType;
import org.sentilo.web.catalog.dto.ComponentDTO;
import org.sentilo.web.catalog.dto.InfoBoxDTO;
import org.sentilo.web.catalog.dto.ObservationDTO;
import org.sentilo.web.catalog.dto.SensorDTO;
import org.sentilo.web.catalog.format.datetime.LocalDateFormatter;
import org.sentilo.web.catalog.format.misc.SensorValueFormatter;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.search.builder.DefaultSearchFilterBuilderImpl;
import org.sentilo.web.catalog.search.builder.SearchFilterBuilder;
import org.sentilo.web.catalog.service.ComponentService;
import org.sentilo.web.catalog.service.ComponentTypesService;
import org.sentilo.web.catalog.service.ProviderService;
import org.sentilo.web.catalog.service.SensorService;
import org.sentilo.web.catalog.service.SensorSubstateService;
import org.sentilo.web.catalog.service.SensorTypesService;
import org.sentilo.web.catalog.utils.CatalogUtils;
import org.sentilo.web.catalog.utils.LastUpdateMessageBuilder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.MessageSource;
import org.springframework.http.MediaType;
import org.springframework.ui.Model;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

public class AbstractMapController extends CatalogBaseController {

  @Autowired
  private ProviderService providerService;

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

  @Autowired
  private SensorSubstateService sensorSubstateService;

  @Autowired
  private SensorValueFormatter sensorValueFormatter;

  private final SearchFilterBuilder searchFilterBuilder = new DefaultSearchFilterBuilderImpl();

  @RequestMapping(value = "/{id}", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  public ComponentDTO getComponentDetails(@PathVariable final String id, final Principal principal, final Model model) {
    final Component component = componentService.findById(id);
    final Provider provider = providerService.findById(component.getProviderId());
    final ComponentType type = componentTypesService.findById(component.getComponentType());

    return new ComponentDTO(provider, component, type);
  }

  @RequestMapping(value = "/{id}/lastOb", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  public InfoBoxDTO getLastObservations(@PathVariable final String id, @RequestParam(required = false) final String to,
      @RequestParam(required = false) final String from, final Principal principal, final Model model) {

    final Component component = componentService.findById(id);

    // First, we must find all sensors associated with this component
    final List<Sensor> sensors = getComponentSensors(id, principal);
    final List<SensorDTO> sensorsDtoList = new ArrayList<SensorDTO>();
    for (final Sensor sensor : sensors) {
      sensorsDtoList.add(toSensorDTO(sensor));
    }

    // Next, for each sensor retrieve its last observation
    final List<ObservationDTO> lastObservationsList = new ArrayList<ObservationDTO>();

    // If search must be filtered by timestamp therefore "from" param must be fill in ("to" param is
    // optional).
    final QueryFilterParams filterParams =
        StringUtils.hasText(from) ? new QueryFilterParams(DateUtils.stringToDate(from), DateUtils.stringToDate(to), 1) : new QueryFilterParams(1);

    final List<Long> updatedTimestamps = new ArrayList<Long>();
    updatedTimestamps.add(0L);

    for (final Sensor sensor : sensors) {
      final ObservationDTO observationDTO = getSensorLastObservation(filterParams, sensor);

      // Finally, add the observation to list
      lastObservationsList.add(observationDTO);

      if (observationDTO.getTime() != 0) {
        updatedTimestamps.add(observationDTO.getTime());
      }

      // and sort timestamps in descending order
      Collections.sort(updatedTimestamps, Collections.reverseOrder());
    }

    return new InfoBoxDTO(component, sensorsDtoList, lastObservationsList,
        LastUpdateMessageBuilder.buildMessage(messageSource, updatedTimestamps.get(0)));
  }

  private ObservationDTO getSensorLastObservation(final QueryFilterParams filterParams, final Sensor sensor) {

    if (sensor.getSubstate() != null) {
      final SensorSubstate ss = sensorSubstateService.find(sensor.getSubstate());
      sensor.setSubstateDesc(ss.getDescription());
    }

    final Observation observation = getSensorService().getLastObservation(sensor, filterParams);
    translateAndEscapeSensorType(sensor);

    final ObservationDTO observationDTO = new ObservationDTO(sensor, observation);

    if (StringUtils.hasText(observationDTO.getValue())) {
      observationDTO.setFormattedValue(sensorValueFormatter.formatValue(sensor, observation));
    }

    return observationDTO;
  }

  private List<Sensor> getComponentSensors(final String id, final Principal principal) {
    final SearchFilter filter = new SearchFilter();
    filter.addAndParam("componentId", id);

    // If user is not logged in, then he only can view public components
    if (principal == null) {
      filter.addAndParam("publicAccess", Boolean.TRUE);
    }

    return getSensorService().search(filter).getContent();
  }

  protected Map<String, String> getIconMap() {
    final List<ComponentType> componentTypes = CatalogUtils.sortAlphabetically(getComponentTypesService().findAll());
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
    final SensorType type = sensorTypesService.findById(sensor.getType());
    if (type != null) {
      sensor.setType(type.getName());
    }
  }

  protected SearchFilter buildSearchFilter(final String[] componentTypes, final String[] bounds) {

    // If user is not logged in then only public components must be returned
    // It will be calculated in this build filter method
    final SearchFilter filter = getSearchFilterBuilder().buildMapSearchFilter();

    if (!SentiloUtils.arrayIsEmpty(componentTypes)) {
      filter.addAndParam("componentType", componentTypes);
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

  public SearchFilterBuilder getSearchFilterBuilder() {
    return searchFilterBuilder;
  }

  private SensorDTO toSensorDTO(final Sensor sensor) {
    return new SensorDTO(sensor.getSensorId(), sensor.getType(), sensor.getState().name(), sensor.getSubstate(), sensor.getSubstateDesc(),
        sensor.getUnit(), sensor.getDataType().name(), sensor.getProviderId());
  }
}
