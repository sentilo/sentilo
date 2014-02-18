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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.sentilo.platform.client.core.domain.Observation;
import org.sentilo.web.catalog.domain.Component;
import org.sentilo.web.catalog.domain.ComponentType;
import org.sentilo.web.catalog.domain.Sensor;
import org.sentilo.web.catalog.domain.SensorType;
import org.sentilo.web.catalog.dto.MapComponentDTO;
import org.sentilo.web.catalog.dto.ObservationDTO;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.service.SensorTypesService;
import org.sentilo.web.catalog.utils.Constants;
import org.sentilo.web.catalog.utils.ModelUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

@Controller
@RequestMapping("/component")
public class PublicComponentController extends BaseComponentController {

  @Autowired
  private SensorTypesService sensorTypesService;

  @RequestMapping(value = "/map", method = RequestMethod.GET)
  public String showMap(final Model model) {
    ModelUtils.addActiveMenuTo(model, Constants.MENU_COMPONENT_MAP);
    ModelUtils.addUpdateDateTo(model);
    // El listado de tipos de sensores se necesita para montar el desplegable del filtro
    // del mapa
    model.addAttribute(Constants.MODEL_COMPONENT_TYPES, componentTypesService.findAll());
    return Constants.VIEW_PUBLIC_COMPONENT_MAP;
  }

  @RequestMapping(value = "/map/json", method = RequestMethod.GET)
  @ResponseBody
  public List<MapComponentDTO> getJSONComponentMap(@RequestParam final String componentType, final Model model) {
    final SearchFilter filter = new SearchFilter();
    filter.addAndParam("mobile", Constants.STATIC);
    filter.addAndParam("publicAccess", Boolean.TRUE);
    if (StringUtils.hasText(componentType)) {
      filter.addAndParam("componentType", componentType);
    }
    final Map<String, String> icons = getIconMap();
    final List<MapComponentDTO> result = new ArrayList<MapComponentDTO>();
    for (final Component component : componentService.search(filter).getContent()) {
      result.add(new MapComponentDTO(component, icons.get(component.getComponentType())));
    }
    return result;
  }

  @RequestMapping(value = "/{id}/lastOb", method = RequestMethod.GET)
  @ResponseBody
  public List<ObservationDTO> getLastObservations(@PathVariable final String id, final Model model) {
    final SearchFilter filter = new SearchFilter();
    filter.addAndParam("componentId", id);
    filter.addAndParam("publicAccess", Boolean.TRUE);
    final List<Sensor> sensors = sensorService.search(filter).getContent();
    final List<ObservationDTO> result = new ArrayList<ObservationDTO>();
    for (final Sensor sensor : sensors) {
      final Observation observation = sensorService.getLastObservation(sensor);
      translateAndEscapeSensorType(sensor);
      result.add(new ObservationDTO(sensor, observation));
    }
    return result;
  }

  @Override
  protected void doBeforeViewResource(final String componentId, final Model model) {
    super.doBeforeViewResource(componentId, model);
    addComponentSensorsTo(model, componentId);
  }

  protected void initViewNames() {
    viewNames.put(LIST_ACTION, Constants.VIEW_PUBLIC_COMPONENT_LIST);
    viewNames.put(DETAIL_ACTION, Constants.VIEW_PUBLIC_COMPONENT_DETAIL);
    // Se ha eliminado la vista de creación/edición a propósito.
  }

  /**
   * Searches for component related sensors.
   * 
   * @param model
   * @param componentId Component identifier.
   */
  private void addComponentSensorsTo(final Model model, final String componentId) {
    final SearchFilter filter = new SearchFilter();
    filter.addParam("componentId", componentId);
    final List<Sensor> sensors = sensorService.search(filter).getContent();

    for (final Sensor sensor : sensors) {
      translateAndEscapeSensorType(sensor);
    }
    model.addAttribute(Constants.MODEL_COMPONENT_SENSORS, sensors);
  }

  private void translateAndEscapeSensorType(final Sensor sensor) {
    // TODO Mikel: Esto debería ser una join o bien tener cachaeada la coleccion SensorType
    final SensorType type = sensorTypesService.find(new SensorType(sensor.getType()));
    if (type != null) {
      sensor.setType(type.getName());
    }
  }

  private Map<String, String> getIconMap() {
    final List<ComponentType> componentTypes = componentTypesService.findAll();
    final Map<String, String> images = new HashMap<String, String>();
    for (final ComponentType componentType : componentTypes) {
      images.put(componentType.getId(), componentType.getIcon());
    }
    return images;
  }
}
