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

import java.util.List;

import org.sentilo.common.enums.SensorState;
import org.sentilo.web.catalog.domain.Component;
import org.sentilo.web.catalog.domain.ComponentType;
import org.sentilo.web.catalog.domain.Sensor;
import org.sentilo.web.catalog.domain.SensorType;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.security.CatalogUserDetails;
import org.sentilo.web.catalog.service.ComponentTypesService;
import org.sentilo.web.catalog.service.SensorTypesService;
import org.sentilo.web.catalog.utils.Constants;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.RequestMapping;

@Controller
@RequestMapping("/component")
public class PublicComponentController extends BaseComponentController {

  @Autowired
  private SensorTypesService sensorTypesService;

  @Autowired
  private ComponentTypesService componentTypesService;

  @Override
  protected void doBeforeViewResource(final String componentId, final Model model) {
    super.doBeforeViewResource(componentId, model);
    addComponentSensorsRelatedTo(model, componentId);
  }

  @Override
  protected void initViewNames() {
    getViewNames().put(DETAIL_ACTION, Constants.VIEW_PUBLIC_COMPONENT_DETAIL);
  }

  @Override
  protected void doAfterViewResource(final Model model) {
    getPhotoToShow(model);
    addComponentTypeName(model);
    super.doAfterViewResource(model);
  }

  private void addComponentTypeName(final Model model) {
    final Component component = (Component) model.asMap().get(getEntityModelKey());
    if (StringUtils.hasText(component.getComponentType())) {
      final ComponentType type = getComponentTypesService().find(new ComponentType(component.getComponentType()));
      // Replace component type id for component type name because name should be displayed in the
      // public detail page
      component.setComponentType(type.getName());
    }
  }

  private void getPhotoToShow(final Model model) {
    final Component component = (Component) model.asMap().get(getEntityModelKey());
    if (!StringUtils.hasText(component.getPhotoUrl())) {
      final ComponentType compType = componentTypesService.find(new ComponentType(component.getComponentType()));
      if (StringUtils.hasText(compType.getPhotoUrl())) {
        component.setPhotoUrl(compType.getPhotoUrl());
      }
    }
  }

  /**
   * Add to view's model all sensors related to component with id componentId
   *
   * @param model
   * @param componentId Component identifier.
   */
  private void addComponentSensorsRelatedTo(final Model model, final String componentId) {

    final SearchFilter filter = new SearchFilter();
    filter.addAndParam("componentId", componentId);

    // If user is not logged in, then he only can view public components
    final Authentication authetication = SecurityContextHolder.getContext().getAuthentication();
    final boolean userIsLoggedIn = authetication != null && authetication.getPrincipal() instanceof CatalogUserDetails;
    if (!userIsLoggedIn) {
      filter.addAndParam("publicAccess", Boolean.TRUE);
    }

    // Only active sensors are displayed on the map
    filter.addAndParam("state", SensorState.online);
    final List<Sensor> sensors = getSensorService().search(filter).getContent();
    for (final Sensor sensor : sensors) {
      translateAndEscapeSensorType(sensor);
    }

    model.addAttribute(Constants.MODEL_COMPONENT_SENSORS, sensors);
  }

  /**
   * Replace the sensorType id value of a sensor for the sensorType name, because it is a more
   * friendly name.
   *
   * @param sensor
   */
  private void translateAndEscapeSensorType(final Sensor sensor) {
    final SensorType type = sensorTypesService.find(new SensorType(sensor.getType()));
    if (type != null) {
      sensor.setType(type.getName());
    }
  }

}
