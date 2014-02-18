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
package org.sentilo.web.catalog.controller.admin;

import java.util.Arrays;
import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.validation.Valid;

import org.sentilo.web.catalog.controller.BaseComponentController;
import org.sentilo.web.catalog.domain.Component;
import org.sentilo.web.catalog.domain.ComponentType;
import org.sentilo.web.catalog.domain.Provider;
import org.sentilo.web.catalog.domain.Sensor;
import org.sentilo.web.catalog.dto.ComponentComponentsDTO;
import org.sentilo.web.catalog.dto.ComponentSensorsDTO;
import org.sentilo.web.catalog.dto.ComponentsDTO;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.service.ProviderService;
import org.sentilo.web.catalog.utils.CatalogUtils;
import org.sentilo.web.catalog.utils.Constants;
import org.sentilo.web.catalog.utils.ModelUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

@Controller
@RequestMapping("/admin/component")
public class ComponentController extends BaseComponentController {

  private static final String COMPONENT_ID_ATTRIBUTE = "componentId";
  private static final String PARENT_ID_ATTRIBUTE = "parentId";

  @Autowired
  private ProviderService providerService;

  @ModelAttribute(Constants.MODEL_COMPONENT_TYPES)
  public List<ComponentType> getComponentTypes() {
    return componentTypesService.findAll();
  }

  @RequestMapping("/search/json")
  @ResponseBody
  public List<Component> search(final HttpServletRequest request, @RequestParam(required = true) final String providerId, final Model model) {
    // TODO Este método se utiliza en el mantenimiento de alertas para seleccionar el sensor de la
    // alerta
    final SearchFilter filter = new SearchFilter();
    filter.addAndParam("providerId", providerId);
    return componentService.search(filter).getContent();
  }

  @RequestMapping(value = "/{id}/addComponents", method = RequestMethod.GET)
  public String addComponents(@PathVariable final String id, final Model model) {
    final List<Component> components = componentService.findAll();
    final ComponentComponentsDTO form = new ComponentComponentsDTO(id, components);
    model.addAttribute(Constants.MODEL_COMPONENT_COMPONENTS, form);
    return Constants.VIEW_ADD_COMPONENTS_TO_COMPONENT;
  }

  @RequestMapping(value = "/{id}/addComponents", method = RequestMethod.POST)
  public String assignComponents(@PathVariable final String id, @Valid final ComponentComponentsDTO componentComponents, final BindingResult result, final Model model) {

    if (!CatalogUtils.arrayIsEmpty(componentComponents.getSelectedIds())) {
      componentService.updateMulti(Arrays.asList(componentComponents.getSelectedIds()), PARENT_ID_ATTRIBUTE, id);
    }

    ModelUtils.addConfirmationMessageTo(model, "component.assigned");
    ModelUtils.addOpenedTabTo(model, Constants.TAB_2);
    addResourceToModel(id, model);
    return Constants.VIEW_COMPONENT_DETAIL;
  }

  @RequestMapping(value = "/{id}/addSensors", method = RequestMethod.GET)
  public String addSensors(@PathVariable final String id, final Model model) {
    final SearchFilter filter = new SearchFilter();
    filter.addParam(COMPONENT_ID_ATTRIBUTE, null);
    filter.addParam(COMPONENT_ID_ATTRIBUTE, "");
    final List<Sensor> sensors = sensorService.search(filter).getContent();
    final ComponentSensorsDTO form = new ComponentSensorsDTO(id, sensors);
    model.addAttribute(Constants.MODEL_COMPONENT_SENSORS, form);
    return Constants.VIEW_ADD_SENSORS_TO_COMPONENT;
  }

  @RequestMapping(value = "/{id}/removeComponents", method = RequestMethod.POST)
  public String removeComponents(@PathVariable final String id, @Valid final ComponentsDTO components, final BindingResult result, final Model model) {
    if (!CatalogUtils.arrayIsEmpty(components.getSelectedIds())) {
      componentService.updateMulti(Arrays.asList(components.getSelectedIds()), PARENT_ID_ATTRIBUTE, null);
    }

    ModelUtils.addConfirmationMessageTo(model, "component.unassigned");
    ModelUtils.addOpenedTabTo(model, Constants.TAB_2);
    addResourceToModel(id, model);
    return Constants.VIEW_COMPONENT_DETAIL;
  }

  @RequestMapping(value = "/{id}/addSensors", method = RequestMethod.POST)
  public String addSensors(@PathVariable final String id, @Valid final ComponentSensorsDTO componentSensors, final BindingResult result, final Model model) {

    if (!CatalogUtils.arrayIsEmpty(componentSensors.getSelectedIds())) {
      sensorService.updateMulti(Arrays.asList(componentSensors.getSelectedIds()), COMPONENT_ID_ATTRIBUTE, id);
    }

    ModelUtils.addConfirmationMessageTo(model, "sensor.assigned");
    ModelUtils.addOpenedTabTo(model, Constants.TAB_3);
    addResourceToModel(id, model);
    return Constants.VIEW_COMPONENT_DETAIL;
  }

  @RequestMapping(value = "/{id}/removeSensors", method = RequestMethod.POST)
  public String removeSensors(@PathVariable final String id, @Valid final ComponentSensorsDTO sensors, final BindingResult result, final Model model) {

    if (!CatalogUtils.arrayIsEmpty(sensors.getSelectedIds())) {
      sensorService.updateMulti(Arrays.asList(sensors.getSelectedIds()), COMPONENT_ID_ATTRIBUTE, null);
    }

    ModelUtils.addConfirmationMessageTo(model, "sensor.unassigned");
    ModelUtils.addOpenedTabTo(model, Constants.TAB_3);
    addResourceToModel(id, model);
    return Constants.VIEW_COMPONENT_DETAIL;
  }

  @Override
  protected void initViewNames() {
    viewNames.put(LIST_ACTION, Constants.VIEW_COMPONENT_LIST);
    viewNames.put(DETAIL_ACTION, Constants.VIEW_COMPONENT_DETAIL);
    viewNames.put(NEW_ACTION, Constants.VIEW_NEW_COMPONENT);
  }

  @Override
  protected void doBeforeNewResource(final HttpServletRequest request, final Model model) {
    addProviderListTo(model);
  }

  @Override
  protected void doBeforeEditResource(final Model model) {
    addProviderListTo(model);
  }

  @Override
  protected void doBeforeViewResource(final String componentId, final Model model) {
    super.doBeforeViewResource(componentId, model);
    addProviderListTo(model);
  }

  private List<Provider> addProviderListTo(final Model model) {
    final List<Provider> providers = providerService.findAll();
    model.addAttribute(Constants.MODEL_PROVIDERS, providers);
    return providers;
  }
}
