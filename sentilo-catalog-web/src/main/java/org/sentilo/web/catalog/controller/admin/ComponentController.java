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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.validation.Valid;

import org.sentilo.common.utils.SentiloUtils;
import org.sentilo.web.catalog.controller.BaseComponentController;
import org.sentilo.web.catalog.domain.Component;
import org.sentilo.web.catalog.domain.ComponentType;
import org.sentilo.web.catalog.domain.Location;
import org.sentilo.web.catalog.domain.Provider;
import org.sentilo.web.catalog.dto.ComponentComponentsDTO;
import org.sentilo.web.catalog.dto.ComponentSensorsDTO;
import org.sentilo.web.catalog.dto.ComponentsDTO;
import org.sentilo.web.catalog.dto.OptionDTO;
import org.sentilo.web.catalog.editor.LocationPropertyEditor;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.service.ProviderService;
import org.sentilo.web.catalog.utils.Constants;
import org.sentilo.web.catalog.utils.ModelUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.NoSuchMessageException;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.util.StringUtils;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.ServletRequestDataBinder;
import org.springframework.web.bind.annotation.InitBinder;
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

  @InitBinder
  protected void initBinder(final HttpServletRequest request, final ServletRequestDataBinder binder) {
    binder.registerCustomEditor(Location.class, "location", new LocationPropertyEditor());
  }

  @ModelAttribute(Constants.MODEL_COMPONENT_TYPES)
  public List<ComponentType> getComponentTypes() {
    return getComponentTypesService().findAll();
  }

  @RequestMapping("/search/json")
  @ResponseBody
  public List<Component> search(final HttpServletRequest request, @RequestParam(required = true) final String providerId, final Model model) {
    // This method is called in the alert maintenance to select the sensor of the alert.
    final SearchFilter filter = new SearchFilter();
    filter.addAndParam("providerId", providerId);
    return getComponentService().search(filter).getContent();
  }

  @RequestMapping(value = "/{id}/addComponents", method = RequestMethod.GET)
  public String addComponents(@PathVariable final String id, final Model model) {
    final List<Component> components = getComponentService().findAll();
    final ComponentComponentsDTO form = new ComponentComponentsDTO(id, components);
    model.addAttribute(Constants.MODEL_COMPONENT_COMPONENTS, form);
    return Constants.VIEW_ADD_COMPONENTS_TO_COMPONENT;
  }

  @RequestMapping(value = "/{id}/addComponents", method = RequestMethod.POST)
  public String assignComponents(@PathVariable final String id, @Valid final ComponentComponentsDTO componentComponents, final BindingResult result,
      final Model model) {

    if (!SentiloUtils.arrayIsEmpty(componentComponents.getSelectedIds())) {
      getComponentService().updateMulti(Arrays.asList(componentComponents.getSelectedIds()), PARENT_ID_ATTRIBUTE, id);
    }

    ModelUtils.addConfirmationMessageTo(model, "component.assigned");
    ModelUtils.addOpenedTabTo(model, Constants.TAB_2);
    addResourceToModel(id, model);
    return Constants.VIEW_COMPONENT_DETAIL;
  }

  @RequestMapping(value = "/{id}/removeComponents", method = RequestMethod.POST)
  public String removeComponents(@PathVariable final String id, @Valid final ComponentsDTO components, final BindingResult result, final Model model) {
    if (!SentiloUtils.arrayIsEmpty(components.getSelectedIds())) {
      getComponentService().updateMulti(Arrays.asList(components.getSelectedIds()), PARENT_ID_ATTRIBUTE, null);
    }

    ModelUtils.addConfirmationMessageTo(model, "component.unassigned");
    ModelUtils.addOpenedTabTo(model, Constants.TAB_2);
    addResourceToModel(id, model);
    return Constants.VIEW_COMPONENT_DETAIL;
  }

  @RequestMapping(value = "/{id}/addSensors", method = RequestMethod.POST)
  public String addSensors(@PathVariable final String id, @Valid final ComponentSensorsDTO componentSensors, final BindingResult result,
      final Model model) {

    if (!SentiloUtils.arrayIsEmpty(componentSensors.getSelectedIds())) {
      getSensorService().updateMulti(Arrays.asList(componentSensors.getSelectedIds()), COMPONENT_ID_ATTRIBUTE, id);
    }

    ModelUtils.addConfirmationMessageTo(model, "sensor.assigned");
    ModelUtils.addOpenedTabTo(model, Constants.TAB_3);
    addResourceToModel(id, model);
    return Constants.VIEW_COMPONENT_DETAIL;
  }

  @RequestMapping(value = "/{id}/removeSensors", method = RequestMethod.POST)
  public String removeSensors(@PathVariable final String id, @Valid final ComponentSensorsDTO sensors, final BindingResult result, final Model model) {

    if (!SentiloUtils.arrayIsEmpty(sensors.getSelectedIds())) {
      getSensorService().updateMulti(Arrays.asList(sensors.getSelectedIds()), COMPONENT_ID_ATTRIBUTE, null);
    }

    ModelUtils.addConfirmationMessageTo(model, "sensor.unassigned");
    ModelUtils.addOpenedTabTo(model, Constants.TAB_3);
    addResourceToModel(id, model);
    return Constants.VIEW_COMPONENT_DETAIL;
  }

  @RequestMapping(value = "/changeAccessType", method = RequestMethod.POST)
  public String changeAccessType(@RequestParam final String newAccessType, @RequestParam final String[] selectedIds,
      final HttpServletRequest request, final Model model) {
    final boolean isPublicAccess = (StringUtils.hasText(newAccessType) && "public".equals(newAccessType) ? true : false);
    getComponentService().changeAccessType(selectedIds, isPublicAccess);
    ModelUtils.addConfirmationMessageTo(model, "accessType.changed");
    return getNameOfViewToReturn(LIST_ACTION);
  }

  @Override
  protected void initViewNames() {
    getViewNames().put(LIST_ACTION, Constants.VIEW_COMPONENT_LIST);
    getViewNames().put(DETAIL_ACTION, Constants.VIEW_COMPONENT_DETAIL);
    getViewNames().put(NEW_ACTION, Constants.VIEW_NEW_COMPONENT);
  }

  @Override
  protected void doBeforeNewResource(final HttpServletRequest request, final Model model) {
    final String providerId = request.getParameter("providerId");
    if (StringUtils.hasText(providerId)) {
      model.addAttribute(Constants.MODEL_PROVIDER_ID, providerId);
    }
    addProviderListTo(model);
    addEnergyTypesListTo(model);
    addConnectivityTypesListTo(model);
  }

  @Override
  protected void doBeforeEditResource(final Model model) {
    addProviderListTo(model);
    addEnergyTypesListTo(model);
    addConnectivityTypesListTo(model);
  }

  @Override
  protected void doBeforeViewResource(final String componentId, final Model model) {
    super.doBeforeViewResource(componentId, model);
    addProviderListTo(model);
  }

  @Override
  protected void doBeforeUpdateResource(final Component resource, final Model model) {
    // Before update the component, if it is mobile verify that it has the routePointList attribute
    // fill in. If it is null, first we load the route attribute from backend and we assign it to
    // the component to update.
    if (resource.isMobileComponent() && resource.getRoutePointList() == null) {
      final Component aux = getService().find(resource);
      resource.setRoutePointList(aux.getRoutePointList());
    }
  }

  private List<Provider> addProviderListTo(final Model model) {
    final List<Provider> providers = providerService.findAll();
    model.addAttribute(Constants.MODEL_PROVIDERS, providers);
    return providers;
  }

  private void addEnergyTypesListTo(final Model model) {
    final List<OptionDTO> options = new ArrayList<OptionDTO>();
    final String energyTypes = getMessageSource().getMessage(Constants.ENERGY_TYPES_KEY, null, LocaleContextHolder.getLocale());
    if (StringUtils.hasText(energyTypes)) {
      final String[] energyTypesList = energyTypes.split(Constants.COMMA_TOKEN_SPLITTER);
      for (final String energyType : energyTypesList) {
        final String energyTypesKey = Constants.ENERGY_TYPES_KEY.concat(Constants.DEFAULT_KEY_TOKEN_SPLITTER).concat(energyType);
        final String label = getOptionLabel(energyTypesKey, energyType);
        options.add(new OptionDTO(label, energyType));
      }
    }

    model.addAttribute(Constants.MODEL_ENERGY_TYPES, options);
  }

  private void addConnectivityTypesListTo(final Model model) {
    final List<OptionDTO> options = new ArrayList<OptionDTO>();
    final String connectivityTypes = getMessageSource().getMessage(Constants.CONNECTIVITY_TYPES_KEY, null, LocaleContextHolder.getLocale());
    if (StringUtils.hasText(connectivityTypes)) {
      final String[] connectivityTypesList = connectivityTypes.split(Constants.COMMA_TOKEN_SPLITTER);
      for (final String connectivityType : connectivityTypesList) {
        final String connectivityTypesKey = Constants.CONNECTIVITY_TYPES_KEY.concat(Constants.DEFAULT_KEY_TOKEN_SPLITTER).concat(connectivityType);
        final String label = getOptionLabel(connectivityTypesKey, connectivityType);
        options.add(new OptionDTO(label, connectivityType));
      }
    }

    model.addAttribute(Constants.MODEL_CONNECTIVITY_TYPES, options);
  }

  private String getOptionLabel(final String key, final String defaultValue) {
    String label = defaultValue;
    try {
      label = getMessageSource().getMessage(key, null, LocaleContextHolder.getLocale());
    } catch (final NoSuchMessageException nme) {
      logger.warn("Message key {} couldn't be resolved. Return default value {}", key, defaultValue);
    }

    return label;
  }
}
