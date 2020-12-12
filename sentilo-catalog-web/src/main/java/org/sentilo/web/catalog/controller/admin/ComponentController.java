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
package org.sentilo.web.catalog.controller.admin;

import java.util.Arrays;
import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.validation.Valid;

import org.sentilo.common.utils.SentiloUtils;
import org.sentilo.web.catalog.controller.BaseComponentController;
import org.sentilo.web.catalog.domain.Component;
import org.sentilo.web.catalog.domain.Location;
import org.sentilo.web.catalog.dto.ComponentComponentsDTO;
import org.sentilo.web.catalog.dto.ComponentSensorsDTO;
import org.sentilo.web.catalog.dto.ComponentsDTO;
import org.sentilo.web.catalog.dto.OptionDTO;
import org.sentilo.web.catalog.editor.LocationPropertyEditor;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.service.ProviderService;
import org.sentilo.web.catalog.utils.CatalogUtils;
import org.sentilo.web.catalog.utils.Constants;
import org.sentilo.web.catalog.utils.ModelUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.util.CollectionUtils;
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
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

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
  public List<OptionDTO> getComponentTypes() {
    return CatalogUtils.toOptionList(getComponentTypesService().findAll());
  }

  @RequestMapping(value = "search/json", produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  public List<Component> search(final HttpServletRequest request, @RequestParam(required = true) final String providerId, final Model model) {
    // This method is called in the alert maintenance to select the sensor of the alert.
    final SearchFilter filter = new SearchFilter();
    filter.addAndParam("providerId", providerId);
    return CatalogUtils.sortAlphabetically(getComponentService().search(filter).getContent());
  }

  @RequestMapping(value = "/{id}/addComponents", method = RequestMethod.GET)
  public String addComponents(@PathVariable final String id, final Model model) {
    final List<Component> components = CatalogUtils.sortAlphabetically(getComponentService().findAll());
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
    ModelUtils.addOpenedTabTo(model, Constants.TAB_4);
    addResourceToModel(id, model);
    return Constants.VIEW_COMPONENT_DETAIL;
  }

  @RequestMapping(value = "/{id}/removeComponents", method = RequestMethod.POST)
  public String removeComponents(@PathVariable final String id, @Valid final ComponentsDTO components, final BindingResult result,
      final Model model) {
    if (!SentiloUtils.arrayIsEmpty(components.getSelectedIds())) {
      getComponentService().updateMulti(Arrays.asList(components.getSelectedIds()), PARENT_ID_ATTRIBUTE, null);
    }

    ModelUtils.addConfirmationMessageTo(model, "component.unassigned");
    ModelUtils.addOpenedTabTo(model, Constants.TAB_4);
    addResourceToModel(id, model);
    return Constants.VIEW_COMPONENT_DETAIL;
  }

  @RequestMapping(value = "/{id}/removeSensors", method = RequestMethod.POST)
  public String removeSensors(@PathVariable final String id, @Valid final ComponentSensorsDTO sensors, final BindingResult result,
      final Model model) {

    if (!SentiloUtils.arrayIsEmpty(sensors.getSelectedIds())) {
      getSensorService().updateMulti(Arrays.asList(sensors.getSelectedIds()), COMPONENT_ID_ATTRIBUTE, null);
    }

    ModelUtils.addConfirmationMessageTo(model, "sensor.unassigned");
    ModelUtils.addOpenedTabTo(model, Constants.TAB_3);
    addResourceToModel(id, model);
    return Constants.VIEW_COMPONENT_DETAIL;
  }

  @RequestMapping(value = "/changeAccessType", method = RequestMethod.POST)
  public String changeAccessType(@RequestParam final String newAccessType, @RequestParam final String[] selectedIds, final HttpServletRequest request,
      final RedirectAttributes redirectAttributes, final Model model) {
    final boolean isPublicAccess = StringUtils.hasText(newAccessType) && "public".equals(newAccessType) ? true : false;
    getComponentService().changeAccessType(selectedIds, isPublicAccess);
    ModelUtils.addConfirmationMessageTo(model, "accessType.changed");
    return redirectToList(model, request, redirectAttributes);
  }

  @Override
  protected void initViewNames() {
    getViewNames().put(LIST_ACTION, Constants.VIEW_COMPONENT_LIST);
    getViewNames().put(DETAIL_ACTION, Constants.VIEW_COMPONENT_DETAIL);
    getViewNames().put(NEW_ACTION, Constants.VIEW_NEW_COMPONENT);
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.controller.CrudController#doBeforeNewResource(javax.servlet.http.
   * HttpServletRequest, org.springframework.ui.Model)
   */
  @Override
  protected void doBeforeNewResource(final HttpServletRequest request, final Model model) {
    super.doBeforeNewResource(request, model);

    final String providerId = request.getParameter("providerId");
    if (StringUtils.hasText(providerId)) {
      model.addAttribute(Constants.MODEL_PROVIDER_ID, providerId);
    }

    addProviderListTo(model);
    addEnergyTypesListTo(model);
    addConnectivityTypesListTo(model);
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.controller.CrudController#doBeforeEditResource(java.lang.String,
   * org.springframework.ui.Model)
   */
  @Override
  protected void doBeforeEditResource(final String id, final Model model) {
    super.doBeforeEditResource(id, model);

    addProviderListTo(model);
    addEnergyTypesListTo(model);
    addConnectivityTypesListTo(model);
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.controller.BaseComponentController#doBeforeViewResource(java.lang.
   * String , org.springframework.ui.Model)
   */
  @Override
  protected void doBeforeViewResource(final String id, final Model model) {
    super.doBeforeViewResource(id, model);

    addProviderListTo(model);
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.controller.CrudController#doBeforeUpdateResource(org.sentilo.web.
   * catalog .domain.CatalogDocument, org.springframework.ui.Model)
   */
  @Override
  protected void doBeforeUpdateResource(final Component resource, final Model model) {
    super.doBeforeUpdateResource(resource, model);

    // Before update the component, if it is mobile verify that it has the routePointList attribute
    // fill in. If it is null, first we load the route attribute from backend and we assign it to
    // the component to update.
    if (resource.isMobileComponent() && resource.getRoutePointList() == null) {
      final Component aux = getService().find(resource);
      resource.setRoutePointList(aux.getRoutePointList());
    }

    // Keep additionalInfo field
    if (CollectionUtils.isEmpty(resource.getAdditionalInfo())) {
      final Component aux = getService().find(resource);
      resource.setAdditionalInfo(aux.getAdditionalInfo());
    }
  }

  private List<OptionDTO> addProviderListTo(final Model model) {
    final List<OptionDTO> providers = CatalogUtils.toOptionList(providerService.findAll());
    model.addAttribute(Constants.MODEL_PROVIDERS, providers);
    return providers;
  }

  private void addEnergyTypesListTo(final Model model) {
    final String energyTypes = messageSource.getMessage(Constants.ENERGY_TYPES_KEY, null, LocaleContextHolder.getLocale());
    final List<OptionDTO> energyTypesList = CatalogUtils.toOptionList(energyTypes, Constants.ENERGY_TYPES_KEY, messageSource);
    model.addAttribute(Constants.MODEL_ENERGY_TYPES, energyTypesList);
  }

  private void addConnectivityTypesListTo(final Model model) {
    final String connectivityTypes = messageSource.getMessage(Constants.CONNECTIVITY_TYPES_KEY, null, LocaleContextHolder.getLocale());
    final List<OptionDTO> connectivityTypesList = CatalogUtils.toOptionList(connectivityTypes, Constants.CONNECTIVITY_TYPES_KEY, messageSource);
    model.addAttribute(Constants.MODEL_CONNECTIVITY_TYPES, connectivityTypesList);
  }

}
