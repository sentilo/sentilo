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
import java.util.List;

import javax.servlet.http.HttpServletRequest;

import org.sentilo.web.catalog.controller.CrudController;
import org.sentilo.web.catalog.domain.Component;
import org.sentilo.web.catalog.domain.ComponentType;
import org.sentilo.web.catalog.exception.BusinessValidationException;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.search.SearchFilterResult;
import org.sentilo.web.catalog.service.ComponentService;
import org.sentilo.web.catalog.service.ComponentTypesService;
import org.sentilo.web.catalog.service.CrudService;
import org.sentilo.web.catalog.utils.Constants;
import org.sentilo.web.catalog.utils.FormatUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.util.CollectionUtils;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestMapping;

@Controller
@RequestMapping("/admin/componenttypes")
public class ComponentTypesController extends CrudController<ComponentType> {

  @Autowired
  private ComponentTypesService componentTypesService;

  @Autowired
  private ComponentService componentService;

  @ModelAttribute(Constants.MODEL_ACTIVE_MENU)
  public String getActiveMenu() {
    return Constants.MENU_COMPONENT_TYPE;
  }

  @Override
  protected ComponentType buildNewEntity(final String id) {
    return new ComponentType(id);
  }

  @Override
  protected String getEntityModelKey() {
    return Constants.MODEL_COMPONENT_TYPE;
  }

  @Override
  protected CrudService<ComponentType> getService() {
    return componentTypesService;
  }

  @Override
  protected List<String> toRow(final ComponentType componentType) {
    final List<String> row = new ArrayList<String>();
    row.add(componentType.getId()); // checkbox
    row.add(componentType.getId());
    row.add(componentType.getName());
    row.add(componentType.getDescription());
    row.add(FormatUtils.formatDate(componentType.getCreatedAt()));
    return row;
  }

  @Override
  protected void initViewNames() {
    viewNames.put(LIST_ACTION, Constants.VIEW_COMPONENT_TYPE_LIST);
    viewNames.put(DETAIL_ACTION, Constants.VIEW_COMPONENT_TYPE_DETAIL);
    viewNames.put(NEW_ACTION, Constants.VIEW_NEW_COMPONENT_TYPE);
  }

  @Override
  protected void doBeforeCreateResource(final ComponentType componentType, final Model model) {
    // Todos los id de tipo de sensores los almacenamos en minusculas
    // para poder hacer la comparativa de manera rapida en el alta de sensores
    // via API.
    componentType.setId(componentType.getId().toLowerCase());
  }

  @Override
  protected void doBeforeDeleteResource(final String[] selectedIds, final HttpServletRequest request, final Model model) {
    for (final String componentType : selectedIds) {
      throwExceptionIfComponentsFoundWithType(componentType);
    }
  }

  private void throwExceptionIfComponentsFoundWithType(final String componentType) {
    final SearchFilter filter = new SearchFilter();
    filter.addAndParam("componentType", componentType);
    final SearchFilterResult<Component> components = componentService.search(filter);
    if (!CollectionUtils.isEmpty(components.getContent())) {
      throw new BusinessValidationException("componenttype.error.cannot.delete", new Object[] {componentType});
    }
  }
}
