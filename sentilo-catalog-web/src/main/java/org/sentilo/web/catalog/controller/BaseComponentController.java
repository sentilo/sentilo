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
import java.util.List;
import java.util.Locale;

import javax.servlet.http.HttpServletRequest;

import org.sentilo.web.catalog.domain.Component;
import org.sentilo.web.catalog.domain.ComponentType;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.service.ComponentService;
import org.sentilo.web.catalog.service.ComponentTypesService;
import org.sentilo.web.catalog.service.CrudService;
import org.sentilo.web.catalog.service.SensorService;
import org.sentilo.web.catalog.utils.Constants;
import org.sentilo.web.catalog.utils.FormatUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.MessageSource;
import org.springframework.ui.Model;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.ModelAttribute;

public abstract class BaseComponentController extends CrudController<Component> {

  @Autowired
  protected ComponentService componentService;

  @Autowired
  protected ComponentTypesService componentTypesService;

  @Autowired
  protected MessageSource messageSource;

  @Autowired
  protected SensorService sensorService;

  @ModelAttribute(Constants.MODEL_ACTIVE_MENU)
  public String getActiveMenu() {
    return Constants.MENU_COMPONENT;
  }

  @Override
  protected List<String> toRow(final Component component) {
    final List<String> row = new ArrayList<String>();
    row.add(component.getId()); // checkbox
    row.add(component.getName());
    row.add(component.getDescription());
    row.add(component.getProviderId());
    if (component.isMobileComponent()) {
      row.add(FormatUtils.label(messageSource.getMessage("mobile", null, Locale.getDefault())));
    } else {
      row.add(FormatUtils.label(messageSource.getMessage("static", null, Locale.getDefault())));
    }
    row.add(FormatUtils.formatDate(component.getCreatedAt()));
    return row;
  }

  @Override
  protected CrudService<Component> getService() {
    return componentService;
  }

  @Override
  protected Component buildNewEntity(final String id) {
    return new Component(id);
  }

  @Override
  protected String getEntityModelKey() {
    return Constants.MODEL_COMPONENT;
  }

  @Override
  protected void doBeforeSearchPage(final HttpServletRequest request, final SearchFilter filter) {
    final String parentId = request.getParameter("parentId");
    if (StringUtils.hasText(parentId)) {
      filter.addAndParam("parentId", parentId);
    }
  }

  @Override
  protected void doBeforeViewResource(final String componentId, final Model model) {
    addComponentIconTo(model, componentId);
  }

  /**
   * Adds the component icon to the model.
   * 
   * @param model
   * @param componentId
   */
  protected void addComponentIconTo(final Model model, final String componentId) {
    final Component component = componentService.find(new Component(componentId));
    if (component != null) {
      final ComponentType type = componentTypesService.find(new ComponentType(component.getComponentType()));
      if (type != null) {
        model.addAttribute(Constants.MODEL_COMPONENT_ICON, type.getIcon());
      }
    }
  }
}
