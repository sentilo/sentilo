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
import java.util.Arrays;
import java.util.List;

import javax.servlet.http.HttpServletRequest;

import org.sentilo.web.catalog.domain.Component;
import org.sentilo.web.catalog.domain.ComponentType;
import org.sentilo.web.catalog.format.datetime.LocalDateFormatter;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.service.ComponentService;
import org.sentilo.web.catalog.service.ComponentTypesService;
import org.sentilo.web.catalog.service.CrudService;
import org.sentilo.web.catalog.service.SensorService;
import org.sentilo.web.catalog.utils.Constants;
import org.sentilo.web.catalog.utils.FormatUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.MessageSource;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.ui.Model;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.ModelAttribute;

public abstract class BaseComponentController extends CrudController<Component> {

  @Autowired
  private ComponentService componentService;

  @Autowired
  private ComponentTypesService componentTypesService;

  @Autowired
  private MessageSource messageSource;

  @Autowired
  private SensorService sensorService;

  @Autowired
  private LocalDateFormatter localDateFormat;

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
      row.add(FormatUtils.label(getMessageSource().getMessage("mobile", null, LocaleContextHolder.getLocale())));
    } else {
      row.add(FormatUtils.label(getMessageSource().getMessage("static", null, LocaleContextHolder.getLocale())));
    }
    row.add(FormatUtils.label(component.getComponentType()));
    row.add(String.valueOf(component.getPublicAccess()));
    row.add(localDateFormat.printAsLocalTime(component.getCreatedAt(), Constants.DATETIME_FORMAT));
    return row;
  }

  @Override
  protected CrudService<Component> getService() {
    return getComponentService();
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
    final String providerId = request.getParameter("providerId");
    final String parentId = request.getParameter("parentId");

    if (StringUtils.hasText(providerId)) {
      filter.addAndParam("providerId", providerId);
    }

    if (StringUtils.hasText(parentId)) {
      filter.addAndParam("parentId", parentId);
    }
  }

  @Override
  protected void doBeforeViewResource(final String componentId, final Model model) {
    addComponentIconTo(model, componentId);
  }

  @Override
  protected void initViewNames() {
    // TODO Auto-generated method stub

  }

  /**
   * Adds the component icon to the model.
   * 
   * @param model
   * @param componentId
   */
  protected void addComponentIconTo(final Model model, final String componentId) {
    final Component component = getComponentService().find(new Component(componentId));
    if (component != null) {
      final ComponentType type = getComponentTypesService().find(new ComponentType(component.getComponentType()));
      if (type != null) {
        model.addAttribute(Constants.MODEL_COMPONENT_ICON, type.getIcon());
      }
    }
  }

  @Override
  protected void doBeforeExcelBuilder(final Model model) {
    final String[] listColumnNames =
        {Constants.NAME_PROP, Constants.DESCRIPTION_PROP, Constants.PROVIDER_ID_PROP, Constants.LOCATION_PROP, Constants.TYPE_PROP,
            Constants.PUBLIC_ACCESS_PROP, Constants.CREATED_AT_PROP};

    model.addAttribute(Constants.LIST_COLUMN_NAMES, Arrays.asList(listColumnNames));
    model.addAttribute(Constants.MESSAGE_KEYS_PREFFIX, "component");
  }

  protected ComponentService getComponentService() {
    return componentService;
  }

  protected ComponentTypesService getComponentTypesService() {
    return componentTypesService;
  }

  protected MessageSource getMessageSource() {
    return messageSource;
  }

  protected SensorService getSensorService() {
    return sensorService;
  }
}
