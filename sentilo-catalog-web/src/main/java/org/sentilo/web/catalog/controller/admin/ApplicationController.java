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

import org.sentilo.web.catalog.controller.CrudController;
import org.sentilo.web.catalog.domain.Alert;
import org.sentilo.web.catalog.domain.Application;
import org.sentilo.web.catalog.exception.BusinessValidationException;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.service.AlertService;
import org.sentilo.web.catalog.service.ApplicationService;
import org.sentilo.web.catalog.service.CrudService;
import org.sentilo.web.catalog.utils.Constants;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestMapping;

@Controller
@RequestMapping("/admin/application")
public class ApplicationController extends CrudController<Application> {

  @Autowired
  private ApplicationService applicationService;

  @Autowired
  private AlertService alertService;

  @ModelAttribute(Constants.MODEL_ACTIVE_MENU)
  public String getActiveMenu() {
    return Constants.MENU_APPLICATION;
  }

  @Override
  protected List<String> toRow(final Application application) {
    final List<String> row = new ArrayList<String>();
    row.add(application.getId()); // checkbox
    row.add(application.getId());
    row.add(application.getName());
    row.add(application.getDescription());
    row.add(getLocalDateFormat().printAsLocalTime(application.getCreatedAt(), Constants.DATETIME_FORMAT));
    return row;
  }

  @Override
  protected void initViewNames() {
    getViewNames().put(LIST_ACTION, Constants.VIEW_APPLICATION_LIST);
    getViewNames().put(DETAIL_ACTION, Constants.VIEW_APPLICATION_DETAIL);
    getViewNames().put(NEW_ACTION, Constants.VIEW_NEW_APPLICATION);
  }

  @Override
  protected void doBeforeDeleteResource(final String[] selectedIds, final HttpServletRequest request, final Model model) {
    throwExceptionIfAlarmFound(selectedIds);
  }

  @Override
  protected CrudService<Application> getService() {
    return applicationService;
  }

  @Override
  protected Application buildNewEntity(final String id) {
    return new Application(id);
  }

  @Override
  protected String getEntityModelKey() {
    return Constants.MODEL_APPLICATION;
  }

  @Override
  protected void doBeforeExcelBuilder(final Model model) {
    final String[] listColumnNames = {Constants.ID_PROP, Constants.NAME_PROP, Constants.DESCRIPTION_PROP, Constants.CREATED_AT_PROP};

    model.addAttribute(Constants.LIST_COLUMN_NAMES, Arrays.asList(listColumnNames));
    model.addAttribute(Constants.MESSAGE_KEYS_PREFFIX, "application");
  }

  private void throwExceptionIfAlarmFound(final String[] selectedIds) {
    for (final String applicationId : selectedIds) {
      final SearchFilter filter = new SearchFilter();
      filter.addParam("type", Alert.Type.EXTERNAL.toString());
      filter.addAndParam("applicationId", applicationId);
      final boolean alertFound = alertService.search(filter).getContent().size() > 0;
      if (alertFound) {
        throw new BusinessValidationException("application.error.existing.alerts");
      }
    }
  }

}
