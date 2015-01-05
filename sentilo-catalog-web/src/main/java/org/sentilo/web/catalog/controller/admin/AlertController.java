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

import org.sentilo.common.utils.AlertTriggerType;
import org.sentilo.web.catalog.controller.CrudController;
import org.sentilo.web.catalog.domain.Alert;
import org.sentilo.web.catalog.domain.Alert.Type;
import org.sentilo.web.catalog.service.AlertService;
import org.sentilo.web.catalog.service.ApplicationService;
import org.sentilo.web.catalog.service.CrudService;
import org.sentilo.web.catalog.service.ProviderService;
import org.sentilo.web.catalog.utils.Constants;
import org.sentilo.web.catalog.utils.FormatUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestMapping;

@Controller
@RequestMapping("/admin/alert")
public class AlertController extends CrudController<Alert> {

  @Autowired
  private AlertService alertService;

  @Autowired
  private ApplicationService applicationService;

  @Autowired
  private ProviderService providerService;

  @ModelAttribute(Constants.MODEL_ACTIVE_MENU)
  public String getActiveMenu() {
    return Constants.MENU_ALERT;
  }

  @Override
  protected void initViewNames() {
    getViewNames().put(LIST_ACTION, Constants.VIEW_ALERT_LIST);
    getViewNames().put(DETAIL_ACTION, Constants.VIEW_ALERT_DETAIL);
    getViewNames().put(NEW_ACTION, Constants.VIEW_NEW_ALERT);
  }

  @Override
  protected List<String> toRow(final Alert alert) {
    final List<String> row = new ArrayList<String>();
    row.add(alert.getId()); // checkbox
    row.add(alert.getId());
    row.add(alert.getName());
    row.add(FormatUtils.label(messageSource.getMessage("alert.type." + alert.getType().toString(), null, LocaleContextHolder.getLocale())));
    row.add(getLocalDateFormat().printAsLocalTime(alert.getCreatedAt(), Constants.DATETIME_FORMAT));
    return row;
  }

  private void addAlertMasterDataTo(final Model model) {
    addProviderListTo(model);
    addApplicationListTo(model);
    addAlertTypesTo(model);
    addAlertTriggersTo(model);
  }

  private void addProviderListTo(final Model model) {
    model.addAttribute(Constants.MODEL_PROVIDERS, providerService.findAll());
  }

  private void addApplicationListTo(final Model model) {
    model.addAttribute(Constants.MODEL_APPLICATIONS, applicationService.findAll());
  }

  private void addAlertTypesTo(final Model model) {
    model.addAttribute(Constants.MODEL_ALERT_TYPES, Alert.Type.values());
  }

  private void addAlertTriggersTo(final Model model) {
    model.addAttribute(Constants.MODEL_ALERT_TRIGGERS, AlertTriggerType.values());
  }

  @Override
  protected CrudService<Alert> getService() {
    return alertService;
  }

  @Override
  protected Alert buildNewEntity(final String id) {
    return new Alert(id);
  }

  @Override
  protected String getEntityModelKey() {
    return Constants.MODEL_ALERT;
  }

  @Override
  protected void doBeforeNewResource(final HttpServletRequest request, final Model model) {
    addAlertMasterDataTo(model);
  }

  @Override
  protected void doBeforeEditResource(final Model model) {
    addAlertMasterDataTo(model);
  }

  @Override
  protected void doBeforeCreateResource(final Alert alert, final Model model) {
    // If the alert is internal we put to null all specific attributes of an internal alert.
    // Same with an external alert

    if (Type.INTERNAL.equals(alert.getType())) {
      alert.setApplicationId(null);
    } else {
      alert.setComponentId(null);
      alert.setSensorId(null);
      alert.setTrigger(null);
      alert.setExpression(null);

      if (StringUtils.hasText(alert.getProviderId())) {
        alert.setApplicationId(null);
      } else if (StringUtils.hasText(alert.getApplicationId())) {
        alert.setProviderId(null);
      }

    }
  }

  @Override
  protected void doBeforeExcelBuilder(final Model model) {
    final String[] listColumnNames = {Constants.ID_PROP, Constants.NAME_PROP, Constants.TYPE_PROP, Constants.CREATED_AT_PROP};

    model.addAttribute(Constants.LIST_COLUMN_NAMES, Arrays.asList(listColumnNames));
    model.addAttribute(Constants.MESSAGE_KEYS_PREFFIX, "alert");

  }

}
