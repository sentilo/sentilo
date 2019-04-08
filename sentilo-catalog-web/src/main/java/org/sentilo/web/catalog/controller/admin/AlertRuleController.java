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

import java.util.ArrayList;
import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.validation.Valid;

import org.sentilo.common.enums.AlertTriggerType;
import org.sentilo.web.catalog.controller.CrudController;
import org.sentilo.web.catalog.domain.AlertRule;
import org.sentilo.web.catalog.domain.ApplyAlertRuleResponse;
import org.sentilo.web.catalog.domain.Sensor;
import org.sentilo.web.catalog.service.AlertRuleService;
import org.sentilo.web.catalog.service.ComponentTypesService;
import org.sentilo.web.catalog.service.CrudService;
import org.sentilo.web.catalog.service.ProviderService;
import org.sentilo.web.catalog.service.SensorTypesService;
import org.sentilo.web.catalog.utils.CatalogUtils;
import org.sentilo.web.catalog.utils.Constants;
import org.sentilo.web.catalog.utils.ExcelGeneratorUtils;
import org.sentilo.web.catalog.utils.ValidationResponse;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.validation.FieldError;
import org.springframework.validation.ObjectError;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

@Controller
@RequestMapping("/admin/alertRule")
public class AlertRuleController extends CrudController<AlertRule> {

  @Autowired
  private AlertRuleService alertRuleService;

  @Autowired
  private ProviderService providerService;

  @Autowired
  private ComponentTypesService componentTypesService;

  @Autowired
  private SensorTypesService sensorTypesService;

  @ModelAttribute(Constants.MODEL_ACTIVE_MENU)
  public String getActiveMenu() {
    return Constants.MENU_ALERT_RULE;
  }

  @RequestMapping(value = "/validate", produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  public ValidationResponse processForm(final Model model, @Valid final AlertRule alertRule, final BindingResult result) {
    final ValidationResponse res = new ValidationResponse();
    if (result.hasErrors()) {
      res.setStatus(Constants.VALIDATION_FAIL);
      for (final ObjectError error : result.getAllErrors()) {
        if (error instanceof FieldError) {
          final FieldError fieldError = (FieldError) error;
          final String errorMessage =
              messageSource.getMessage(fieldError.getCode(), null, fieldError.getDefaultMessage(), LocaleContextHolder.getLocale());
          res.addError(fieldError.getField(), errorMessage);
        }
      }
    } else {
      res.setStatus(Constants.VALIDATION_SUCCESS);
    }
    return res;
  }

  @RequestMapping(value = "/{id}/reapply", method = RequestMethod.POST)
  public String reapplyAlertRule(@PathVariable final String id, final Model model, final RedirectAttributes redirectAttributes,
      final HttpServletRequest request) {

    final AlertRule alertRule = alertRuleService.find(new AlertRule(id));
    buildAlerts(alertRule, redirectAttributes, true);
    addConfirmationMessageToModel(RESOURCE_CREATED, redirectAttributes);

    return redirectToList(model, request, redirectAttributes);
  }

  @RequestMapping(value = "/confirm/json", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  public List<Sensor> getApplicableSensors(final HttpServletRequest request, @RequestParam(required = true) final String providerId,
      @RequestParam(required = false) final String componentType, @RequestParam(required = false) final String sensorType, final Model model) {
    return CatalogUtils.sortAlphabetically(alertRuleService.findSensors(providerId, componentType, sensorType));
  }

  @Override
  protected void initViewNames() {
    getViewNames().put(LIST_ACTION, Constants.VIEW_ALERT_RULE_LIST);
    getViewNames().put(DETAIL_ACTION, Constants.VIEW_ALERT_RULE_DETAIL);
    getViewNames().put(NEW_ACTION, Constants.VIEW_NEW_ALERT_RULE);
  }

  @Override
  protected List<String> toRow(final AlertRule alertRule) {
    final List<String> row = new ArrayList<String>();
    row.add(alertRule.getId()); // checkbox
    row.add(alertRule.getName());
    row.add(alertRule.getProviderId());
    row.add(alertRule.getComponentType());
    row.add(alertRule.getSensorType());
    row.add(getLocalDateFormat().printAsLocalTime(alertRule.getCreatedAt(), Constants.DATETIME_FORMAT));
    return row;
  }

  @Override
  protected List<String> toExcelRow(final AlertRule alertRule) {
    return ExcelGeneratorUtils.getAlertRuleExcelRowsData(alertRule, getLocalDateFormat());
  }

  @Override
  protected CrudService<AlertRule> getService() {
    return alertRuleService;
  }

  @Override
  protected AlertRule buildNewEntity(final String id) {
    return new AlertRule(id);
  }

  @Override
  protected String getEntityModelKey() {
    return Constants.MODEL_ALERT_RULE;
  }

  @Override
  protected void doBeforeNewResource(final HttpServletRequest request, final Model model) {
    addRuleMasterDataTo(model);
  }

  @Override
  protected void doBeforeEditResource(final String id, final Model model) {
    addRuleMasterDataTo(model);
  }

  @Override
  protected void doBeforeCreateResource(final AlertRule alertRule, final Model model) {
    alertRule.setId(Long.toString(System.currentTimeMillis()));
  }

  @Override
  protected void doAfterCreateResource(final AlertRule alertRule, final RedirectAttributes redirectAttributes, final Model model) {
    buildAlerts(alertRule, redirectAttributes, false);
  }

  private void buildAlerts(final AlertRule alertRule, final RedirectAttributes redirectAttributes, final boolean isReapplied) {
    // Create the massive alerts
    final ApplyAlertRuleResponse response = alertRuleService.createAlerts(alertRule);

    // Set the number of sensors affected by the rule and the actual number of alerts created.
    alertRule.setGeneratedAlerts(response.getGeneratedAlerts());
    alertRule.setTotalSensors(response.getTotalSensors());
    getService().update(alertRule);

    // Show the success message
    final Object[] arguments = {response.getGeneratedAlerts(), response.getTotalSensors()};
    redirectAttributes.addFlashAttribute(Constants.MODEL_CONFIRMATION_MESSAGE_ARGS_KEY, arguments);
  }

  private void addRuleMasterDataTo(final Model model) {
    addProviderListTo(model);
    addComponentTypesTo(model);
    addSensorTypesTo(model);
    addAlertTriggersTo(model);
  }

  private void addProviderListTo(final Model model) {
    model.addAttribute(Constants.MODEL_PROVIDERS, CatalogUtils.toOptionList(providerService.findAll()));
  }

  private void addComponentTypesTo(final Model model) {
    model.addAttribute(Constants.MODEL_COMPONENT_TYPES, CatalogUtils.toOptionList(componentTypesService.findAll()));
  }

  private void addSensorTypesTo(final Model model) {
    model.addAttribute(Constants.MODEL_SENSOR_TYPES, CatalogUtils.toOptionList(sensorTypesService.findAll()));
  }

  private void addAlertTriggersTo(final Model model) {
    model.addAttribute(Constants.MODEL_ALERT_RULE_TRIGGERS, CatalogUtils.toOptionList(AlertTriggerType.class, "alert.trigger", messageSource));
  }

}
