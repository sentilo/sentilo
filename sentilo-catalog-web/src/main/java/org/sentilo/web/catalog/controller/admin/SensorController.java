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
import java.util.Collections;
import java.util.List;

import javax.servlet.http.HttpServletRequest;

import org.sentilo.common.domain.OrderMessage;
import org.sentilo.platform.client.core.domain.AlarmMessage;
import org.sentilo.platform.client.core.domain.Observation;
import org.sentilo.web.catalog.controller.CrudController;
import org.sentilo.web.catalog.domain.Component;
import org.sentilo.web.catalog.domain.Provider;
import org.sentilo.web.catalog.domain.Sensor;
import org.sentilo.web.catalog.domain.SensorType;
import org.sentilo.web.catalog.dto.ObservationDTO;
import org.sentilo.web.catalog.dto.OptionDTO;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.service.ComponentService;
import org.sentilo.web.catalog.service.CrudService;
import org.sentilo.web.catalog.service.ProviderService;
import org.sentilo.web.catalog.service.SensorService;
import org.sentilo.web.catalog.service.SensorTypesService;
import org.sentilo.web.catalog.utils.AlarmMessageComparator;
import org.sentilo.web.catalog.utils.Constants;
import org.sentilo.web.catalog.utils.FormatUtils;
import org.sentilo.web.catalog.utils.ModelUtils;
import org.sentilo.web.catalog.utils.OrderMessageComparator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.MessageSource;
import org.springframework.context.NoSuchMessageException;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

@Controller
@RequestMapping("/admin/sensor")
public class SensorController extends CrudController<Sensor> {

  @Autowired
  private SensorService sensorService;

  @Autowired
  private ProviderService providerService;

  @Autowired
  private ComponentService componentService;

  @Autowired
  private SensorTypesService sensorTypeService;

  @Autowired
  private MessageSource messageSource;

  @ModelAttribute(Constants.MODEL_ACTIVE_MENU)
  public String getActiveMenu() {
    return Constants.MENU_SENSOR;
  }

  @ModelAttribute(Constants.MODEL_SENSOR_TYPES)
  public List<SensorType> getSensorTypes() {
    return sensorTypeService.findAll();
  }

  @ModelAttribute(Constants.MODEL_SENSOR_DATA_TYPES)
  public Sensor.DataType[] getSensorDataTypes(final Model model) {
    return Sensor.DataType.values();
  }

  @RequestMapping("/search/json")
  @ResponseBody
  public List<Sensor> search(final HttpServletRequest request, @RequestParam(required = true) final String search,
      @RequestParam(required = true) final String providerId, @RequestParam(required = false) final String componentId, final Model model) {
    // This method is called in the alert maintenance to select the sensor of the alert.
    final SearchFilter filter = getSearchFilterBuilder().buildSearchFilter(request, null, search);
    filter.addAndParam("providerId", providerId);
    if (StringUtils.hasText(componentId)) {
      filter.addAndParam("componentId", componentId);
    }
    return sensorService.search(filter).getContent();
  }

  @RequestMapping(value = "/{id}/data", method = RequestMethod.GET)
  public String retrieveProviderSensorData(@PathVariable final String id, final Model model) {
    ModelUtils.setDataMode(model);
    final Sensor sensor = addResourceToModel(id, model);
    addSensorLastObservationToModel(model, sensor);
    return Constants.VIEW_SENSOR_DETAIL;
  }

  @RequestMapping(value = "/lastOb/{sensorId}", method = RequestMethod.GET)
  @ResponseBody
  public ObservationDTO getLastObservation(@PathVariable final String sensorId) {
    final Sensor sensor = sensorService.find(new Sensor(sensorId));
    if (sensor != null) {
      translateIdForNameSensorType(sensor);
      final Observation observation = sensorService.getLastObservation(sensor);
      return new ObservationDTO(sensor, observation);
    }
    return null;
  }

  @RequestMapping(value = "/lastObs/{sensorId}", method = RequestMethod.GET)
  @ResponseBody
  public List<Observation> getLastObservations(@PathVariable final String sensorId) {
    final Sensor sensor = sensorService.find(new Sensor(sensorId));
    final List<Observation> observations = sensorService.getLastObservations(sensor);
    Collections.reverse(observations);
    return observations;
  }

  @RequestMapping(value = "/lastAlarms/{sensorId}", method = RequestMethod.GET)
  @ResponseBody
  public List<AlarmMessage> getLastAlarms(@PathVariable final String sensorId) {
    final Sensor sensor = sensorService.find(new Sensor(sensorId));
    final List<AlarmMessage> alarmMessages = sensorService.getLastAlarmsMessages(sensor);
    Collections.sort(alarmMessages, Collections.reverseOrder(new AlarmMessageComparator()));
    return alarmMessages;
  }

  @RequestMapping(value = "/lastOrders/{sensorId}", method = RequestMethod.GET)
  @ResponseBody
  public List<OrderMessage> getLastOrders(@PathVariable final String sensorId) {
    final Sensor sensor = sensorService.find(new Sensor(sensorId));
    final List<OrderMessage> orderMessages = sensorService.getLastOrderMessages(sensor);
    Collections.sort(orderMessages, Collections.reverseOrder(new OrderMessageComparator()));
    return orderMessages;
  }

  @RequestMapping(value = "/changeAccessType", method = RequestMethod.POST)
  public String changeAccessType(@RequestParam final String newAccessType, @RequestParam final String[] selectedIds,
      final HttpServletRequest request, final Model model) {
    final boolean isPublicAccess = (StringUtils.hasText(newAccessType) && "public".equals(newAccessType) ? true : false);
    sensorService.changeAccessType(selectedIds, isPublicAccess);
    ModelUtils.addConfirmationMessageTo(model, "accessType.changed");
    return getNameOfViewToReturn(LIST_ACTION);
  }

  @Override
  protected List<String> toRow(final Sensor sensor) {
    final List<String> row = new ArrayList<String>();
    row.add(sensor.getId()); // checkbox
    row.add(sensor.getSensorId());
    row.add(sensor.getProviderId());
    row.add(FormatUtils.label(sensor.getType()));
    row.add(String.valueOf(sensor.getPublicAccess()));
    row.add(getLocalDateFormat().printAsLocalTime(sensor.getCreatedAt(), Constants.DATETIME_FORMAT));
    return row;
  }

  @Override
  protected void initViewNames() {
    getViewNames().put(LIST_ACTION, Constants.VIEW_SENSOR_LIST);
    getViewNames().put(DETAIL_ACTION, Constants.VIEW_SENSOR_DETAIL);
    getViewNames().put(NEW_ACTION, Constants.VIEW_NEW_SENSOR);
  }

  @Override
  protected CrudService<Sensor> getService() {
    return sensorService;
  }

  @Override
  protected Sensor buildNewEntity(final String id) {
    return new Sensor(id);
  }

  @Override
  protected String getEntityModelKey() {
    return Constants.MODEL_SENSOR;
  }

  @Override
  protected void doBeforeNewResource(final HttpServletRequest request, final Model model) {
    if (CollectionUtils.isEmpty(addProviderListTo(model))) {
      ModelUtils.addErrorMessageTo(model, "error.no.providers");
    }
    final String providerId = request.getParameter("providerId");
    if (StringUtils.hasText(providerId)) {
      model.addAttribute(Constants.MODEL_PROVIDER_ID, providerId);
    }

    addEnergyTypesListTo(model);
    addConnectivityTypesListTo(model);
  }

  @Override
  protected void doBeforeEditResource(final Model model) {
    addProviderListTo(model);
    addComponentListTo(model);
    addEnergyTypesListTo(model);
    addConnectivityTypesListTo(model);
  }

  @Override
  protected void doBeforeViewResource(final String sensorId, final Model model) {
    addProviderListTo(model);
    addComponentListTo(model);
  }

  @Override
  protected void doBeforeSearchPage(final HttpServletRequest request, final SearchFilter filter) {
    // Filter the list of sensors per provider, if need be
    final String providerId = request.getParameter("providerId");
    if (StringUtils.hasText(providerId)) {
      filter.addAndParam("providerId", providerId);
    }
    // Filter the list of sensors per component, if need be
    final String componentId = request.getParameter("componentId");
    if (StringUtils.hasText(componentId)) {
      filter.addAndParam("componentId", componentId);
    }
  }

  @Override
  protected void doBeforeExcelBuilder(final Model model) {
    final String[] listColumnNames =
        {Constants.SENSOR_ID_PROP, Constants.PROVIDER_ID_PROP, Constants.TYPE_PROP, Constants.PUBLIC_ACCESS_PROP, Constants.CREATED_AT_PROP};

    model.addAttribute(Constants.LIST_COLUMN_NAMES, Arrays.asList(listColumnNames));
    model.addAttribute(Constants.MESSAGE_KEYS_PREFFIX, "sensor");
  }

  private List<Provider> addProviderListTo(final Model model) {
    final List<Provider> providers = providerService.findAll();
    model.addAttribute(Constants.MODEL_PROVIDERS, providers);
    return providers;
  }

  private List<Component> addComponentListTo(final Model model) {
    final List<Component> components = componentService.findAll();
    model.addAttribute(Constants.MODEL_COMPONENTS, components);
    return components;
  }

  private void addSensorLastObservationToModel(final Model model, final Sensor sensor) {
    final Observation observation = sensorService.getLastObservation(sensor);
    if (observation == null) {
      ModelUtils.addErrorMessageTo(model, "sensor.error.nodata");
    }
    model.addAttribute(Constants.MODEL_SENSOR_LAST_OBSERVATION, observation);
  }

  private void addEnergyTypesListTo(final Model model) {
    final List<OptionDTO> options = new ArrayList<OptionDTO>();
    final String energyTypes = messageSource.getMessage(Constants.ENERGY_TYPES_KEY, null, LocaleContextHolder.getLocale());
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
    final String connectivityTypes = messageSource.getMessage(Constants.CONNECTIVITY_TYPES_KEY, null, LocaleContextHolder.getLocale());
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
      label = messageSource.getMessage(key, null, LocaleContextHolder.getLocale());
    } catch (final NoSuchMessageException nme) {
      logger.warn("Message key {} couldn't be resolved. Return default value {}", key, defaultValue);
    }

    return label;
  }

  private void translateIdForNameSensorType(final Sensor sensor) {
    final SensorType type = sensorTypeService.find(new SensorType(sensor.getType()));
    if (type != null) {
      sensor.setType(type.getName());
    }
  }

}
