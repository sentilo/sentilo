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
package org.sentilo.web.catalog.service.impl;

import java.util.ArrayList;
import java.util.List;

import org.sentilo.common.enums.SensorState;
import org.sentilo.web.catalog.domain.Alert;
import org.sentilo.web.catalog.domain.Alert.Type;
import org.sentilo.web.catalog.domain.AlertRule;
import org.sentilo.web.catalog.domain.ApplyAlertRuleResponse;
import org.sentilo.web.catalog.domain.Component;
import org.sentilo.web.catalog.domain.Sensor;
import org.sentilo.web.catalog.repository.AlertRuleRepository;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.search.SearchFilterResult;
import org.sentilo.web.catalog.service.AlertRuleService;
import org.sentilo.web.catalog.service.AlertService;
import org.sentilo.web.catalog.service.ComponentService;
import org.sentilo.web.catalog.service.SensorService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

@Service
public class AlertRuleServiceImpl extends AbstractBaseCrudServiceImpl<AlertRule> implements AlertRuleService {

  @Autowired
  private AlertRuleRepository repository;

  @Autowired
  private ComponentService componentService;

  @Autowired
  private SensorService sensorService;

  @Autowired
  private AlertService alertService;

  public AlertRuleServiceImpl() {
    super(AlertRule.class);
  }

  @Override
  public MongoRepository<AlertRule, String> getRepository() {
    return repository;
  }

  @Override
  public String getEntityId(final AlertRule entity) {
    return entity.getId();
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.service.AlertRuleService#findSensors(java.lang.String,
   * java.lang.String, java.lang.String)
   */
  public List<Sensor> findSensors(final String providerId, final String componentType, final String sensorType) {
    final List<Sensor> sensors = new ArrayList<Sensor>();

    if (StringUtils.hasText(providerId)) {
      if (StringUtils.hasText(componentType)) {
        final List<Component> components = findComponents("providerId", providerId, "componentType", componentType);
        for (final Component component : components) {
          sensors.addAll(findSensors("providerId", providerId, "componentId", component.getId(), "type", sensorType));
        }
      } else {
        sensors.addAll(findSensors("providerId", providerId, "type", sensorType));
      }
    }

    return sensors;
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.web.catalog.service.AlertRuleService#createAlerts(org.sentilo.web.catalog.domain
   * .AlertRule)
   */
  public ApplyAlertRuleResponse createAlerts(final AlertRule alertRule) {
    final ApplyAlertRuleResponse response = new ApplyAlertRuleResponse();

    // Get the applicable sensors
    final List<Sensor> sensors = findSensors(alertRule.getProviderId(), alertRule.getComponentType(), alertRule.getSensorType());
    int generatedAlerts = 0;
    for (final Sensor sensor : sensors) {
      if (createAlertIfNeedBe(sensor, alertRule)) {
        generatedAlerts++;
      }
    }

    response.setGeneratedAlerts(generatedAlerts);
    response.setTotalSensors(sensors.size());

    return response;
  }

  protected boolean createAlertIfNeedBe(final Sensor sensor, final AlertRule alertRule) {
    final Alert alert = buildAlert(sensor, alertRule);
    boolean alertCreated = false;
    if (!alertAlreadyExists(alert)) {
      alertService.create(alert);
      alertCreated = true;
    }

    return alertCreated;
  }

  /**
   * Checks if already exists an alert with equal conditions (sensor and trigger expression)
   *
   * @param alert
   * @return
   */
  protected boolean alertAlreadyExists(final Alert alert) {
    final SearchFilter filter = new SearchFilter();
    filter.addAndParam("providerId", alert.getProviderId());
    filter.addAndParam("sensorId", alert.getSensorId());
    filter.addAndParam("trigger", alert.getTrigger());
    filter.addAndParam("expression", alert.getExpression());
    return alertService.search(filter).hasContent();
  }

  protected String buildAlertId(final Sensor sensor, final AlertRule alertRule, final long createdTime) {
    // Alerts generated automatically must have the following id:
    // PROVIDER_ID+'_'+SENSOR_ID+"_"+TRIGGER_TYPE+"_"+TIMESTAMP
    return alertRule.getProviderId() + "_" + sensor.getSensorId() + "_" + alertRule.getTrigger().name() + "_" + createdTime;
  }

  protected Alert buildAlert(final Sensor sensor, final AlertRule alertRule) {
    final String descPattern = "This alert was created automatically by the rule alert : %s";
    final String alertId = buildAlertId(sensor, alertRule, System.currentTimeMillis());

    final Alert alert = new Alert(alertId);
    alert.setName(alertId);
    alert.setDescription(String.format(descPattern, alertRule.getName()));
    alert.setProviderId(alertRule.getProviderId());
    alert.setSensorId(sensor.getSensorId());
    alert.setComponentId(sensor.getComponentId());
    alert.setType(Type.INTERNAL);
    alert.setTrigger(alertRule.getTrigger());
    alert.setExpression(alertRule.getExpression());

    // Finally, alert state depends on sensor state: if sensor is online then alert is active.
    // Otherwise, alert is disabled.
    alert.setActive(SensorState.online == sensor.getState());

    return alert;
  }

  protected List<Sensor> findSensors(final String... filterParams) {
    final SearchFilter filter = buildFilter(filterParams);
    final SearchFilterResult<Sensor> result = sensorService.search(filter);

    return result.getContent();
  }

  protected List<Component> findComponents(final String... filterParams) {
    final SearchFilter filter = buildFilter(filterParams);
    final SearchFilterResult<Component> result = componentService.search(filter);

    return result.getContent();
  }

  private static SearchFilter buildFilter(final String... filterParams) {
    final SearchFilter searchFilter = new SearchFilter(false);

    // filterParams is an array where each two consecutive elements represents a pair (key, value)
    for (int i = 0; i < filterParams.length - 1; i = i + 2) {
      if (StringUtils.hasText(filterParams[i + 1])) {
        searchFilter.addAndParam(filterParams[i], filterParams[i + 1]);
      }
    }

    return searchFilter;
  }

}
