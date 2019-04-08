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
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.List;

import org.sentilo.common.domain.OrderMessage;
import org.sentilo.common.domain.QueryFilterParams;
import org.sentilo.common.enums.SensorState;
import org.sentilo.platform.client.core.PlatformTemplate;
import org.sentilo.platform.client.core.domain.AlarmInputMessage;
import org.sentilo.platform.client.core.domain.AlarmMessage;
import org.sentilo.platform.client.core.domain.AlarmsOutputMessage;
import org.sentilo.platform.client.core.domain.DataInputMessage;
import org.sentilo.platform.client.core.domain.Observation;
import org.sentilo.platform.client.core.domain.ObservationsOutputMessage;
import org.sentilo.platform.client.core.domain.OrderInputMessage;
import org.sentilo.platform.client.core.domain.OrdersOutputMessage;
import org.sentilo.web.catalog.context.UserConfigContextHolder;
import org.sentilo.web.catalog.domain.Alert;
import org.sentilo.web.catalog.domain.Sensor;
import org.sentilo.web.catalog.domain.SortedEventsList;
import org.sentilo.web.catalog.event.SensorsStateChangeEvent;
import org.sentilo.web.catalog.exception.builder.CompoundDuplicateKeyExceptionBuilder;
import org.sentilo.web.catalog.repository.SensorRepository;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.service.AlertService;
import org.sentilo.web.catalog.service.SensorService;
import org.sentilo.web.catalog.validator.SensorKeyValidatorImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

@Service
public class SensorServiceImpl extends AbstractBaseCrudServiceImpl<Sensor> implements SensorService {

  @Autowired
  private SensorRepository repository;

  @Autowired
  private PlatformTemplate platformTemplate;

  @Autowired
  private AlertService alertService;

  public SensorServiceImpl() {
    super(Sensor.class);
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.service.impl.AbstractBaseCrudServiceImpl#doAfterInit()
   */
  @Override
  protected void doAfterInit() {
    setResourceKeyValidator(new SensorKeyValidatorImpl(this, new CompoundDuplicateKeyExceptionBuilder("error.sensor.duplicate.key")));
    super.doAfterInit();
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.service.impl.AbstractBaseCrudServiceImpl#getRepository()
   */
  @Override
  public SensorRepository getRepository() {
    return repository;
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.web.catalog.service.impl.AbstractBaseCrudServiceImpl#getEntityId(org.sentilo.web
   * .catalog .domain.CatalogDocument)
   */
  @Override
  public String getEntityId(final Sensor entity) {
    return entity.getId();
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.service.SensorService#getLastObservations(org.sentilo.web.catalog.
   * domain.Sensor, java.util.Date, java.util.Date)
   */
  @Override
  public SortedEventsList<Observation> getLastObservations(final Sensor sensor, final Date from, final Date to) {

    final int limit = calculateSensorEventsLimit(sensor);
    final QueryFilterParams filterParams = new QueryFilterParams(from, to, limit);
    return getFilteredLastObservations(sensor, filterParams);
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.web.catalog.service.SensorService#getLastObservation(org.sentilo.web.catalog.domain
   * .Sensor)
   */
  @Override
  public Observation getLastObservation(final Sensor sensor) {
    final QueryFilterParams filterParams = new QueryFilterParams(1);
    return getLastObservation(sensor, filterParams);
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.web.catalog.service.SensorService#getLastObservation(org.sentilo.web.catalog.domain
   * .Sensor, org.sentilo.common.domain.QueryFilterParams)
   */
  @Override
  public Observation getLastObservation(final Sensor sensor, final QueryFilterParams filterParams) {
    final DataInputMessage message = new DataInputMessage(sensor.getProviderId(), sensor.getSensorId(), filterParams);
    final ObservationsOutputMessage outMessage = platformTemplate.getDataOps().getLastObservations(message);
    return CollectionUtils.isEmpty(outMessage.getObservations()) ? null : outMessage.getObservations().get(0);
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.web.catalog.service.SensorService#getLastAlarmsMessages(org.sentilo.web.catalog.
   * domain.Sensor)
   */
  @Override
  public SortedEventsList<AlarmMessage> getLastAlarmsMessages(final Sensor sensor) {
    // To retrieve the latest alarms associated with the sensor alerts, we must first retrieve the
    // sensor alerts list and then, for each alert, recover their last alarm.
    final List<AlarmMessage> lastAlarmMessages = new ArrayList<AlarmMessage>();

    final List<Alert> sensorAlerts = getSensorAlerts(sensor);
    final int limit = calculateSensorEventsLimit(sensor);

    for (final Alert alert : sensorAlerts) {
      final QueryFilterParams filterParams = new QueryFilterParams(limit);
      final AlarmInputMessage message = new AlarmInputMessage(alert.getId(), filterParams);
      final AlarmsOutputMessage outMessage = platformTemplate.getAlarmOps().getLastAlarmMessages(message);
      if (!CollectionUtils.isEmpty(outMessage.getAlarms())) {
        lastAlarmMessages.addAll(outMessage.getAlarms());
      }
    }

    return new SortedEventsList<AlarmMessage>(lastAlarmMessages);
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.web.catalog.service.SensorService#getLastOrderMessages(org.sentilo.web.catalog.
   * domain.Sensor)
   */
  @Override
  public SortedEventsList<OrderMessage> getLastOrderMessages(final Sensor sensor) {
    final int limit = calculateSensorEventsLimit(sensor);
    final QueryFilterParams filterParams = new QueryFilterParams(limit);
    final OrderInputMessage message = new OrderInputMessage(sensor.getProviderId(), sensor.getSensorId(), filterParams);
    final OrdersOutputMessage outMessage = platformTemplate.getOrderOps().getLastOrders(message);
    return new SortedEventsList<OrderMessage>(outMessage.getOrders());
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.service.SensorService#deleteSensors(java.lang.String,
   * java.lang.String[])
   */
  @Override
  public void deleteSensors(final String provider, final String[] sensorsNames) {
    // If sensors are deleted, then also must be deleted the associated alerts
    final SearchFilter searchFilter = new SearchFilter();
    searchFilter.addAndParam("providerId", provider);
    searchFilter.addAndParam("sensorId", sensorsNames);

    deleteSensorsAndChildren(buildQuery(searchFilter));
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.service.SensorService#deleteSensorsFromComponents(java.util.List)
   */
  @Override
  public void deleteSensorsFromComponents(final List<String> componentsIds) {
    // If sensors are deleted, then also must be deleted the associated alerts
    final Query componentIdFilter = buildQueryForParamInCollection("componentId", componentsIds);
    deleteSensorsAndChildren(componentIdFilter);
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.service.SensorService#findByName(java.lang.String,
   * java.lang.String)
   */
  @Override
  public Sensor findByName(final String providerId, final String sensorId) {
    final SearchFilter filter = new SearchFilter();
    filter.addAndParam("providerId", providerId);
    filter.addAndParam("sensorId", sensorId);

    return getMongoOps().findOne(buildQuery(filter), Sensor.class);
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.service.SensorService#changeAccessType(java.lang.String[],
   * boolean)
   */
  @Override
  public void changeAccessType(final String[] sensorsIds, final Boolean isPublicAccess) {
    updateMulti(Arrays.asList(sensorsIds), "publicAccess", isPublicAccess);
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.service.SensorService#changeState(java.lang.String[],
   * org.sentilo.web.catalog.enums.SensorState, java.lang.String)
   */
  @Override
  public void changeState(final String[] sensorsIds, final SensorState newState, final String newSubstate) {
    updateMulti(Arrays.asList(sensorsIds), Arrays.asList("state", "substate"), Arrays.asList(newState.name(), newSubstate));
    notifyStatesChange(sensorsIds, newState);
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.web.catalog.service.SensorService#notifyStateChange(org.sentilo.web.catalog.domain.
   * Sensor)
   */
  @Override
  public void notifyStateChange(final Sensor sensor) {
    final String[] sensorsIds = {sensor.getId()};
    notifyStatesChange(sensorsIds, sensor.getState());
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.web.catalog.service.impl.AbstractBaseCrudServiceImpl#doAfterDelete(org.sentilo.web.
   * catalog.domain.CatalogDocument)
   */
  @Override
  protected void doAfterDelete(final Sensor sensor) {
    doAfterDelete(Arrays.asList(new Sensor[] {sensor}));
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.service.impl.AbstractBaseCrudServiceImpl#doAfterDelete(java.util.
   * Collection)
   */
  @Override
  protected void doAfterDelete(final Collection<Sensor> entities) {
    // After delete the sensors, a second step should be done: remove its alerts
    for (final Sensor sensor : entities) {
      final SearchFilter filter = new SearchFilter();
      filter.addAndParam("providerId", sensor.getProviderId());
      filter.addAndParam("sensorId", sensor.getSensorId());
      deleteChildren(buildQuery(filter));
    }
  }

  private void deleteSensorsAndChildren(final Query query) {
    doDelete(query);
    deleteChildren(query);
  }

  private void deleteChildren(final Query query) {
    doDelete(query, Alert.class);
  }

  private List<Alert> getSensorAlerts(final Sensor sensor) {
    final SearchFilter sensorAlertsFilter = new SearchFilter();
    sensorAlertsFilter.addAndParam("sensorId", sensor.getSensorId());
    sensorAlertsFilter.addAndParam("providerId", sensor.getProviderId());
    sensorAlertsFilter.addAndParam("active", Boolean.TRUE);

    return alertService.search(sensorAlertsFilter).getContent();
  }

  private void notifyStatesChange(final String[] sensorsIds, final SensorState newState) {
    // Create new SensorsStateChangeEvent to notify state change
    final List<Sensor> sensors = new ArrayList<Sensor>();
    for (final String sensorId : sensorsIds) {
      final Sensor sensor = new Sensor(sensorId);
      sensor.setState(newState);
      sensors.add(sensor);
    }
    if (!CollectionUtils.isEmpty(sensors)) {
      getContext().publishEvent(new SensorsStateChangeEvent(this, sensors));
    }
  }

  private SortedEventsList<Observation> getFilteredLastObservations(final Sensor sensor, final QueryFilterParams filterParams) {
    final DataInputMessage message = new DataInputMessage(sensor.getProviderId(), sensor.getSensorId(), filterParams);
    final ObservationsOutputMessage outMessage = platformTemplate.getDataOps().getLastObservations(message);
    return new SortedEventsList<Observation>(outMessage.getObservations());
  }

  private int calculateSensorEventsLimit(final Sensor sensor) {
    int sensorLimit = 0;
    if (sensor.getVisualConfiguration() != null && sensor.getVisualConfiguration().getChartVisiblePointsNumber() != null) {
      sensorLimit = sensor.getVisualConfiguration().getChartVisiblePointsNumber();
    }
    return sensorLimit > 0 ? sensorLimit : UserConfigContextHolder.getContext().getChartVisiblePointsNumber();
  }

}
