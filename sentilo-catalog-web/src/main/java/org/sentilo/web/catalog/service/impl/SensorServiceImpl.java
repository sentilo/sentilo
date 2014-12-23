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
package org.sentilo.web.catalog.service.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.List;

import org.sentilo.common.domain.OrderMessage;
import org.sentilo.common.domain.QueryFilterParams;
import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.platform.client.core.PlatformTemplate;
import org.sentilo.platform.client.core.domain.AlarmInputMessage;
import org.sentilo.platform.client.core.domain.AlarmMessage;
import org.sentilo.platform.client.core.domain.AlarmsOutputMessage;
import org.sentilo.platform.client.core.domain.DataInputMessage;
import org.sentilo.platform.client.core.domain.Observation;
import org.sentilo.platform.client.core.domain.ObservationsOutputMessage;
import org.sentilo.platform.client.core.domain.OrderInputMessage;
import org.sentilo.platform.client.core.domain.OrdersOutputMessage;
import org.sentilo.web.catalog.domain.Alert;
import org.sentilo.web.catalog.domain.Sensor;
import org.sentilo.web.catalog.event.DeletePlatformResourcesEvent;
import org.sentilo.web.catalog.exception.builder.CompoundDuplicateKeyExceptionBuilder;
import org.sentilo.web.catalog.repository.SensorRepository;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.service.AlertService;
import org.sentilo.web.catalog.service.SensorService;
import org.sentilo.web.catalog.validator.SensorEntityKeyValidatorImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.data.mongodb.core.query.Update;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

@Service
public class SensorServiceImpl extends AbstractBaseServiceImpl<Sensor> implements SensorService {

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
   * @see org.sentilo.web.catalog.service.impl.AbstractBaseServiceImpl#doAfterInit()
   */
  protected void doAfterInit() {
    setEntityKeyValidator(new SensorEntityKeyValidatorImpl(this, new CompoundDuplicateKeyExceptionBuilder("error.sensor.duplicate.key")));
    super.doAfterInit();
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.sentilo.web.catalog.service.impl.AbstractBaseServiceImpl#getRepository()
   */
  public SensorRepository getRepository() {
    return repository;
  }

  /*
   * (non-Javadoc)
   * 
   * @see
   * org.sentilo.web.catalog.service.impl.AbstractBaseServiceImpl#getEntityId(org.sentilo.web.catalog
   * .domain.CatalogDocument)
   */
  public String getEntityId(final Sensor entity) {
    return entity.getId();
  }

  /*
   * (non-Javadoc)
   * 
   * @see
   * org.sentilo.web.catalog.service.SensorService#getLastObservations(org.sentilo.web.catalog.domain
   * .Sensor)
   */
  public List<Observation> getLastObservations(final Sensor sensor) {
    final QueryFilterParams filterParams = new QueryFilterParams(SentiloConstants.NUM_MAXIM_ELEMENTS);
    final DataInputMessage message = new DataInputMessage(sensor.getProviderId(), sensor.getSensorId(), filterParams);
    final ObservationsOutputMessage outMessage = platformTemplate.getDataOps().getLastObservations(message);
    return outMessage.getObservations();
  }

  /*
   * (non-Javadoc)
   * 
   * @see
   * org.sentilo.web.catalog.service.SensorService#getLastObservation(org.sentilo.web.catalog.domain
   * .Sensor)
   */
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
  public Observation getLastObservation(final Sensor sensor, final QueryFilterParams filterParams) {
    final DataInputMessage message = new DataInputMessage(sensor.getProviderId(), sensor.getSensorId(), filterParams);
    final ObservationsOutputMessage outMessage = platformTemplate.getDataOps().getLastObservations(message);
    return (CollectionUtils.isEmpty(outMessage.getObservations()) ? null : outMessage.getObservations().get(0));
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.sentilo.web.catalog.service.SensorService#updateMulti(java.util.Collection,
   * java.lang.String, java.lang.Object)
   */
  public void updateMulti(final Collection<String> sensorIds, final String param, final Object value) {
    final Update update = Update.update(param, value);
    getMongoOps().updateMulti(buildQueryForIdInCollection(sensorIds), update, Sensor.class);
  }

  /*
   * (non-Javadoc)
   * 
   * @see
   * org.sentilo.web.catalog.service.SensorService#getLastAlarmsMessages(org.sentilo.web.catalog
   * .domain.Sensor)
   */
  public List<AlarmMessage> getLastAlarmsMessages(final Sensor sensor) {
    // To retrieve the latest alarms associated with the sensor alerts, we must first retrieve the
    // sensor alerts list and then, for each alert, recover their last alarm.
    final List<AlarmMessage> lastAlarmMessages = new ArrayList<AlarmMessage>();

    final List<Alert> sensorAlerts = getSensorAlerts(sensor);

    for (final Alert alert : sensorAlerts) {
      final QueryFilterParams filterParams = new QueryFilterParams(SentiloConstants.NUM_MAXIM_ELEMENTS);
      final AlarmInputMessage message = new AlarmInputMessage(alert.getId(), filterParams);
      final AlarmsOutputMessage outMessage = platformTemplate.getAlarmOps().getLastAlarmMessages(message);
      if (!CollectionUtils.isEmpty(outMessage.getAlarms())) {
        lastAlarmMessages.addAll(outMessage.getAlarms());
      }
    }

    return lastAlarmMessages;
  }

  /*
   * (non-Javadoc)
   * 
   * @see
   * org.sentilo.web.catalog.service.SensorService#getLastOrderMessages(org.sentilo.web.catalog.
   * domain.Sensor)
   */
  public List<OrderMessage> getLastOrderMessages(final Sensor sensor) {
    final QueryFilterParams filterParams = new QueryFilterParams(SentiloConstants.NUM_MAXIM_ELEMENTS);
    final OrderInputMessage message = new OrderInputMessage(sensor.getProviderId(), sensor.getSensorId(), filterParams);
    final OrdersOutputMessage outMessage = platformTemplate.getOrderOps().getLastOrders(message);
    return outMessage.getOrders();
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.sentilo.web.catalog.service.SensorService#deleteSensors(java.lang.String[])
   */
  public void deleteSensors(final String[] sensorsNames) {
    // If sensors are deleted, then also must be deleted the associated alerts
    final List<String> values = Arrays.asList(sensorsNames);
    final Query sensorIdFilter = buildQueryForParamInCollection("sensorId", values);
    deleteSensorsAndAlerts(sensorIdFilter);
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.sentilo.web.catalog.service.SensorService#deleteSensorsFromComponents(java.util.List)
   */
  public void deleteSensorsFromComponents(final List<String> componentsIds) {
    // If sensors are deleted, then also must be deleted the associated alerts
    final Query componentIdFilter = buildQueryForParamInCollection("componentId", componentsIds);
    deleteSensorsAndAlerts(componentIdFilter);
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.sentilo.web.catalog.service.SensorService#findByName(java.lang.String,
   * java.lang.String)
   */
  public Sensor findByName(final String providerId, final String sensorId) {
    final SearchFilter filter = new SearchFilter();
    filter.addAndParam("providerId", providerId);
    filter.addAndParam("sensorId", sensorId);

    return getMongoOps().findOne(buildQuery(filter), Sensor.class);
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.sentilo.web.catalog.service.impl.AbstractBaseServiceImpl#delete(java.util.Collection)
   */
  public void delete(final Collection<Sensor> sensors) {
    final Collection<String> ids = new ArrayList<String>();
    for (final Sensor sensor : sensors) {
      ids.add(sensor.getId());
    }
    notifySensorsToDelete(buildQueryForIdInCollection(ids));

    super.delete(sensors);
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.sentilo.web.catalog.service.SensorService#changeAccessType(java.lang.String[],
   * boolean)
   */
  public void changeAccessType(final String[] sensorsIds, final boolean isPublicAccess) {
    final List<String> values = Arrays.asList(sensorsIds);
    final Query query = buildQueryForIdInCollection(values);
    final Update update = Update.update("publicAccess", isPublicAccess).set("updateAt", new Date());
    getMongoOps().updateMulti(query, update, Sensor.class);

  }

  private void deleteSensorsAndAlerts(final Query query) {
    // First, notify what sensors will be deleted
    notifySensorsToDelete(query);
    // Second, delete Sensor and Alerts that satisfies the query filter
    getMongoOps().remove(query, Alert.class);
    getMongoOps().remove(query, Sensor.class);
  }

  private void notifySensorsToDelete(final Query query) {
    // Create new DeletePlatformResourceEvent to notify what sensors will be deleted
    // Define a projection over the query to limit fields to retrieve
    query.fields().include("providerId").include("sensorId");
    final List<Sensor> sensors = getMongoOps().find(query, Sensor.class);
    getContext().publishEvent(new DeletePlatformResourcesEvent<Sensor>(this, sensors, Sensor.class));
  }

  private List<Alert> getSensorAlerts(final Sensor sensor) {
    final SearchFilter sensorAlertsFilter = new SearchFilter();
    sensorAlertsFilter.addAndParam("sensorId", sensor.getSensorId());
    return alertService.search(sensorAlertsFilter).getContent();
  }
}
