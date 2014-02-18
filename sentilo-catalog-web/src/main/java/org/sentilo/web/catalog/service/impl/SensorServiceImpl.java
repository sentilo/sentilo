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
  @Override
  protected void doAfterInit() {
    entityKeyValidator = new SensorEntityKeyValidatorImpl(this, new CompoundDuplicateKeyExceptionBuilder("error.sensor.duplicate.key"));
    super.doAfterInit();
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.sentilo.web.catalog.service.impl.AbstractBaseServiceImpl#getRepository()
   */
  @Override
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
  @Override
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
  @Override
  public List<Observation> getLastObservations(final Sensor sensor) {
    final DataInputMessage message = new DataInputMessage(sensor.getProviderId(), sensor.getSensorId(), new QueryFilterParams(SentiloConstants.NUM_MAXIM_ELEMENTS));
    final ObservationsOutputMessage outMessage = platformTemplate.getDataOps().getLastObservations(message);
    return outMessage.getObservations();
    // TODO Mikel: falta transformar la excepcion RESTClientException en una propia del catalogo.
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
    final DataInputMessage message = new DataInputMessage(sensor.getProviderId(), sensor.getSensorId());
    final ObservationsOutputMessage outMessage = platformTemplate.getDataOps().getLastObservations(message);
    return (CollectionUtils.isEmpty(outMessage.getObservations()) ? null : outMessage.getObservations().get(0));
    // TODO Mikel: falta transformar la excepcion RESTClientException en una propia del catalogo.
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.sentilo.web.catalog.service.SensorService#updateMulti(java.util.Collection,
   * java.lang.String, java.lang.Object)
   */
  @Override
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
  @Override
  public List<AlarmMessage> getLastAlarmsMessages(final Sensor sensor) {
    // Para recuperar las últimos alarmas asociadas a las alertas de un sensor, primero debemos
    // recuperar las alertas del sensor. Y despues, para cada alerta, recuperar sus ultimas alarmas.
    final List<AlarmMessage> lastAlarmMessages = new ArrayList<AlarmMessage>();

    final List<Alert> sensorAlerts = getSensorAlerts(sensor);

    for (final Alert alert : sensorAlerts) {
      final AlarmInputMessage message = new AlarmInputMessage(alert.getId(), new QueryFilterParams(SentiloConstants.NUM_MAXIM_ELEMENTS));
      final AlarmsOutputMessage outMessage = platformTemplate.getAlarmOps().getLastAlarmMessages(message);
      if (!CollectionUtils.isEmpty(outMessage.getAlarms())) {
        lastAlarmMessages.addAll(outMessage.getAlarms());
      }
    }

    // TODO Mikel: falta transformar la excepcion RESTClientException en una propia del catalogo.

    return lastAlarmMessages;
  }

  /*
   * (non-Javadoc)
   * 
   * @see
   * org.sentilo.web.catalog.service.SensorService#getLastOrderMessages(org.sentilo.web.catalog.
   * domain.Sensor)
   */
  @Override
  public List<OrderMessage> getLastOrderMessages(final Sensor sensor) {
    final OrderInputMessage message = new OrderInputMessage(sensor.getProviderId(), sensor.getSensorId(), new QueryFilterParams(SentiloConstants.NUM_MAXIM_ELEMENTS));
    final OrdersOutputMessage outMessage = platformTemplate.getOrderOps().getLastOrders(message);
    return outMessage.getOrders();
    // TODO Mikel: falta transformar la excepcion RESTClientException en una propia del catalogo.
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.sentilo.web.catalog.service.SensorService#deleteSensors(java.lang.String[])
   */
  @Override
  public void deleteSensors(final String[] sensorsNames) {
    // Al borrar sensores, tambien hay que eliminar las alertas que tienen asociadas.
    final List<String> values = Arrays.asList(sensorsNames);
    final Query sensorIdFilter = buildQueryForParamInCollection("sensorId", values);
    getMongoOps().remove(sensorIdFilter, Alert.class);
    getMongoOps().remove(sensorIdFilter, Sensor.class);
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.sentilo.web.catalog.service.SensorService#deleteSensorsFromComponents(java.util.List)
   */
  @Override
  public void deleteSensorsFromComponents(final List<String> componentsIds) {
    // Al borrar sensores, tambien hay que eliminar las alertas que tienen asociadas.
    final Query componentIdFilter = buildQueryForParamInCollection("componentId", componentsIds);
    getMongoOps().remove(componentIdFilter, Alert.class);
    getMongoOps().remove(componentIdFilter, Sensor.class);
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

  private List<Alert> getSensorAlerts(final Sensor sensor) {
    final SearchFilter sensorAlertsFilter = new SearchFilter();
    sensorAlertsFilter.addAndParam("sensorId", sensor.getSensorId());
    return alertService.search(sensorAlertsFilter).getContent();
  }

}
