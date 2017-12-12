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
package org.sentilo.platform.service.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.sentilo.common.cache.LRUCache;
import org.sentilo.common.cache.impl.LRUCacheImpl;
import org.sentilo.common.enums.EventType;
import org.sentilo.common.enums.SensorState;
import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.platform.common.domain.AlarmInputMessage;
import org.sentilo.platform.common.domain.DataInputMessage;
import org.sentilo.platform.common.domain.Observation;
import org.sentilo.platform.common.domain.Sensor;
import org.sentilo.platform.common.exception.EventRejectedException;
import org.sentilo.platform.common.exception.RejectedResourcesContext;
import org.sentilo.platform.common.exception.ResourceNotFoundException;
import org.sentilo.platform.common.exception.ResourceOfflineException;
import org.sentilo.platform.common.service.DataService;
import org.sentilo.platform.common.service.ResourceService;
import org.sentilo.platform.service.monitor.Metric;
import org.sentilo.platform.service.monitor.RequestType;
import org.sentilo.platform.service.utils.ChannelUtils;
import org.sentilo.platform.service.utils.ChannelUtils.PubSubChannelPrefix;
import org.sentilo.platform.service.utils.PublishMessageUtils;
import org.sentilo.platform.service.utils.QueryFilterParamsUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.redis.listener.Topic;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

@Service
public class DataServiceImpl extends AbstractPlatformServiceImpl implements DataService {

  private static final Logger LOGGER = LoggerFactory.getLogger(DataServiceImpl.class);

  @Autowired
  private ResourceService resourceService;

  /** Internal cache to evict spam with ghost alarms notifications. */
  private final LRUCache<String, String> ghostSensors = new LRUCacheImpl<String, String>(1000, 10);

  @Value("${api.data.reject-unknown-sensors:true}")
  private boolean rejectUnknownSensors = true;

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.platform.common.service.DataService#setObservations(org.sentilo.platform.common
   * .domain.DataInputMessage)
   */
  @Metric(requestType = RequestType.PUT, eventType = EventType.DATA)
  public void setObservations(final DataInputMessage message) {
    final List<Observation> observations = message.getObservations();
    final RejectedResourcesContext rejectedContext = new RejectedResourcesContext();

    for (final Observation observation : observations) {
      try {
        checkTargetResourceState(observation);
        setObservation(observation);
      } catch (final ResourceNotFoundException rnfe) {
        rejectedContext.rejectEvent(observation.getSensor(), rnfe.getMessage());
        LOGGER.warn("Observation [{}] has been rejected because sensor [{}], belonging to provider [{}], doesn't exist on Sentilo.",
            observation.getValue(), observation.getSensor(), observation.getProvider());
      } catch (final ResourceOfflineException roe) {
        rejectedContext.rejectEvent(observation.getSensor(), roe.getMessage());
        LOGGER.warn("Observation [{}] has been rejected because sensor [{}], belonging to provider [{}], is not online.", observation.getValue(),
            observation.getSensor(), observation.getProvider());
      }
    }

    if (!rejectedContext.isEmpty()) {
      throw new EventRejectedException(EventType.DATA, rejectedContext);
    }
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.platform.common.service.DataService#deleteLastObservations(org.sentilo.platform
   * .common.domain.DataInputMessage)
   */
  public void deleteLastObservations(final DataInputMessage message) {
    if (StringUtils.hasText(message.getSensorId())) {
      deleteLastObservation(message.getProviderId(), message.getSensorId());
    } else {
      deleteLastObservations(message.getProviderId());
    }
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.platform.common.service.DataService#getLastObservations(org.sentilo.platform.common
   * .domain.DataInputMessage)
   */
  @Metric(requestType = RequestType.GET, eventType = EventType.DATA)
  public List<Observation> getLastObservations(final DataInputMessage message) {
    // Para recuperar las observaciones del sensor / sensores de un proveedor, debemos hacer lo
    // siguiente:
    // 1. Recuperar los identificadores internos de los sensores de los cuales queremos recuperar
    // las observaciones.
    // 2. Para cada sensor, recuperar las observaciones que cumplen el criterio de busqueda
    final List<Observation> globalObservations = new ArrayList<Observation>();
    final Set<String> sids = resourceService.getSensorsToInspect(message.getProviderId(), message.getSensorId());
    if (CollectionUtils.isEmpty(sids)) {
      LOGGER.debug("Provider [{}] has not sensors registered", message.getProviderId());
      return globalObservations;
    }

    LOGGER.debug("Retrieving last observations for {} sensors belonging to provider [{}]", sids.size(), message.getProviderId());

    final Iterator<String> it = sids.iterator();
    while (it.hasNext()) {
      final String sid = it.next();
      final List<Observation> observationsFromSensor = getLastObservations(sid, message);
      if (!CollectionUtils.isEmpty(observationsFromSensor)) {
        globalObservations.addAll(observationsFromSensor);
      }
    }

    return globalObservations;
  }

  private void deleteLastObservations(final String providerId) {
    final Set<String> sids = resourceService.getSensorsFromProvider(providerId);

    if (CollectionUtils.isEmpty(sids)) {
      LOGGER.debug("Provider [{}] has not sensors registered", providerId);
      return;
    }

    LOGGER.debug("Found {} sensors belonging to provider [{}]", sids.size(), providerId);
    final Iterator<String> it = sids.iterator();
    while (it.hasNext()) {
      final String sid = it.next();
      deleteLastObservation(new Long(sid));
      LOGGER.debug("Removed last observation from sensor with sid {} and belonging to provider [{}]", sid, providerId);
    }
  }

  private void deleteLastObservation(final String providerId, final String sensorId) {
    final Long sid = jedisSequenceUtils.getSid(providerId, sensorId);
    if (sid == null) {
      // Si no hay identificador interno del sensor, entonces este no tiene ninguna observacion
      // registrada.
      return;
    }

    deleteLastObservation(sid);

    LOGGER.debug("Removed last observation from sensor [{}] belonging to provider [{}]", sensorId, providerId);
  }

  private void setObservation(final Observation data) throws ResourceNotFoundException {
    registerProviderAndSensorIfNeedBe(data);
    registerSensorData(data);
    publishSensorData(data);
  }

  private List<Observation> getLastObservations(final String sid, final DataInputMessage message) {
    final Long to = QueryFilterParamsUtils.getTo(message);
    final Long from = QueryFilterParamsUtils.getFrom(message);
    final Integer limit = QueryFilterParamsUtils.getLimit(message);

    // La sentencia a utilizar en Redis es:
    // ZREVRANGEBYSCORE sid:{sid}:observations to from LIMIT 0 limit

    final Set<String> sdids = jedisTemplate.zRevRangeByScore(keysBuilder.getSensorObservationsKey(sid), to, from, 0, limit);
    List<Observation> observations = null;

    if (!CollectionUtils.isEmpty(sdids)) {
      observations = getObservations(sdids);
    }

    return observations;
  }

  private List<Observation> getObservations(final Set<String> sdids) {
    final List<Observation> observations = new ArrayList<Observation>();
    final Iterator<String> it = sdids.iterator();

    while (it.hasNext()) {
      final Long sdid = Long.parseLong(it.next());
      final Observation observation = getObservation(sdid);
      if (observation != null) {
        observations.add(observation);
      }
    }
    return observations;
  }

  private Observation getObservation(final Long sdid) {
    Observation observation = null;
    String sid = null;
    String value = null;
    String ts = null;
    String location = null;

    final Map<String, String> infoSdid = jedisTemplate.hGetAll(keysBuilder.getObservationKey(sdid));
    if (!CollectionUtils.isEmpty(infoSdid)) {
      value = infoSdid.get(DATA);
      ts = infoSdid.get(TIMESTAMP);
      sid = infoSdid.get(SID);
      location = infoSdid.get(LOCATION);
    }

    if (StringUtils.hasText(sid)) {
      final Sensor sensor = resourceService.getSensor(Long.parseLong(sid));
      observation = new Observation(sensor.getProvider(), sensor.getSensor(), value, Long.parseLong(ts), location);
    }

    return observation;
  }

  private void deleteLastObservation(final Long sid) {
    // Para eliminar la ultima observacion de un sensor lo que debemos hacer es lo siguiente:
    // 1. Recuperamos el ultimo elemento del Sorted Set de observaciones del sensor (i.e., el que
    // tiene score mas alto).
    // 2. Eliminamos este elemento del Sorted Set.
    // 3. Eliminamos la clave sdid:{sdid}
    final String sensorObservationsKey = keysBuilder.getSensorObservationsKey(sid);
    final Set<String> sdids = jedisTemplate.zRange(sensorObservationsKey, -1, -1);
    if (!CollectionUtils.isEmpty(sdids)) {
      jedisTemplate.zRemRangeByRank(sensorObservationsKey, -1, -1);
      final String sdid = sdids.iterator().next();
      jedisTemplate.del(keysBuilder.getObservationKey(sdid));
    }
  }

  /**
   * Checks if the sensor exists in Redis and if it is enabled. Otherwise throws an exception.
   */
  private void checkTargetResourceState(final Observation data) throws ResourceNotFoundException, ResourceOfflineException {
    final SensorState sensorState = resourceService.getSensorState(data.getProvider(), data.getSensor());
    final boolean existsSensor = sensorState != null && !sensorState.equals(SensorState.ghost);

    if (!existsSensor && rejectUnknownSensors) {
      throw new ResourceNotFoundException(data.getSensor(), "Sensor");
    } else if (!existsSensor) {
      publishGhostSensorAlarm(data);
    } else if (SensorState.offline.equals(sensorState)) {
      throw new ResourceOfflineException(data.getSensor(), "Sensor");
    }
  }

  private void registerSensorData(final Observation data) {
    final Long sid = jedisSequenceUtils.getSid(data.getProvider(), data.getSensor());

    final Long sdid = jedisSequenceUtils.getSdid();
    final Long timestamp = data.getTimestamp();
    final String location = StringUtils.hasText(data.getLocation()) ? data.getLocation() : "";
    // Guardamos una hash de clave sdid:{sdid} y valores sid, data (aleatorio), timestamp y
    // location.
    final String obsKey = keysBuilder.getObservationKey(sdid);
    final Map<String, String> fields = new HashMap<String, String>();
    fields.put(SID, Long.toString(sid));
    fields.put(DATA, data.getValue());
    fields.put(TIMESTAMP, timestamp.toString());
    fields.put(LOCATION, location);
    jedisTemplate.hmSet(obsKey, fields);

    // if expireSeconds is defined and !=0, set the expire time to key
    if (expireSeconds != 0) {
      jedisTemplate.expire(obsKey, expireSeconds);
    }

    // Y definimos una reverse lookup key con la cual recuperar rapidamente las observaciones de un
    // sensor.
    // A continuacion, añadimos el sdid al Sorted Set sensor:{sid}:observations. La puntuacion, o
    // score, que se asocia a cada elemento del Set es el timestamp de la observacion.
    jedisTemplate.zAdd(keysBuilder.getSensorObservationsKey(sid), timestamp, sdid.toString());

    LOGGER.debug("Registered in Redis observation [{}] for sensor [{}] belonging to provider [{}]", sdid, data.getSensor(), data.getProvider());
  }

  private void publishSensorData(final Observation data) {
    final Topic topic = ChannelUtils.buildTopic(PubSubChannelPrefix.data, data.getProvider(), data.getSensor());
    jedisTemplate.publish(topic.getTopic(), PublishMessageUtils.buildContentToPublish(data, topic));
  }

  private void publishGhostSensorAlarm(final Observation data) {
    final String ghostSensorKey = data.getProvider() + "." + data.getSensor();
    if (ghostSensors.get(ghostSensorKey) == null) {
      final String ghost_message_template = "Detected ghost sensor %s belonging to provider %s";
      final Topic topic = ChannelUtils.buildTopic(PubSubChannelPrefix.alarm, data.getProvider(), data.getSensor());

      final AlarmInputMessage aim = new AlarmInputMessage();
      aim.setProviderId(data.getProvider());
      aim.setSensorId(data.getSensor());
      aim.setAlertType("INTERNAL");
      aim.setAlertId(SentiloConstants.GHOST_SENSOR_ALERT);
      aim.setSender(SentiloConstants.GHOST_SENSOR_SENDER);
      aim.setMessage(String.format(ghost_message_template, data.getSensor(), data.getProvider()));

      jedisTemplate.publish(topic.getTopic(), PublishMessageUtils.buildContentToPublish(aim, topic));
      ghostSensors.put(ghostSensorKey, ghost_message_template);
      LOGGER.info("Published new ghost sensor alarm related to sensor [{}] from provider [{}]", data.getSensor(), data.getProvider());
    }
  }

  private void registerProviderAndSensorIfNeedBe(final Observation data) {
    // Save both the provider and the sensor in Redis only if rejectUnknownSensors is false. By
    // default, sensor state is marked as ghost
    if (!rejectUnknownSensors) {
      resourceService.registerProviderIfNeedBe(data.getProvider());
      resourceService.registerSensorIfNeedBe(data.getProvider(), data.getSensor(), SensorState.ghost, false);
    }
  }

}
