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

import org.sentilo.common.enums.EventType;
import org.sentilo.common.enums.SensorState;
import org.sentilo.platform.common.domain.DataInputMessage;
import org.sentilo.platform.common.domain.Observation;
import org.sentilo.platform.common.domain.Sensor;
import org.sentilo.platform.common.exception.EventRejectedException;
import org.sentilo.platform.common.exception.RejectedResourcesContext;
import org.sentilo.platform.common.exception.ResourceNotFoundException;
import org.sentilo.platform.common.exception.ResourceOfflineException;
import org.sentilo.platform.common.service.DataService;
import org.sentilo.platform.common.service.InternalAlarmService;
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

import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Iterables;

@Service
public class DataServiceImpl extends AbstractPlatformServiceImpl implements DataService {

  private static final Logger LOGGER = LoggerFactory.getLogger(DataServiceImpl.class);

  @Autowired
  private ResourceService resourceService;

  @Autowired
  private InternalAlarmService internalAlarmService;

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
        final Sensor sensor = getSensorMetadata(observation.getProvider(), observation.getSensor());
        checkTargetResourceState(sensor, observation);
        setObservation(sensor, observation);
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
    final Long sid = sequenceUtils.getSid(providerId, sensorId);
    if (sid == null) {
      // Si no hay identificador interno del sensor, entonces este no tiene ninguna observacion
      // registrada.
      return;
    }

    deleteLastObservation(sid);

    LOGGER.debug("Removed last observation from sensor [{}] belonging to provider [{}]", sensorId, providerId);
  }

  private void setObservation(final Sensor sensor, final Observation data) {
    registerSensorData(sensor, data);
    publishSensorData(data);
  }

  private List<Observation> getLastObservations(final String sid, final DataInputMessage message) {
    final Long to = QueryFilterParamsUtils.getTo(message);
    final Long from = QueryFilterParamsUtils.getFrom(message);
    final Integer limit = QueryFilterParamsUtils.getLimit(message);

    // Because sensor events may each have a different expire time, and is likely to have some
    // entries
    // already expired (i.e. the associated sdid:{sdid} entry is expired),
    // the process read from the ZSET more than limit entries (limit + 1) : it improves the
    // performance
    // to return the limit observations requested by the
    // client because additional reads are made only if exists these additional entries.

    final List<Observation> observations = new ArrayList<Observation>();
    boolean readMore = true;
    // To evict a situation of no return if database is inconsistent (f.e. many of the entries in
    // ZSETs
    // are related with data already expired), we limit the maximum number
    // of iterations
    final int MAX_ITERATIONS = 10;
    int iteration = 1;

    while (readMore) {
      final int offset = (iteration - 1) * limit;
      final int count = limit + 1;
      // Redis call is: ZREVRANGEBYSCORE sid:{sid}:observations to from LIMIT 0 limit
      final Set<String> sdids = sRedisTemplate.zRevRangeByScore(keysBuilder.getSensorObservationsKey(sid), to, from, offset, count);

      // As count=limit+1 and client only request limit elements, sdids set is subset to contain a
      // maximum
      // of limit elements
      if (!CollectionUtils.isEmpty(sdids)) {
        final Set<String> sdidsToEval = sdids.size() < count ? sdids : ImmutableSet.copyOf(Iterables.limit(sdids, limit));
        addObservations(sdidsToEval, observations, limit);
      }

      readMore = observations.size() < limit && !CollectionUtils.isEmpty(sdids) && sdids.size() > limit && iteration < MAX_ITERATIONS;
      iteration++;
    }

    return observations;
  }

  private void addObservations(final Set<String> sdids, final List<Observation> observations, final Integer limit) {
    final Iterator<String> it = sdids.iterator();

    while (it.hasNext() && observations.size() < limit) {
      final Long sdid = Long.parseLong(it.next());
      final Observation observation = getObservation(sdid);
      if (observation != null) {
        // Añadir llamada a un ThreadMonitor que se encargue de eliminar las entradas invalidas de
        // los ZSETs
        // en background. Este Thread tendra una
        // cola en la cual irán añadiendo elementos a borrar (key y field) u periodicamente la ira
        // borrando.
        // Esto evitará tener que ir repitiendo
        // la lectura de elementos inconsistentes
        observations.add(observation);
      }
    }
  }

  private Observation getObservation(final Long sdid) {
    Observation observation = null;
    String sid = null;
    String value = null;
    String ts = null;
    String location = null;

    final Map<String, String> infoSdid = sRedisTemplate.hGetAll(keysBuilder.getObservationKey(sdid));
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
    final Set<String> sdids = sRedisTemplate.zRange(sensorObservationsKey, -1, -1);
    if (!CollectionUtils.isEmpty(sdids)) {
      sRedisTemplate.zRemRangeByRank(sensorObservationsKey, -1, -1);
      final String sdid = sdids.iterator().next();
      sRedisTemplate.del(keysBuilder.getObservationKey(sdid));
    }
  }

  /**
   * Checks if the sensor exists in Redis and if it is enabled. Otherwise throws an exception.
   */
  private void checkTargetResourceState(final Sensor sensor, final Observation data) throws ResourceNotFoundException, ResourceOfflineException {
    final boolean existsSensor = sensor.getState() != null && !SensorState.ghost.equals(sensor.getState());

    if (!existsSensor && rejectUnknownSensors) {
      throw new ResourceNotFoundException(data.getSensor(), "Sensor");
    } else if (!existsSensor) {
      internalAlarmService.publishGhostSensorAlarm(data);
    } else if (SensorState.offline.equals(sensor.getState())) {
      throw new ResourceOfflineException(data.getSensor(), "Sensor");
    }
  }

  private void registerSensorData(final Sensor sensor, final Observation data) {
    // final Long sid = sequenceUtils.getSid(data.getProvider(), data.getSensor());
    final Long sid = sensor.getSid();
    final Long sdid = sequenceUtils.getSdid();
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
    sRedisTemplate.hmSet(obsKey, fields);

    // if expired time in seconds (ttl) is defined and !=0, set the expire time to the key
    final int ttl = ttlToExpiredTime(sensor.getTtl());
    if (ttl != 0) {
      sRedisTemplate.expire(obsKey, ttl);
    }

    // Y definimos una reverse lookup key con la cual recuperar rapidamente las observaciones de un
    // sensor.
    // A continuacion, añadimos el sdid al Sorted Set sensor:{sid}:observations. La puntuacion, o
    // score, que se asocia a cada elemento del Set es el timestamp de la observacion.
    sRedisTemplate.zAdd(keysBuilder.getSensorObservationsKey(sid), timestamp, sdid.toString());

    LOGGER.debug("Registered in Redis observation [{}] for sensor [{}] belonging to provider [{}]", sdid, data.getSensor(), data.getProvider());
  }

  private void publishSensorData(final Observation data) {
    final Topic topic = ChannelUtils.buildTopic(PubSubChannelPrefix.data, data.getProvider(), data.getSensor());
    sRedisTemplate.publish(topic.getTopic(), PublishMessageUtils.buildContentToPublish(data, topic));
  }

  private Sensor getSensorMetadata(final String provider, final String sensorId) {
    final Sensor sensor = resourceService.getSensor(provider, sensorId);

    if (!rejectUnknownSensors && !sensor.exists()) {
      // Save both the provider and the sensor in Redis only if rejectUnknownSensors is false. By
      // default, in this context, if sensor doesn't exist, it is created as a ghost sensor
      resourceService.registerProviderIfNeedBe(sensor.getProvider());
      resourceService.registerGhostSensorIfNeedBe(sensor);
    }

    return sensor;
  }

}
