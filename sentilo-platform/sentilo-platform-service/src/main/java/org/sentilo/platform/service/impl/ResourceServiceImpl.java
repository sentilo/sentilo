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

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import org.sentilo.common.domain.CatalogAlert;
import org.sentilo.common.domain.CatalogSensor;
import org.sentilo.common.enums.EventType;
import org.sentilo.common.enums.SensorState;
import org.sentilo.platform.common.domain.Alert;
import org.sentilo.platform.common.domain.Sensor;
import org.sentilo.platform.common.exception.ResourceNotFoundException;
import org.sentilo.platform.common.exception.ResourceOfflineException;
import org.sentilo.platform.common.service.ResourceService;
import org.sentilo.platform.service.clean.RedisDataCleaner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.util.Pair;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

@Service
public class ResourceServiceImpl extends AbstractPlatformServiceImpl implements ResourceService {

  private static final Logger LOGGER = LoggerFactory.getLogger(ResourceServiceImpl.class);
  private static String NEW_PROVIDER_LOCK_NAME = "register_new_provider";
  private static String NEW_SENSOR_LOCK_NAME = "register_new_sensor";
  private static String NEW_ALERT_LOCK_NAME = "register_new_alert";

  @Autowired
  private RedisDataCleaner cleaner;

  @Override
  public void addOrphanEventsToRemove(final EventType type, final String zset, final List<String> eventsIds) {
    cleaner.addOrphanEventsToRemove(type, zset, eventsIds);
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.platform.common.service.ResourceService#registerProviderIfNecessary(java.lang.
   * String )
   */
  @Override
  public Long registerProviderIfNeedBe(final String providerId) {
    Optional<Long> pid = sequenceUtils.getLocalPid(providerId);

    if (!pid.isPresent()) {
      lockProvider.getLock(NEW_PROVIDER_LOCK_NAME).lock();
      try {
        final Pair<Long, Boolean> auxPid = sequenceUtils.getNewPidIfAbsent(providerId);
        // Provider must be registered into Redis if its pid doesn't exist
        if (auxPid.getSecond()) {
          sRedisTemplate.set(keysBuilder.getProviderKey(auxPid.getFirst()), providerId);
          // Reverse lookup key for quickly get the {pid} from a provider with identifier providerId
          sRedisTemplate.set(keysBuilder.getReverseProviderKey(providerId), auxPid.getFirst().toString());
          LOGGER.debug("Registered in Redis provider {} with pid {}", providerId, auxPid.getFirst());
        }
        pid = Optional.of(auxPid.getFirst());
      } finally {
        lockProvider.getLock(NEW_PROVIDER_LOCK_NAME).unlock();
      }
    }

    return pid.get();
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.platform.common.service.ResourceService#registerSensorIfNeedBe(org.sentilo.common.
   * domain.CatalogSensor, boolean)
   */
  @Override
  public Long registerSensorIfNeedBe(final CatalogSensor sensor, final boolean update) {
    Optional<Long> sid = sequenceUtils.getLocalSid(sensor.getProvider(), sensor.getSensor());
    boolean isNew = false;
    if (!sid.isPresent()) {
      // Sensor sequence doesn't exist in local cache. It should be get from Redis (existing or
      // new sequence one)
      lockProvider.getLock(NEW_SENSOR_LOCK_NAME).lock();
      try {
        final Pair<Long, Boolean> auxSid = sequenceUtils.getNewSidIfAbsent(sensor.getProvider(), sensor.getSensor());
        // Sensor must be registered into Redis if its sid doesn't exist
        if (auxSid.getSecond()) {
          // To quickly get the sensors list from a provider, a reverse lookup key is defined
          final Long pid = sequenceUtils.getPid(sensor.getProvider()).get();
          sRedisTemplate.sAdd(keysBuilder.getProviderSensorsKey(pid), auxSid.getFirst().toString());

          // And finally, a new reverse lookup key is defined to quickly get the internal sid of a
          // sensor from the pair <providerId,sensorId>, so that sensor's data could be stored
          sRedisTemplate.set(keysBuilder.getReverseSensorKey(sensor.getProvider(), sensor.getSensor()), auxSid.getFirst().toString());
        }
        sid = Optional.of(auxSid.getFirst());
        isNew = auxSid.getSecond();
      } finally {
        lockProvider.getLock(NEW_SENSOR_LOCK_NAME).unlock();
      }
    }

    if (isNew || update) {
      // Store a hash with key sid:{sid} and fields provider, sensor, state and ttl
      final Map<String, String> fields = new HashMap<String, String>();
      fields.put(PROVIDER, sensor.getProvider());
      fields.put(SENSOR, sensor.getSensor());
      fields.put(STATE, sensor.getState().name());
      fields.put(TTL, catalogSensorTtlToRedisTtl(sensor.getTtl()));

      sRedisTemplate.hmSet(keysBuilder.getSensorKey(sid.get()), fields);

      LOGGER.debug("Saved in Redis sensor [{}]  with sid [{}] and state [{}], belonging to provider [{}]", sensor.getSensor(), sid.get(),
          sensor.getState().name(), sensor.getProvider());
    }

    return sid.get();
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.platform.common.service.ResourceService#getSensorsFromProvider(java.lang.String)
   */
  @Override
  public Set<String> getSensorsFromProvider(final String providerId) {
    final Optional<Long> pid = sequenceUtils.getPid(providerId);
    return getSensorsFromProvider(pid);

  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.platform.common.service.ResourceService#getSensorsToInspect(java.lang.String,
   * java.lang.String)
   */
  @Override
  public Set<String> getSensorsToInspect(final String providerId, final String sensorId) {
    Set<String> sids = null;

    if (!StringUtils.hasText(providerId)) {
      return sids;
    }

    if (StringUtils.hasText(sensorId)) {
      final Optional<Long> sid = sequenceUtils.getSid(providerId, sensorId);
      if (sid.isPresent()) {
        sids = new HashSet<String>();
        sids.add(sid.get().toString());
      }
    } else {
      sids = getSensorsFromProvider(providerId);
    }

    return sids;
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.platform.common.service.ResourceService#getSensor(java.lang.String,
   * java.lang.String, boolean)
   */
  @Override
  public Optional<Sensor> getSensor(final String providerId, final String sensorId, final boolean create) {
    // If not exist a sensor in Redis with the given parameters and create parameter is true, then a
    // new ghost sensor is created and returned.
    final Optional<Long> sid = sequenceUtils.getSid(providerId, sensorId);
    Optional<Sensor> sensor = Optional.empty();

    if (sid.isPresent()) {
      sensor = getSensor(sid.get());
    }

    if (!sensor.isPresent() && create) {
      registerProviderIfNeedBe(providerId);
      final Sensor aux = registerGhostSensorIfNeedBe(providerId, sensorId);
      sensor = Optional.of(aux);
    }

    return sensor;
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.platform.common.service.ResourceService#getSensor(java.lang.Long)
   */
  @Override
  public Optional<Sensor> getSensor(final Long sid) {
    final Map<String, String> infoSid = sRedisTemplate.hGetAll(keysBuilder.getSensorKey(sid));
    if (!CollectionUtils.isEmpty(infoSid)) {
      final String sensorId = infoSid.get(SENSOR);
      final String providerId = infoSid.get(PROVIDER);
      final String state = infoSid.get(STATE);
      final String ttl = infoSid.get(TTL);
      return Optional.of(new Sensor(sid, providerId, sensorId, state, ttl));
    } else {
      return Optional.empty();
    }
  }

  @Override
  public Optional<Alert> getAlert(final String alertId) {
    final Optional<Long> aid = sequenceUtils.getAid(alertId);
    Optional<Alert> alert = Optional.empty();

    if (aid.isPresent()) {
      alert = getAlert(aid.get());
    }

    return alert;
  }

  public Optional<Alert> getAlert(final Long aid) {
    final Map<String, String> infoAid = sRedisTemplate.hGetAll(keysBuilder.getAlertKey(aid));
    if (!CollectionUtils.isEmpty(infoAid)) {
      final String alertId = infoAid.get(ALERT);
      final String entity = infoAid.get(ENTITY);
      final String active = infoAid.get(ACTIVE);
      return Optional.of(new Alert(alertId, entity, active));
    } else {
      return Optional.empty();
    }
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.platform.common.service.ResourceService#registerAlertIfNeedBe(org.sentilo.common.
   * domain.CatalogAlert, boolean)
   */
  @Override
  public Long registerAlertIfNeedBe(final CatalogAlert alert, final boolean update) {
    Optional<Long> aid = sequenceUtils.getLocalAid(alert.getId());
    boolean isNew = false;

    if (!aid.isPresent()) {
      lockProvider.getLock(NEW_ALERT_LOCK_NAME).lock();
      try {
        final Pair<Long, Boolean> auxAid = sequenceUtils.getNewAidIfAbsent(alert.getId());
        if (auxAid.getSecond()) {
          // A reverse lookup key is defined for quickly get the {aid} from an alert with identifier
          // alertId
          sRedisTemplate.set(keysBuilder.getReverseAlertKey(alert.getId()), auxAid.getFirst().toString());
        }
        aid = Optional.of(auxAid.getFirst());
        isNew = auxAid.getSecond();
      } finally {
        lockProvider.getLock(NEW_ALERT_LOCK_NAME).unlock();
      }
    }

    if (isNew || update) {
      // Store a hash with key aid:{aid} and fields alert, entity and active
      final Map<String, String> fields = new HashMap<String, String>();
      fields.put(ALERT, alert.getId());
      fields.put(ENTITY, alert.getEntity());
      fields.put(ACTIVE, alert.getActive());
      sRedisTemplate.hmSet(keysBuilder.getAlertKey(aid.get()), fields);

      LOGGER.debug("Registered in Redis alert [{}] with aid [{}] and active flag to [{}]", alert.getId(), aid.get(), alert.getActive());
    }

    return aid.get();
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.platform.common.service.ResourceService#removeProvider(java.lang.String)
   */
  @Override
  public void removeProvider(final String providerId) {
    LOGGER.debug("Deleting provider {} from Redis", providerId);
    final Optional<Long> pid = sequenceUtils.getPid(providerId);
    if (pid.isPresent()) {
      // Remove key pid:{pid}
      sRedisTemplate.del(keysBuilder.getProviderKey(pid.get()));
      // Remove key provider:{providerId}:pid
      sRedisTemplate.del(keysBuilder.getReverseProviderKey(providerId));

      // Remove every sensor related to the provider.
      final Set<String> sids = getSensorsFromProvider(pid);
      if (!CollectionUtils.isEmpty(sids)) {
        for (final String sid : sids) {
          removeSensor(Long.valueOf(sid), providerId, false);
        }
      }

      // Remove key pid:{pid}:sensors
      sRedisTemplate.del(keysBuilder.getProviderSensorsKey(pid.get()));

      // Finally, remove {pid} from internal cache
      sequenceUtils.removePid(providerId);
    }

    LOGGER.debug("Provider [{}] deleted", providerId);
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.platform.common.service.ResourceService#removeSensor(java.lang.String,
   * java.lang.String)
   */
  @Override
  public void removeSensor(final String sensorId, final String providerId) {
    LOGGER.debug("Deleting in Redis sensor [{}] belonging to  provider {}", sensorId, providerId);
    final Optional<Long> sid = sequenceUtils.getSid(providerId, sensorId);
    if (sid.isPresent()) {
      removeSensor(sid.get(), providerId, true);
    }

    LOGGER.debug("Sensor [{}], belonging to provider [{}], deleted.", sensorId, providerId);
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.platform.common.service.ResourceService#removeAlert(org.sentilo.common.domain.
   * CatalogAlert)
   */
  @Override
  public void removeAlert(final CatalogAlert alert) {
    LOGGER.debug("Deleting in Redis alert [{}] belonging to entity {}", alert.getId(), alert.getEntity());
    final Optional<Long> aid = sequenceUtils.getAid(alert.getId());
    if (aid.isPresent()) {
      sRedisTemplate.del(keysBuilder.getReverseAlertKey(alert.getId()));
      sRedisTemplate.del(keysBuilder.getAlertKey(aid.get()));
      // Finally, remove {aid} from the internal cache
      sequenceUtils.removeAid(alert.getId());
    }

    LOGGER.debug("Alert [{}], belonging to entity [{}], deleted.", alert.getId(), alert.getEntity());

  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.platform.common.service.ResourceService#checkSensorState(java.lang.String,
   * java.lang.String)
   */
  @Override
  public void checkSensorState(final String providerId, final String sensorId) {
    if (!StringUtils.hasText(sensorId)) {
      return;
    }

    final Optional<Sensor> sensor = getSensor(providerId, sensorId, false);
    final boolean existsSensor = sensor.isPresent() && !SensorState.ghost.equals(sensor.get().getState());

    if (!existsSensor) {
      throw new ResourceNotFoundException(sensorId, "Sensor");
    } else if (SensorState.offline.equals(sensor.get().getState())) {
      throw new ResourceOfflineException(sensorId, "Sensor");
    }
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.platform.common.service.ResourceService#checkAlertState(java.lang.String)
   */
  @Override
  public void checkAlertState(final String alertId) {
    if (!StringUtils.hasText(alertId)) {
      return;
    }

    final Optional<Alert> alert = getAlert(alertId);

    if (!alert.isPresent()) {
      throw new ResourceNotFoundException(alertId, "Alert");
    } else if (!alert.get().isActive()) {
      throw new ResourceOfflineException(alertId, "Alert");
    }
  }

  private Sensor registerGhostSensorIfNeedBe(final String providerId, final String sensorId) {

    final CatalogSensor catalogSensor = new CatalogSensor();
    catalogSensor.setProvider(providerId);
    catalogSensor.setSensor(sensorId);
    catalogSensor.setState(SensorState.ghost);

    final Long sid = registerSensorIfNeedBe(catalogSensor, false);
    final Sensor sensor = new Sensor(providerId, sensorId);
    sensor.setSid(sid);
    sensor.setState(SensorState.ghost);

    return sensor;
  }

  private void removeSensor(final Long sid, final String providerId, final boolean removeFromProviderList) {
    if (sid != null) {
      // Remove key sensor:{providerId}:{sensorId}:sid
      final String sensorId = sRedisTemplate.hGet(keysBuilder.getSensorKey(sid), "sensor");
      sRedisTemplate.del(keysBuilder.getReverseSensorKey(providerId, sensorId));

      // Remove key sid:{sid}:observations
      sRedisTemplate.del(keysBuilder.getSensorObservationsKey(sid));

      // Remove key sid:{sid}:orders
      sRedisTemplate.del(keysBuilder.getSensorOrdersKey(sid));

      // Remove key sid:{sid}
      sRedisTemplate.del(keysBuilder.getSensorKey(sid));
      if (removeFromProviderList) {
        // Only if removeFromProviderList is true, i.e., method is invoked to remove only sensor
        // identified by sid,
        // also remove reference to sensor in the list defined by key
        // keysBuilder.getProviderSensorsKey(pid)

        final Optional<Long> pid = sequenceUtils.getPid(providerId);
        sRedisTemplate.sRem(keysBuilder.getProviderSensorsKey(pid.get()), sid.toString());
      }

      // Finally, remove {sid} from the internal cache
      sequenceUtils.removeSid(providerId, sensorId);
    }
  }

  private Set<String> getSensorsFromProvider(final Optional<Long> pid) {
    // If provider hasn't a pid sequence this means that provider is not defined and so it hasn't
    // related sensors.
    return pid.isPresent() ? sRedisTemplate.sMembers(keysBuilder.getProviderSensorsKey(pid.get())) : Collections.<String>emptySet();
  }

}
