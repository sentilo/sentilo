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
import java.util.Map;
import java.util.Set;

import org.sentilo.common.domain.CatalogAlert;
import org.sentilo.common.domain.CatalogSensor;
import org.sentilo.common.enums.SensorState;
import org.sentilo.platform.common.domain.Alert;
import org.sentilo.platform.common.domain.Sensor;
import org.sentilo.platform.common.service.ResourceService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

@Service
public class ResourceServiceImpl extends AbstractPlatformServiceImpl implements ResourceService {

  private static final Logger LOGGER = LoggerFactory.getLogger(ResourceServiceImpl.class);

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.platform.common.service.ResourceService#registerProviderIfNecessary(java.lang.
   * String )
   */
  public Long registerProviderIfNeedBe(final String providerId) {
    Long pid = sequenceUtils.getPid(providerId);
    // Provider must be registered into Redis if its pid doesn't exist
    if (pid == null) {
      pid = sequenceUtils.setPid(providerId);
      sRedisTemplate.set(keysBuilder.getProviderKey(pid), providerId);
      // Reverse lookup key for quickly get the {pid} from a provider with identifier providerId
      sRedisTemplate.set(keysBuilder.getReverseProviderKey(providerId), pid.toString());
      LOGGER.debug("Registered in Redis provider {} with pid {}", providerId, pid);
    }

    return pid;
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.platform.common.service.ResourceService#registerGhostSensorIfNeedBe(org.sentilo.
   * platform.common.domain.Sensor)
   */
  public Long registerGhostSensorIfNeedBe(final Sensor sensor) {
    if (!sensor.exists()) {
      final CatalogSensor catalogSensor = new CatalogSensor();
      catalogSensor.setProvider(sensor.getProvider());
      catalogSensor.setSensor(sensor.getSensor());
      catalogSensor.setState(SensorState.ghost);

      final Long sid = registerSensorIfNeedBe(catalogSensor, false);

      sensor.setSid(sid);
      sensor.setState(SensorState.ghost);
    }

    return sensor.getSid();
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.platform.common.service.ResourceService#registerSensorIfNeedBe(org.sentilo.common.
   * domain.CatalogSensor, boolean)
   */
  public Long registerSensorIfNeedBe(final CatalogSensor sensor, final boolean update) {

    Long sid = sequenceUtils.getSid(sensor.getProvider(), sensor.getSensor());
    // Sensor must be registered into Redis if its sid doesn't exist or if update is true
    if (sid == null || update) {

      if (sid == null) {
        final Long pid = sequenceUtils.getPid(sensor.getProvider());
        sid = sequenceUtils.setSid(sensor.getProvider(), sensor.getSensor());

        // To quickly get the sensors list from a provider, a reverse lookup key is defined
        sRedisTemplate.sAdd(keysBuilder.getProviderSensorsKey(pid), sid.toString());

        // And finally, a new reverse lookup key is defined to quickly get the internal sid of a
        // sensor from the pair <providerId,sensorId>, so that sensor's data could be stored
        sRedisTemplate.set(keysBuilder.getReverseSensorKey(sensor.getProvider(), sensor.getSensor()), sid.toString());
      }

      // Store a hash with key sid:{sid} and fields provider, sensor, state and ttl
      final Map<String, String> fields = new HashMap<String, String>();
      fields.put(PROVIDER, sensor.getProvider());
      fields.put(SENSOR, sensor.getSensor());
      fields.put(STATE, sensor.getState().name());
      fields.put(TTL, catalogSensorTtlToRedisTtl(sensor.getTtl()));

      sRedisTemplate.hmSet(keysBuilder.getSensorKey(sid), fields);

      LOGGER.debug("Saved in Redis sensor [{}]  with sid [{}] and state [{}], belonging to provider [{}]", sensor.getSensor(), sid,
          sensor.getState().name(), sensor.getProvider());
    }

    return sid;
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.platform.common.service.ResourceService#getSensorsFromProvider(java.lang.String)
   */
  public Set<String> getSensorsFromProvider(final String providerId) {
    final Long pid = sequenceUtils.getPid(providerId);

    return getSensorsFromProvider(pid);

  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.platform.common.service.ResourceService#getSensorsToInspect(java.lang.String,
   * java.lang.String)
   */
  public Set<String> getSensorsToInspect(final String providerId, final String sensorId) {
    Set<String> sids = null;

    if (!StringUtils.hasText(providerId)) {
      return sids;
    }

    if (StringUtils.hasText(sensorId)) {
      final Long sid = sequenceUtils.getSid(providerId, sensorId);
      if (sid != null) {
        sids = new HashSet<String>();
        sids.add(sid.toString());
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
   * java.lang.String)
   */
  public Sensor getSensor(final String providerId, final String sensorId) {
    final Long sid = sequenceUtils.getSid(providerId, sensorId);
    return sid != null ? getSensor(sid) : new Sensor(providerId, sensorId);
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.platform.common.service.ResourceService#getSensor(java.lang.Long)
   */
  public Sensor getSensor(final Long sid) {
    Sensor sensor = null;
    final Map<String, String> infoSid = sRedisTemplate.hGetAll(keysBuilder.getSensorKey(sid));
    if (!CollectionUtils.isEmpty(infoSid)) {
      final String sensorId = infoSid.get(SENSOR);
      final String providerId = infoSid.get(PROVIDER);
      final String state = infoSid.get(STATE);
      final String ttl = infoSid.get(TTL);
      sensor = new Sensor(sid, providerId, sensorId, state, ttl);
    }
    return sensor;
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.platform.common.service.ResourceService#getAlert(java.lang.Long)
   */
  public Alert getAlert(final Long aid) {
    Alert alert = null;
    final Map<String, String> infoAid = sRedisTemplate.hGetAll(keysBuilder.getAlertKey(aid));
    if (!CollectionUtils.isEmpty(infoAid)) {
      final String alertId = infoAid.get(ALERT);
      final String entity = infoAid.get(ENTITY);
      final String active = infoAid.get(ACTIVE);
      alert = new Alert(alertId, entity, active);
    }
    return alert;
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.platform.common.service.ResourceService#registerAlertIfNeedBe(org.sentilo.common.
   * domain.CatalogAlert, boolean)
   */
  public Long registerAlertIfNeedBe(final CatalogAlert alert, final boolean update) {
    Long aid = sequenceUtils.getAid(alert.getId());
    if (aid == null || update) {
      if (aid == null) {
        aid = sequenceUtils.setAid(alert.getId());

        // A reverse lookup key is defined for quickly get the {aid} from an alert with identifier
        // alertId
        sRedisTemplate.set(keysBuilder.getReverseAlertKey(alert.getId()), aid.toString());
      }

      // Store a hash with key aid:{aid} and fields alert, entity and active
      final Map<String, String> fields = new HashMap<String, String>();
      fields.put(ALERT, alert.getId());
      fields.put(ENTITY, alert.getEntity());
      fields.put(ACTIVE, alert.getActive());
      sRedisTemplate.hmSet(keysBuilder.getAlertKey(aid), fields);

      LOGGER.debug("Registered in Redis alert [{}] with aid [{}] and active flag to [{}]", alert.getId(), aid, alert.getActive());
    }

    return aid;
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.platform.common.service.ResourceService#removeProvider(java.lang.String)
   */
  public void removeProvider(final String providerId) {
    LOGGER.debug("Deleting in Redis provider {} ", providerId);
    final Long pid = sequenceUtils.getPid(providerId);
    if (pid != null) {
      // Remove key pid:{pid}
      sRedisTemplate.del(keysBuilder.getProviderKey(pid));
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
      sRedisTemplate.del(keysBuilder.getProviderSensorsKey(pid));

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
  public void removeSensor(final String sensorId, final String providerId) {
    LOGGER.debug("Deleting in Redis sensor [{}] belonging to  provider {}", sensorId, providerId);
    final Long sid = sequenceUtils.getSid(providerId, sensorId);
    if (sid != null) {
      removeSensor(sid, providerId, true);
    }

    LOGGER.debug("Sensor [{}], belonging to provider [{}], deleted.", sensorId, providerId);
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.platform.common.service.ResourceService#removeAlert(org.sentilo.common.domain.
   * CatalogAlert)
   */
  public void removeAlert(final CatalogAlert alert) {
    LOGGER.debug("Deleting in Redis alert [{}] belonging to entity {}", alert.getId(), alert.getEntity());
    final Long aid = sequenceUtils.getAid(alert.getId());
    if (aid != null) {
      sRedisTemplate.del(keysBuilder.getReverseAlertKey(alert.getId()));
      sRedisTemplate.del(keysBuilder.getAlertKey(aid));
      // Finally, remove {aid} from the internal cache
      sequenceUtils.removeAid(alert.getId());
    }

    LOGGER.debug("Alert [{}], belonging to entity [{}], deleted.", alert.getId(), alert.getEntity());

  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.platform.common.service.ResourceService#getSensorState(java.lang.String,
   * java.lang.String)
   */
  public SensorState getSensorState(final String providerId, final String sensorId) {
    final Sensor sensor = getSensor(providerId, sensorId);

    return sensor.getState();
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.platform.common.service.ResourceService#isAlertDisabled(java.lang.String)
   */
  public boolean isAlertDisabled(final String alertId) {
    final Long aid = sequenceUtils.getAid(alertId);
    boolean alertDisabled = false;

    if (aid != null) {
      final Alert alert = getAlert(aid);
      alertDisabled = alert != null && StringUtils.hasText(alert.getActive()) && !Boolean.valueOf(alert.getActive());
    }

    return alertDisabled;
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.platform.common.service.ResourceService#existsAlert(java.lang.String)
   */
  public boolean existsAlert(final String alertId) {
    final Long aid = sequenceUtils.getAid(alertId);

    return aid != null;
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

        final Long pid = sequenceUtils.getPid(providerId);
        sRedisTemplate.sRem(keysBuilder.getProviderSensorsKey(pid), sid.toString());
      }

      // Finally, remove {sid} from the internal cache
      sequenceUtils.removeSid(providerId, sensorId);
    }
  }

  private Set<String> getSensorsFromProvider(final Long pid) {
    // Si no hay identificador interno del proveedor, entonces este no esta registrado en Redis y
    // por lo tanto no tiene ningun sensor asociado
    return pid == null ? Collections.<String>emptySet() : sRedisTemplate.sMembers(keysBuilder.getProviderSensorsKey(pid));
  }

}
