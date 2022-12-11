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
package org.sentilo.platform.service.test.service;

import static org.mockito.Matchers.anyMapOf;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.locks.Lock;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.domain.CatalogAlert;
import org.sentilo.common.domain.CatalogSensor;
import org.sentilo.common.enums.SensorState;
import org.sentilo.common.lock.LockFactory;
import org.sentilo.platform.common.domain.Alert;
import org.sentilo.platform.common.domain.Sensor;
import org.sentilo.platform.service.dao.SentiloRedisTemplate;
import org.sentilo.platform.service.dao.SentiloSequenceUtils;
import org.sentilo.platform.service.impl.ResourceServiceImpl;
import org.springframework.data.util.Pair;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.util.CollectionUtils;

public class ResourceServiceImplTest {

  private static final String PROVIDER_ID = "provider1";
  private static final String SENSOR_ID = "sensor1";
  private static final String ALERT_ID = "alert1";
  private static final Long PID = new Long(1);
  private static final Long SID = new Long(1);
  private static final Long AID = new Long(1);

  private final int platformExpireTime = 60 * 60;

  @Mock
  private CatalogAlert catalogAlert;
  @Mock
  private SentiloRedisTemplate sRedisTemplate;
  @Mock
  private SentiloSequenceUtils sequenceUtils;
  @Mock
  private LockFactory lockProvider;
  @Mock
  private Lock lock;

  @InjectMocks
  private ResourceServiceImpl service;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    ReflectionTestUtils.setField(service, "expireSeconds", platformExpireTime);
    when(lockProvider.getLock(anyString())).thenReturn(lock);
  }

  @Test
  public void registerNewProvider() {
    when(sequenceUtils.getLocalPid(PROVIDER_ID)).thenReturn(Optional.empty());
    when(sequenceUtils.getPid(PROVIDER_ID)).thenReturn(Optional.empty());
    when(sequenceUtils.getNewPidIfAbsent(PROVIDER_ID)).thenReturn(Pair.of(PID, true));

    service.registerProviderIfNeedBe(PROVIDER_ID);

    verify(sequenceUtils).getLocalPid(PROVIDER_ID);
    verify(lockProvider, times(2)).getLock("register_new_provider");
    verify(lock).lock();
    verify(sequenceUtils).getNewPidIfAbsent(PROVIDER_ID);
    verify(sRedisTemplate).set(service.getKeysBuilder().getProviderKey(PID), PROVIDER_ID);
    verify(sRedisTemplate).set(service.getKeysBuilder().getReverseProviderKey(PROVIDER_ID), PID.toString());
    verify(lock).unlock();
  }

  @Test
  public void registerProviderThatAlreadyExists() {
    when(sequenceUtils.getLocalPid(PROVIDER_ID)).thenReturn(Optional.of(PID));

    service.registerProviderIfNeedBe(PROVIDER_ID);

    verify(sequenceUtils).getLocalPid(PROVIDER_ID);
    verify(sequenceUtils, times(0)).getNewPidIfAbsent(PROVIDER_ID);
    verify(sRedisTemplate, times(0)).set(service.getKeysBuilder().getProviderKey(PID), PROVIDER_ID);
    verify(sRedisTemplate, times(0)).set(service.getKeysBuilder().getReverseProviderKey(PROVIDER_ID), PID.toString());
  }

  @Test
  public void registerNewSensor() {
    final CatalogSensor sensor = new CatalogSensor();
    sensor.setProvider(PROVIDER_ID);
    sensor.setSensor(SENSOR_ID);
    sensor.setState(SensorState.online);
    sensor.setTtl(5 * 60);

    final Map<String, String> fields = new HashMap<String, String>();
    fields.put("provider", PROVIDER_ID);
    fields.put("sensor", SENSOR_ID);
    fields.put("state", "online");
    fields.put("ttl", Integer.toString(5 * 60));

    when(sequenceUtils.getLocalSid(PROVIDER_ID, SENSOR_ID)).thenReturn(Optional.empty());
    when(sequenceUtils.getPid(PROVIDER_ID)).thenReturn(Optional.of(PID));
    when(sequenceUtils.getNewSidIfAbsent(PROVIDER_ID, SENSOR_ID)).thenReturn(Pair.of(SID, true));

    service.registerSensorIfNeedBe(sensor, false);

    verify(sequenceUtils).getLocalSid(PROVIDER_ID, SENSOR_ID);
    verify(lockProvider, times(2)).getLock("register_new_sensor");
    verify(lock).lock();
    verify(sequenceUtils).getPid(PROVIDER_ID);
    verify(sequenceUtils).getNewSidIfAbsent(PROVIDER_ID, SENSOR_ID);
    verify(sRedisTemplate).hmSet(service.getKeysBuilder().getSensorKey(SID), fields);
    verify(sRedisTemplate).sAdd(service.getKeysBuilder().getProviderSensorsKey(PID), SID.toString());
    verify(sRedisTemplate).set(service.getKeysBuilder().getReverseSensorKey(PROVIDER_ID, SENSOR_ID), SID.toString());
    verify(lock).unlock();
  }

  @Test
  public void registerSensor_when_exists_in_local_cache() {
    final CatalogSensor sensor = new CatalogSensor();
    sensor.setProvider(PROVIDER_ID);
    sensor.setSensor(SENSOR_ID);
    sensor.setState(SensorState.online);
    when(sequenceUtils.getLocalSid(PROVIDER_ID, SENSOR_ID)).thenReturn(Optional.of(SID));

    service.registerSensorIfNeedBe(sensor, false);

    verify(sequenceUtils).getLocalSid(PROVIDER_ID, SENSOR_ID);
    verify(lockProvider, times(0)).getLock("register_new_sensor");
    verify(sequenceUtils, times(0)).getPid(PROVIDER_ID);
    verify(sequenceUtils, times(0)).getNewSidIfAbsent(PROVIDER_ID, SENSOR_ID);
    verify(sRedisTemplate, times(0)).hmSet(anyString(), anyMapOf(String.class, String.class));
    verify(sRedisTemplate, times(0)).sAdd(service.getKeysBuilder().getProviderSensorsKey(PID), SID.toString());
    verify(sRedisTemplate, times(0)).sAdd(service.getKeysBuilder().getReverseSensorKey(PROVIDER_ID, SENSOR_ID), SID.toString());
  }

  @Test
  public void registerSensorThatAlreadyExists() {
    final CatalogSensor sensor = new CatalogSensor();
    sensor.setProvider(PROVIDER_ID);
    sensor.setSensor(SENSOR_ID);
    sensor.setState(SensorState.online);
    when(sequenceUtils.getLocalSid(PROVIDER_ID, SENSOR_ID)).thenReturn(Optional.empty());
    when(sequenceUtils.getNewSidIfAbsent(PROVIDER_ID, SENSOR_ID)).thenReturn(Pair.of(SID, false));

    service.registerSensorIfNeedBe(sensor, false);

    verify(lockProvider, times(2)).getLock("register_new_sensor");
    verify(lock).lock();
    verify(sequenceUtils).getLocalSid(PROVIDER_ID, SENSOR_ID);
    verify(sequenceUtils).getNewSidIfAbsent(PROVIDER_ID, SENSOR_ID);
    verify(sequenceUtils, times(0)).getPid(PROVIDER_ID);
    verify(sRedisTemplate, times(0)).hmSet(anyString(), anyMapOf(String.class, String.class));
    verify(sRedisTemplate, times(0)).sAdd(service.getKeysBuilder().getProviderSensorsKey(PID), SID.toString());
    verify(sRedisTemplate, times(0)).sAdd(service.getKeysBuilder().getReverseSensorKey(PROVIDER_ID, SENSOR_ID), SID.toString());
    verify(lock).unlock();
  }

  @Test
  public void updateSensorThatAlreadyExists() {
    final CatalogSensor sensor = new CatalogSensor();
    sensor.setProvider(PROVIDER_ID);
    sensor.setSensor(SENSOR_ID);
    sensor.setState(SensorState.online);
    sensor.setTtl(30 * 60);

    final Map<String, String> fields = new HashMap<String, String>();
    fields.put("provider", PROVIDER_ID);
    fields.put("sensor", SENSOR_ID);
    fields.put("state", "online");
    fields.put("ttl", Integer.toString(30 * 60));

    when(sequenceUtils.getLocalSid(PROVIDER_ID, SENSOR_ID)).thenReturn(Optional.of(SID));

    service.registerSensorIfNeedBe(sensor, true);

    verify(sequenceUtils).getLocalSid(PROVIDER_ID, SENSOR_ID);
    verify(lockProvider, times(0)).getLock("register_new_sensor");
    verify(sequenceUtils, times(0)).getPid(PROVIDER_ID);
    verify(sequenceUtils, times(0)).getNewSidIfAbsent(PROVIDER_ID, SENSOR_ID);
    verify(sRedisTemplate).hmSet(service.getKeysBuilder().getSensorKey(SID), fields);
    verify(sRedisTemplate, times(0)).sAdd(service.getKeysBuilder().getProviderSensorsKey(PID), SID.toString());
    verify(sRedisTemplate, times(0)).sAdd(service.getKeysBuilder().getReverseSensorKey(PROVIDER_ID, SENSOR_ID), SID.toString());
  }

  @Test
  public void updateSensorWithoutTTL() {
    final CatalogSensor sensor = new CatalogSensor();
    sensor.setProvider(PROVIDER_ID);
    sensor.setSensor(SENSOR_ID);
    sensor.setState(SensorState.online);

    final Map<String, String> fields = new HashMap<String, String>();
    fields.put("provider", PROVIDER_ID);
    fields.put("sensor", SENSOR_ID);
    fields.put("state", "online");
    fields.put("ttl", Integer.toString(platformExpireTime));

    when(sequenceUtils.getLocalSid(PROVIDER_ID, SENSOR_ID)).thenReturn(Optional.of(SID));

    service.registerSensorIfNeedBe(sensor, true);

    verify(sequenceUtils).getLocalSid(PROVIDER_ID, SENSOR_ID);
    verify(lockProvider, times(0)).getLock("register_new_sensor");
    verify(sequenceUtils, times(0)).getPid(PROVIDER_ID);
    verify(sequenceUtils, times(0)).getNewSidIfAbsent(PROVIDER_ID, SENSOR_ID);
    verify(sRedisTemplate).hmSet(service.getKeysBuilder().getSensorKey(SID), fields);
    verify(sRedisTemplate, times(0)).sAdd(service.getKeysBuilder().getProviderSensorsKey(PID), SID.toString());
    verify(sRedisTemplate, times(0)).sAdd(service.getKeysBuilder().getReverseSensorKey(PROVIDER_ID, SENSOR_ID), SID.toString());
  }

  @Test
  public void registerGhostSensor() {
    final Map<String, String> fields = new HashMap<String, String>();
    fields.put("provider", PROVIDER_ID);
    fields.put("sensor", SENSOR_ID);
    fields.put("state", SensorState.ghost.name());
    fields.put("ttl", Integer.toString(platformExpireTime));

    when(sequenceUtils.getLocalSid(PROVIDER_ID, SENSOR_ID)).thenReturn(Optional.empty());
    when(sequenceUtils.getSid(PROVIDER_ID, SENSOR_ID)).thenReturn(Optional.empty());
    when(sequenceUtils.getLocalPid(PROVIDER_ID)).thenReturn(Optional.of(PID));
    when(sequenceUtils.getPid(PROVIDER_ID)).thenReturn(Optional.of(PID));
    when(sequenceUtils.getNewSidIfAbsent(PROVIDER_ID, SENSOR_ID)).thenReturn(Pair.of(SID, true));

    final Optional<Sensor> sensor = service.getSensor(PROVIDER_ID, SENSOR_ID, true);

    verify(sequenceUtils).getSid(PROVIDER_ID, SENSOR_ID);
    verify(lockProvider, times(2)).getLock("register_new_sensor");
    verify(lock).lock();
    verify(sequenceUtils).getPid(PROVIDER_ID);
    verify(sequenceUtils).getNewSidIfAbsent(PROVIDER_ID, SENSOR_ID);
    verify(sRedisTemplate).hmSet(service.getKeysBuilder().getSensorKey(SID), fields);
    verify(sRedisTemplate).sAdd(service.getKeysBuilder().getProviderSensorsKey(PID), SID.toString());
    verify(sRedisTemplate).set(service.getKeysBuilder().getReverseSensorKey(PROVIDER_ID, SENSOR_ID), SID.toString());
    Assert.assertEquals(SID, sensor.get().getSid());
    Assert.assertEquals(SensorState.ghost, sensor.get().getState());
    verify(lock).unlock();
  }

  @Test
  public void getSensorsFromProvider() {
    when(sequenceUtils.getPid(PROVIDER_ID)).thenReturn(Optional.of(PID));
    when(sRedisTemplate.sMembers(service.getKeysBuilder().getProviderSensorsKey(PID))).thenReturn(Collections.<String>emptySet());

    final Set<String> members = service.getSensorsFromProvider(PROVIDER_ID);

    verify(sequenceUtils).getPid(PROVIDER_ID);
    verify(sRedisTemplate).sMembers(service.getKeysBuilder().getProviderSensorsKey(PID));
    Assert.assertNotNull(members);
  }

  @Test
  public void getSensorsFromNotRegisteredProvider() {
    when(sequenceUtils.getPid(PROVIDER_ID)).thenReturn(Optional.empty());

    final Set<String> members = service.getSensorsFromProvider(PROVIDER_ID);

    verify(sequenceUtils).getPid(PROVIDER_ID);
    verify(sRedisTemplate, times(0)).sMembers(anyString());
    Assert.assertTrue(CollectionUtils.isEmpty(members));
  }

  @Test
  public void getSensorFromInternalSid() {
    final Map<String, String> fields = new HashMap<String, String>();
    fields.put("provider", PROVIDER_ID);
    fields.put("sensor", SENSOR_ID);
    fields.put("state", SensorState.online.name());
    fields.put("ttl", "23");

    when(sRedisTemplate.hGetAll(service.getKeysBuilder().getSensorKey(SID))).thenReturn(fields);

    final Optional<Sensor> sensor = service.getSensor(SID);

    verify(sRedisTemplate).hGetAll(service.getKeysBuilder().getSensorKey(SID));
    Assert.assertEquals(PROVIDER_ID, sensor.get().getProvider());
    Assert.assertEquals(SENSOR_ID, sensor.get().getSensor());
  }

  @Test
  public void getSensor() {
    final Map<String, String> fields = new HashMap<String, String>();
    fields.put("provider", PROVIDER_ID);
    fields.put("sensor", SENSOR_ID);
    fields.put("state", SensorState.online.name());
    fields.put("ttl", "23");

    when(sequenceUtils.getSid(PROVIDER_ID, SENSOR_ID)).thenReturn(Optional.of(SID));
    when(sRedisTemplate.hGetAll(service.getKeysBuilder().getSensorKey(SID))).thenReturn(fields);

    final Optional<Sensor> sensor = service.getSensor(PROVIDER_ID, SENSOR_ID, false);

    verify(sequenceUtils).getSid(PROVIDER_ID, SENSOR_ID);
    verify(sRedisTemplate).hGetAll(service.getKeysBuilder().getSensorKey(SID));
    Assert.assertEquals(PROVIDER_ID, sensor.get().getProvider());
    Assert.assertEquals(SENSOR_ID, sensor.get().getSensor());
  }

  @Test
  public void getSensor_from_no_valid_sid() {
    when(sRedisTemplate.hGetAll(service.getKeysBuilder().getSensorKey(SID))).thenReturn(Collections.<String, String>emptyMap());

    final Optional<Sensor> sensor = service.getSensor(SID);

    verify(sRedisTemplate).hGetAll(service.getKeysBuilder().getSensorKey(SID));
    Assert.assertFalse(sensor.isPresent());
  }

  @Test
  public void removeProvider() {
    when(sequenceUtils.getPid(PROVIDER_ID)).thenReturn(Optional.of(PID));
    when(sRedisTemplate.sMembers(service.getKeysBuilder().getProviderSensorsKey(PID))).thenReturn(Collections.<String>emptySet());

    service.removeProvider(PROVIDER_ID);

    verify(sequenceUtils).getPid(PROVIDER_ID);
    verify(sRedisTemplate).del(service.getKeysBuilder().getProviderKey(PID));
    verify(sRedisTemplate).del(service.getKeysBuilder().getReverseProviderKey(PROVIDER_ID));
    verify(sequenceUtils).removePid(PROVIDER_ID);

  }

  @Test
  public void removeSensor() {
    when(sequenceUtils.getSid(PROVIDER_ID, SENSOR_ID)).thenReturn(Optional.of(SID));
    when(sequenceUtils.getPid(PROVIDER_ID)).thenReturn(Optional.of(PID));
    when(sRedisTemplate.hGet(service.getKeysBuilder().getSensorKey(SID), "sensor")).thenReturn(SENSOR_ID);

    when(sRedisTemplate.sMembers(service.getKeysBuilder().getProviderSensorsKey(PID))).thenReturn(Collections.<String>emptySet());

    service.removeSensor(SENSOR_ID, PROVIDER_ID);

    verify(sequenceUtils).getSid(PROVIDER_ID, SENSOR_ID);
    verify(sequenceUtils).getPid(PROVIDER_ID);
    verify(sRedisTemplate).del(service.getKeysBuilder().getSensorKey(SID));
    verify(sRedisTemplate).sRem(service.getKeysBuilder().getProviderSensorsKey(PID), SID.toString());
    verify(sRedisTemplate).del(service.getKeysBuilder().getReverseSensorKey(PROVIDER_ID, SENSOR_ID));
    verify(sRedisTemplate).del(service.getKeysBuilder().getSensorObservationsKey(SID));
    verify(sRedisTemplate).del(service.getKeysBuilder().getSensorOrdersKey(SID));
    verify(sequenceUtils).removeSid(PROVIDER_ID, SENSOR_ID);
  }

  @Test
  public void removeAlert() {
    when(catalogAlert.getId()).thenReturn(ALERT_ID);
    when(sequenceUtils.getAid(ALERT_ID)).thenReturn(Optional.of(AID));

    service.removeAlert(catalogAlert);

    verify(sequenceUtils).getAid(ALERT_ID);
    verify(sRedisTemplate).del(service.getKeysBuilder().getAlertKey(AID));
    verify(sRedisTemplate).del(service.getKeysBuilder().getReverseAlertKey(ALERT_ID));
    verify(sequenceUtils).removeAid(ALERT_ID);
  }

  @Test
  public void registerNewAlert() {
    final Map<String, String> fields = new HashMap<String, String>();
    fields.put("alert", ALERT_ID);
    fields.put("entity", PROVIDER_ID);
    fields.put("active", "true");

    when(catalogAlert.getId()).thenReturn(ALERT_ID);
    when(catalogAlert.getEntity()).thenReturn(PROVIDER_ID);
    when(catalogAlert.getActive()).thenReturn(Boolean.TRUE.toString());
    when(sequenceUtils.getLocalAid(ALERT_ID)).thenReturn(Optional.empty());
    when(sequenceUtils.getNewAidIfAbsent(ALERT_ID)).thenReturn(Pair.of(AID, true));

    service.registerAlertIfNeedBe(catalogAlert, false);

    verify(sequenceUtils).getLocalAid(ALERT_ID);
    verify(lockProvider, times(2)).getLock("register_new_alert");
    verify(lock).lock();
    verify(sequenceUtils).getNewAidIfAbsent(ALERT_ID);
    verify(sRedisTemplate).hmSet(service.getKeysBuilder().getAlertKey(AID), fields);
    verify(sRedisTemplate).set(service.getKeysBuilder().getReverseAlertKey(ALERT_ID), AID.toString());
    verify(lock).unlock();
  }

  @Test
  public void registerAlertThatAlreadyExists() {
    final Map<String, String> fields = new HashMap<String, String>();
    fields.put("alert", ALERT_ID);
    fields.put("entity", PROVIDER_ID);
    fields.put("active", "true");

    when(catalogAlert.getId()).thenReturn(ALERT_ID);
    when(catalogAlert.getEntity()).thenReturn(PROVIDER_ID);
    when(catalogAlert.getActive()).thenReturn(Boolean.TRUE.toString());
    when(sequenceUtils.getLocalAid(ALERT_ID)).thenReturn(Optional.of(AID));

    service.registerAlertIfNeedBe(catalogAlert, false);

    verify(sequenceUtils).getLocalAid(ALERT_ID);
    verify(lockProvider, times(0)).getLock("register_new_alert");
    verify(sequenceUtils, times(0)).getNewAidIfAbsent(ALERT_ID);
    verify(sRedisTemplate, times(0)).hmSet(service.getKeysBuilder().getAlertKey(AID), fields);
    verify(sRedisTemplate, times(0)).set(service.getKeysBuilder().getReverseAlertKey(ALERT_ID), AID.toString());
  }

  @Test
  public void updateAlertThatAlreadyExists() {
    final Map<String, String> fields = new HashMap<String, String>();
    fields.put("alert", ALERT_ID);
    fields.put("entity", PROVIDER_ID);
    fields.put("active", "true");

    when(catalogAlert.getId()).thenReturn(ALERT_ID);
    when(catalogAlert.getEntity()).thenReturn(PROVIDER_ID);
    when(catalogAlert.getActive()).thenReturn(Boolean.TRUE.toString());
    when(sequenceUtils.getLocalAid(ALERT_ID)).thenReturn(Optional.of(AID));

    service.registerAlertIfNeedBe(catalogAlert, true);

    verify(sequenceUtils).getLocalAid(ALERT_ID);
    verify(lockProvider, times(0)).getLock("register_new_alert");
    verify(sequenceUtils, times(0)).getNewAidIfAbsent(ALERT_ID);
    verify(sRedisTemplate).hmSet(service.getKeysBuilder().getAlertKey(AID), fields);
    verify(sRedisTemplate, times(0)).set(service.getKeysBuilder().getReverseAlertKey(ALERT_ID), AID.toString());
  }

  @SuppressWarnings("unchecked")
  @Test
  public void getSensorState() {
    final Map<String, String> fields = new HashMap<String, String>();
    fields.put("provider", PROVIDER_ID);
    fields.put("sensor", SENSOR_ID);
    fields.put("state", SensorState.online.name());

    final Map<String, String> fields2 = new HashMap<String, String>(fields);
    fields2.remove("state");
    final Map<String, String> fields3 = new HashMap<String, String>(fields);
    fields3.put("state", SensorState.offline.name());
    final Map<String, String> fields4 = new HashMap<String, String>(fields);
    fields4.put("state", SensorState.ghost.name());

    when(sRedisTemplate.hGetAll(service.getKeysBuilder().getSensorKey(SID))).thenReturn(fields, fields2, fields3, fields4);
    when(sequenceUtils.getSid(PROVIDER_ID, SENSOR_ID)).thenReturn(Optional.of(SID));

    final Optional<Sensor> sensor1 = service.getSensor(PROVIDER_ID, SENSOR_ID, false);
    final Optional<Sensor> sensor2 = service.getSensor(PROVIDER_ID, SENSOR_ID, false);
    final Optional<Sensor> sensor3 = service.getSensor(PROVIDER_ID, SENSOR_ID, false);
    final Optional<Sensor> sensor4 = service.getSensor(PROVIDER_ID, SENSOR_ID, false);

    Assert.assertEquals(SensorState.online, sensor1.get().getState());
    Assert.assertNull(sensor2.get().getState());
    Assert.assertEquals(SensorState.offline, sensor3.get().getState());
    Assert.assertEquals(SensorState.ghost, sensor4.get().getState());
  }

  @SuppressWarnings("unchecked")
  @Test
  public void isAlertActived() {
    final Map<String, String> fields = new HashMap<String, String>();
    fields.put("alert", ALERT_ID);
    fields.put("entity", PROVIDER_ID);
    fields.put("active", "true");

    final Map<String, String> fields2 = new HashMap<String, String>(fields);
    fields2.remove("active");
    final Map<String, String> fields3 = new HashMap<String, String>(fields);
    fields3.put("active", "false");

    when(sRedisTemplate.hGetAll(service.getKeysBuilder().getAlertKey(AID))).thenReturn(fields, fields2, fields3);
    when(sequenceUtils.getAid(ALERT_ID)).thenReturn(Optional.of(AID));

    final Optional<Alert> alert1 = service.getAlert(ALERT_ID);
    final Optional<Alert> alert2 = service.getAlert(ALERT_ID);
    final Optional<Alert> alert3 = service.getAlert(ALERT_ID);

    Assert.assertTrue(alert1.get().isActive());
    Assert.assertFalse(alert2.get().isActive());
    Assert.assertFalse(alert3.get().isActive());
  }

  @Test
  public void getAlert() {
    final Map<String, String> fields = new HashMap<String, String>();
    fields.put("alert", ALERT_ID);
    fields.put("entity", PROVIDER_ID);
    fields.put("active", "true");

    when(sRedisTemplate.hGetAll(service.getKeysBuilder().getAlertKey(AID))).thenReturn(fields);
    when(sequenceUtils.getAid(ALERT_ID)).thenReturn(Optional.of(AID));

    final Optional<Alert> alert = service.getAlert(ALERT_ID);

    verify(sequenceUtils).getAid(ALERT_ID);
    verify(sRedisTemplate).hGetAll(service.getKeysBuilder().getAlertKey(AID));
    Assert.assertEquals(PROVIDER_ID, alert.get().getEntity());
  }

  @Test
  public void getAlert_from_no_valid_aid() {
    when(sequenceUtils.getAid(ALERT_ID)).thenReturn(Optional.of(AID));
    when(sRedisTemplate.hGetAll(service.getKeysBuilder().getAlertKey(AID))).thenReturn(Collections.<String, String>emptyMap());

    final Optional<Alert> alert = service.getAlert(ALERT_ID);

    verify(sRedisTemplate).hGetAll(service.getKeysBuilder().getAlertKey(AID));
    Assert.assertFalse(alert.isPresent());
  }
}
