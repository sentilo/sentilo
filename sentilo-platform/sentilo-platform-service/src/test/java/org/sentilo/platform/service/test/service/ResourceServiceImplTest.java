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
import java.util.Set;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.domain.CatalogAlert;
import org.sentilo.common.domain.CatalogSensor;
import org.sentilo.common.enums.SensorState;
import org.sentilo.platform.common.domain.Sensor;
import org.sentilo.platform.service.dao.SentiloRedisTemplate;
import org.sentilo.platform.service.dao.SentiloSequenceUtils;
import org.sentilo.platform.service.impl.ResourceServiceImpl;
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
  @InjectMocks
  private ResourceServiceImpl service;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    ReflectionTestUtils.setField(service, "expireSeconds", platformExpireTime);
  }

  @Test
  public void registerNewProvider() {
    when(sequenceUtils.getPid(PROVIDER_ID)).thenReturn(null);
    when(sequenceUtils.setPid(PROVIDER_ID)).thenReturn(PID);

    service.registerProviderIfNeedBe(PROVIDER_ID);

    verify(sequenceUtils).getPid(PROVIDER_ID);
    verify(sequenceUtils).setPid(PROVIDER_ID);
    verify(sRedisTemplate).set(service.getKeysBuilder().getProviderKey(PID), PROVIDER_ID);
    verify(sRedisTemplate).set(service.getKeysBuilder().getReverseProviderKey(PROVIDER_ID), PID.toString());
  }

  @Test
  public void registerProviderThatAlreadeadyExists() {
    when(sequenceUtils.getPid(PROVIDER_ID)).thenReturn(PID);

    service.registerProviderIfNeedBe(PROVIDER_ID);

    verify(sequenceUtils).getPid(PROVIDER_ID);
    verify(sequenceUtils, times(0)).setPid(PROVIDER_ID);
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

    when(sequenceUtils.getSid(PROVIDER_ID, SENSOR_ID)).thenReturn(null);
    when(sequenceUtils.getPid(PROVIDER_ID)).thenReturn(PID);
    when(sequenceUtils.setSid(PROVIDER_ID, SENSOR_ID)).thenReturn(SID);

    service.registerSensorIfNeedBe(sensor, false);

    verify(sequenceUtils).getSid(PROVIDER_ID, SENSOR_ID);
    verify(sequenceUtils).getPid(PROVIDER_ID);
    verify(sequenceUtils).setSid(PROVIDER_ID, SENSOR_ID);
    verify(sRedisTemplate).hmSet(service.getKeysBuilder().getSensorKey(SID), fields);
    verify(sRedisTemplate).sAdd(service.getKeysBuilder().getProviderSensorsKey(PID), SID.toString());
    verify(sRedisTemplate).set(service.getKeysBuilder().getReverseSensorKey(PROVIDER_ID, SENSOR_ID), SID.toString());
  }

  @Test
  public void registerSensorThatAlreadyExists() {
    final CatalogSensor sensor = new CatalogSensor();
    sensor.setProvider(PROVIDER_ID);
    sensor.setSensor(SENSOR_ID);
    sensor.setState(SensorState.online);
    when(sequenceUtils.getSid(PROVIDER_ID, SENSOR_ID)).thenReturn(SID);

    service.registerSensorIfNeedBe(sensor, false);

    verify(sequenceUtils).getSid(PROVIDER_ID, SENSOR_ID);
    verify(sequenceUtils, times(0)).getPid(PROVIDER_ID);
    verify(sequenceUtils, times(0)).setSid(PROVIDER_ID, SENSOR_ID);
    verify(sRedisTemplate, times(0)).hmSet(anyString(), anyMapOf(String.class, String.class));
    verify(sRedisTemplate, times(0)).sAdd(service.getKeysBuilder().getProviderSensorsKey(PID), SID.toString());
    verify(sRedisTemplate, times(0)).sAdd(service.getKeysBuilder().getReverseSensorKey(PROVIDER_ID, SENSOR_ID), SID.toString());
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

    when(sequenceUtils.getSid(PROVIDER_ID, SENSOR_ID)).thenReturn(SID);

    service.registerSensorIfNeedBe(sensor, true);

    verify(sequenceUtils).getSid(PROVIDER_ID, SENSOR_ID);
    verify(sequenceUtils, times(0)).getPid(PROVIDER_ID);
    verify(sequenceUtils, times(0)).setSid(PROVIDER_ID, SENSOR_ID);
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

    when(sequenceUtils.getSid(PROVIDER_ID, SENSOR_ID)).thenReturn(SID);

    service.registerSensorIfNeedBe(sensor, true);

    verify(sequenceUtils).getSid(PROVIDER_ID, SENSOR_ID);
    verify(sequenceUtils, times(0)).getPid(PROVIDER_ID);
    verify(sequenceUtils, times(0)).setSid(PROVIDER_ID, SENSOR_ID);
    verify(sRedisTemplate).hmSet(service.getKeysBuilder().getSensorKey(SID), fields);
    verify(sRedisTemplate, times(0)).sAdd(service.getKeysBuilder().getProviderSensorsKey(PID), SID.toString());
    verify(sRedisTemplate, times(0)).sAdd(service.getKeysBuilder().getReverseSensorKey(PROVIDER_ID, SENSOR_ID), SID.toString());
  }

  @Test
  public void registerGhostSensor() {
    final Sensor sensor = new Sensor(PROVIDER_ID, SENSOR_ID);
    final Map<String, String> fields = new HashMap<String, String>();
    fields.put("provider", PROVIDER_ID);
    fields.put("sensor", SENSOR_ID);
    fields.put("state", SensorState.ghost.name());
    fields.put("ttl", Integer.toString(platformExpireTime));

    when(sequenceUtils.getSid(PROVIDER_ID, SENSOR_ID)).thenReturn(null);
    when(sequenceUtils.getPid(PROVIDER_ID)).thenReturn(PID);
    when(sequenceUtils.setSid(PROVIDER_ID, SENSOR_ID)).thenReturn(SID);

    service.registerGhostSensorIfNeedBe(sensor);

    verify(sequenceUtils).getSid(PROVIDER_ID, SENSOR_ID);
    verify(sequenceUtils).getPid(PROVIDER_ID);
    verify(sequenceUtils).setSid(PROVIDER_ID, SENSOR_ID);
    verify(sRedisTemplate).hmSet(service.getKeysBuilder().getSensorKey(SID), fields);
    verify(sRedisTemplate).sAdd(service.getKeysBuilder().getProviderSensorsKey(PID), SID.toString());
    verify(sRedisTemplate).set(service.getKeysBuilder().getReverseSensorKey(PROVIDER_ID, SENSOR_ID), SID.toString());
    Assert.assertEquals(SID, sensor.getSid());
    Assert.assertEquals(SensorState.ghost, sensor.getState());
  }

  @Test
  public void getSensorsFromProvider() {
    when(sequenceUtils.getPid(PROVIDER_ID)).thenReturn(PID);
    when(sRedisTemplate.sMembers(service.getKeysBuilder().getProviderSensorsKey(PID))).thenReturn(Collections.<String>emptySet());

    final Set<String> members = service.getSensorsFromProvider(PROVIDER_ID);

    verify(sequenceUtils).getPid(PROVIDER_ID);
    verify(sRedisTemplate).sMembers(service.getKeysBuilder().getProviderSensorsKey(PID));
    Assert.assertNotNull(members);
  }

  @Test
  public void getSensorsFromNotRegisteredProvider() {
    when(sequenceUtils.getPid(PROVIDER_ID)).thenReturn(null);

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

    when(sRedisTemplate.hGetAll(service.getKeysBuilder().getSensorKey(SID))).thenReturn(fields);

    final Sensor sensor = service.getSensor(SID);

    verify(sRedisTemplate).hGetAll(service.getKeysBuilder().getSensorKey(SID));
    Assert.assertEquals(PROVIDER_ID, sensor.getProvider());
    Assert.assertEquals(SENSOR_ID, sensor.getSensor());
  }

  @Test
  public void getSensorFromParams() {
    final Map<String, String> fields = new HashMap<String, String>();
    fields.put("provider", PROVIDER_ID);
    fields.put("sensor", SENSOR_ID);

    when(sequenceUtils.getSid(PROVIDER_ID, SENSOR_ID)).thenReturn(SID);
    when(sRedisTemplate.hGetAll(service.getKeysBuilder().getSensorKey(SID))).thenReturn(fields);

    final Sensor sensor = service.getSensor(PROVIDER_ID, SENSOR_ID);

    verify(sequenceUtils).getSid(PROVIDER_ID, SENSOR_ID);
    verify(sRedisTemplate).hGetAll(service.getKeysBuilder().getSensorKey(SID));
    Assert.assertEquals(PROVIDER_ID, sensor.getProvider());
    Assert.assertEquals(SENSOR_ID, sensor.getSensor());
  }

  @Test
  public void getNotRegisteredSensor() {
    when(sRedisTemplate.hGetAll(service.getKeysBuilder().getSensorKey(SID))).thenReturn(Collections.<String, String>emptyMap());

    final Sensor sensor = service.getSensor(SID);

    verify(sRedisTemplate).hGetAll(service.getKeysBuilder().getSensorKey(SID));
    Assert.assertNull(sensor);
  }

  @Test
  public void removeProvider() {
    when(sequenceUtils.getPid(PROVIDER_ID)).thenReturn(PID);
    when(sRedisTemplate.sMembers(service.getKeysBuilder().getProviderSensorsKey(PID))).thenReturn(Collections.<String>emptySet());

    service.removeProvider(PROVIDER_ID);

    verify(sequenceUtils).getPid(PROVIDER_ID);
    verify(sRedisTemplate).del(service.getKeysBuilder().getProviderKey(PID));
    verify(sRedisTemplate).del(service.getKeysBuilder().getReverseProviderKey(PROVIDER_ID));
    verify(sequenceUtils).removePid(PROVIDER_ID);

  }

  @Test
  public void removeSensor() {
    when(sequenceUtils.getSid(PROVIDER_ID, SENSOR_ID)).thenReturn(SID);
    when(sequenceUtils.getPid(PROVIDER_ID)).thenReturn(PID);
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
    when(sequenceUtils.getAid(ALERT_ID)).thenReturn(AID);

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
    when(sequenceUtils.getAid(ALERT_ID)).thenReturn(null);
    when(sequenceUtils.setAid(ALERT_ID)).thenReturn(AID);

    service.registerAlertIfNeedBe(catalogAlert, false);

    verify(sequenceUtils).getAid(ALERT_ID);
    verify(sequenceUtils).setAid(ALERT_ID);
    verify(sRedisTemplate).hmSet(service.getKeysBuilder().getAlertKey(AID), fields);
    verify(sRedisTemplate).set(service.getKeysBuilder().getReverseAlertKey(ALERT_ID), AID.toString());
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
    when(sequenceUtils.getAid(ALERT_ID)).thenReturn(AID);

    service.registerAlertIfNeedBe(catalogAlert, false);

    verify(sequenceUtils).getAid(ALERT_ID);
    verify(sequenceUtils, times(0)).setAid(ALERT_ID);
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
    when(sequenceUtils.getAid(ALERT_ID)).thenReturn(AID);

    service.registerAlertIfNeedBe(catalogAlert, true);

    verify(sequenceUtils).getAid(ALERT_ID);
    verify(sequenceUtils, times(0)).setAid(ALERT_ID);
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
    when(sequenceUtils.getSid(PROVIDER_ID, SENSOR_ID)).thenReturn(SID);

    final SensorState sensorState = service.getSensorState(PROVIDER_ID, SENSOR_ID);
    final SensorState sensorState2 = service.getSensorState(PROVIDER_ID, SENSOR_ID);
    final SensorState sensorState3 = service.getSensorState(PROVIDER_ID, SENSOR_ID);
    final SensorState sensorState4 = service.getSensorState(PROVIDER_ID, SENSOR_ID);

    Assert.assertEquals(SensorState.online, sensorState);
    Assert.assertNull(sensorState2);
    Assert.assertEquals(SensorState.offline, sensorState3);
    Assert.assertEquals(SensorState.ghost, sensorState4);
  }

  @SuppressWarnings("unchecked")
  @Test
  public void isAlertDisabled() {
    final Map<String, String> fields = new HashMap<String, String>();
    fields.put("alert", ALERT_ID);
    fields.put("entity", PROVIDER_ID);
    fields.put("active", "true");

    final Map<String, String> fields2 = new HashMap<String, String>(fields);
    fields2.remove("active");
    final Map<String, String> fields3 = new HashMap<String, String>(fields);
    fields3.put("active", "false");

    when(sRedisTemplate.hGetAll(service.getKeysBuilder().getAlertKey(AID))).thenReturn(fields, fields2, fields3);
    when(sequenceUtils.getAid(ALERT_ID)).thenReturn(AID);

    final boolean disabled = service.isAlertDisabled(ALERT_ID);
    final boolean disabled2 = service.isAlertDisabled(ALERT_ID);
    final boolean disabled3 = service.isAlertDisabled(ALERT_ID);

    Assert.assertFalse(disabled);
    Assert.assertFalse(disabled2);
    Assert.assertTrue(disabled3);
  }

  @Test
  public void existAlert() {
    when(sequenceUtils.getAid(ALERT_ID)).thenReturn(AID, (Long) null);

    final boolean exist = service.existsAlert(ALERT_ID);
    final boolean exist2 = service.existsAlert(ALERT_ID);

    Assert.assertTrue(exist);
    Assert.assertFalse(exist2);
  }
}
