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
import org.sentilo.common.enums.SensorState;
import org.sentilo.platform.common.domain.Sensor;
import org.sentilo.platform.service.dao.JedisSequenceUtils;
import org.sentilo.platform.service.dao.JedisTemplate;
import org.sentilo.platform.service.impl.ResourceServiceImpl;
import org.springframework.util.CollectionUtils;

public class ResourceServiceImplTest {

  private static final String PROVIDER_ID = "provider1";
  private static final String SENSOR_ID = "sensor1";
  private static final String ALERT_ID = "alert1";
  private static final Long PID = new Long(1);
  private static final Long SID = new Long(1);
  private static final Long AID = new Long(1);

  @Mock
  private CatalogAlert catalogAlert;
  @Mock
  private JedisTemplate<String, String> jedisTemplate;
  @Mock
  private JedisSequenceUtils jedisSequenceUtils;
  @InjectMocks
  private ResourceServiceImpl service;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void registerNewProvider() {
    when(jedisSequenceUtils.getPid(PROVIDER_ID)).thenReturn(null);
    when(jedisSequenceUtils.setPid(PROVIDER_ID)).thenReturn(PID);

    service.registerProviderIfNeedBe(PROVIDER_ID);

    verify(jedisSequenceUtils).getPid(PROVIDER_ID);
    verify(jedisSequenceUtils).setPid(PROVIDER_ID);
    verify(jedisTemplate).set(service.getKeysBuilder().getProviderKey(PID), PROVIDER_ID);
    verify(jedisTemplate).set(service.getKeysBuilder().getReverseProviderKey(PROVIDER_ID), PID.toString());
  }

  @Test
  public void registerProviderThatAlreadeadyExists() {
    when(jedisSequenceUtils.getPid(PROVIDER_ID)).thenReturn(PID);

    service.registerProviderIfNeedBe(PROVIDER_ID);

    verify(jedisSequenceUtils).getPid(PROVIDER_ID);
    verify(jedisSequenceUtils, times(0)).setPid(PROVIDER_ID);
    verify(jedisTemplate, times(0)).set(service.getKeysBuilder().getProviderKey(PID), PROVIDER_ID);
    verify(jedisTemplate, times(0)).set(service.getKeysBuilder().getReverseProviderKey(PROVIDER_ID), PID.toString());
  }

  @Test
  public void registerNewSensor() {
    final Map<String, String> fields = new HashMap<String, String>();
    fields.put("provider", PROVIDER_ID);
    fields.put("sensor", SENSOR_ID);
    fields.put("state", "online");

    when(jedisSequenceUtils.getSid(PROVIDER_ID, SENSOR_ID)).thenReturn(null);
    when(jedisSequenceUtils.getPid(PROVIDER_ID)).thenReturn(PID);
    when(jedisSequenceUtils.setSid(PROVIDER_ID, SENSOR_ID)).thenReturn(SID);

    service.registerSensorIfNeedBe(PROVIDER_ID, SENSOR_ID, SensorState.online, false);

    verify(jedisSequenceUtils).getSid(PROVIDER_ID, SENSOR_ID);
    verify(jedisSequenceUtils).getPid(PROVIDER_ID);
    verify(jedisSequenceUtils).setSid(PROVIDER_ID, SENSOR_ID);
    verify(jedisTemplate).hmSet(service.getKeysBuilder().getSensorKey(SID), fields);
    verify(jedisTemplate).sAdd(service.getKeysBuilder().getProviderSensorsKey(PID), SID.toString());
    verify(jedisTemplate).set(service.getKeysBuilder().getReverseSensorKey(PROVIDER_ID, SENSOR_ID), SID.toString());
  }

  @Test
  public void registerSensorThatAlreadyExists() {
    when(jedisSequenceUtils.getSid(PROVIDER_ID, SENSOR_ID)).thenReturn(SID);

    service.registerSensorIfNeedBe(PROVIDER_ID, SENSOR_ID, SensorState.online, false);

    verify(jedisSequenceUtils).getSid(PROVIDER_ID, SENSOR_ID);
    verify(jedisSequenceUtils, times(0)).getPid(PROVIDER_ID);
    verify(jedisSequenceUtils, times(0)).setSid(PROVIDER_ID, SENSOR_ID);
    verify(jedisTemplate, times(0)).hmSet(anyString(), anyMapOf(String.class, String.class));
    verify(jedisTemplate, times(0)).sAdd(service.getKeysBuilder().getProviderSensorsKey(PID), SID.toString());
    verify(jedisTemplate, times(0)).sAdd(service.getKeysBuilder().getReverseSensorKey(PROVIDER_ID, SENSOR_ID), SID.toString());
  }

  @Test
  public void updateSensorThatAlreadyExists() {
    when(jedisSequenceUtils.getSid(PROVIDER_ID, SENSOR_ID)).thenReturn(SID);

    service.registerSensorIfNeedBe(PROVIDER_ID, SENSOR_ID, SensorState.online, true);

    verify(jedisSequenceUtils).getSid(PROVIDER_ID, SENSOR_ID);
    verify(jedisSequenceUtils, times(0)).getPid(PROVIDER_ID);
    verify(jedisSequenceUtils, times(0)).setSid(PROVIDER_ID, SENSOR_ID);
    verify(jedisTemplate).hmSet(anyString(), anyMapOf(String.class, String.class));
    verify(jedisTemplate, times(0)).sAdd(service.getKeysBuilder().getProviderSensorsKey(PID), SID.toString());
    verify(jedisTemplate, times(0)).sAdd(service.getKeysBuilder().getReverseSensorKey(PROVIDER_ID, SENSOR_ID), SID.toString());
  }

  @Test
  public void getSensorsFromProvider() {
    when(jedisSequenceUtils.getPid(PROVIDER_ID)).thenReturn(PID);
    when(jedisTemplate.sMembers(service.getKeysBuilder().getProviderSensorsKey(PID))).thenReturn(Collections.<String>emptySet());

    final Set<String> members = service.getSensorsFromProvider(PROVIDER_ID);

    verify(jedisSequenceUtils).getPid(PROVIDER_ID);
    verify(jedisTemplate).sMembers(service.getKeysBuilder().getProviderSensorsKey(PID));
    Assert.assertNotNull(members);
  }

  @Test
  public void getSensorsFromNotRegisteredProvider() {
    when(jedisSequenceUtils.getPid(PROVIDER_ID)).thenReturn(null);

    final Set<String> members = service.getSensorsFromProvider(PROVIDER_ID);

    verify(jedisSequenceUtils).getPid(PROVIDER_ID);
    verify(jedisTemplate, times(0)).sMembers(anyString());
    Assert.assertTrue(CollectionUtils.isEmpty(members));
  }

  @Test
  public void getSensor() {
    final Map<String, String> fields = new HashMap<String, String>();
    fields.put("provider", PROVIDER_ID);
    fields.put("sensor", SENSOR_ID);

    when(jedisTemplate.hGetAll(service.getKeysBuilder().getSensorKey(SID))).thenReturn(fields);

    final Sensor sensor = service.getSensor(SID);

    verify(jedisTemplate).hGetAll(service.getKeysBuilder().getSensorKey(SID));
    Assert.assertEquals(PROVIDER_ID, sensor.getProvider());
    Assert.assertEquals(SENSOR_ID, sensor.getSensor());

  }

  @Test
  public void getNotRegisteredSensor() {
    when(jedisTemplate.hGetAll(service.getKeysBuilder().getSensorKey(SID))).thenReturn(Collections.<String, String>emptyMap());

    final Sensor sensor = service.getSensor(SID);

    verify(jedisTemplate).hGetAll(service.getKeysBuilder().getSensorKey(SID));
    Assert.assertNull(sensor);
  }

  @Test
  public void removeProvider() {
    when(jedisSequenceUtils.getPid(PROVIDER_ID)).thenReturn(PID);
    when(jedisTemplate.sMembers(service.getKeysBuilder().getProviderSensorsKey(PID))).thenReturn(Collections.<String>emptySet());

    service.removeProvider(PROVIDER_ID);

    verify(jedisSequenceUtils).getPid(PROVIDER_ID);
    verify(jedisTemplate).del(service.getKeysBuilder().getProviderKey(PID));
    verify(jedisTemplate).del(service.getKeysBuilder().getReverseProviderKey(PROVIDER_ID));
    verify(jedisSequenceUtils).removePid(PROVIDER_ID);

  }

  @Test
  public void removeSensor() {
    when(jedisSequenceUtils.getSid(PROVIDER_ID, SENSOR_ID)).thenReturn(SID);
    when(jedisSequenceUtils.getPid(PROVIDER_ID)).thenReturn(PID);
    when(jedisTemplate.hGet(service.getKeysBuilder().getSensorKey(SID), "sensor")).thenReturn(SENSOR_ID);

    when(jedisTemplate.sMembers(service.getKeysBuilder().getProviderSensorsKey(PID))).thenReturn(Collections.<String>emptySet());

    service.removeSensor(SENSOR_ID, PROVIDER_ID);

    verify(jedisSequenceUtils).getSid(PROVIDER_ID, SENSOR_ID);
    verify(jedisSequenceUtils).getPid(PROVIDER_ID);
    verify(jedisTemplate).del(service.getKeysBuilder().getSensorKey(SID));
    verify(jedisTemplate).sRem(service.getKeysBuilder().getProviderSensorsKey(PID), SID.toString());
    verify(jedisTemplate).del(service.getKeysBuilder().getReverseSensorKey(PROVIDER_ID, SENSOR_ID));
    verify(jedisTemplate).del(service.getKeysBuilder().getSensorObservationsKey(SID));
    verify(jedisTemplate).del(service.getKeysBuilder().getSensorOrdersKey(SID));
    verify(jedisSequenceUtils).removeSid(PROVIDER_ID, SENSOR_ID);
  }

  @Test
  public void removeAlert() {
    when(catalogAlert.getId()).thenReturn(ALERT_ID);
    when(jedisSequenceUtils.getAid(ALERT_ID)).thenReturn(AID);

    service.removeAlert(catalogAlert);

    verify(jedisSequenceUtils).getAid(ALERT_ID);
    verify(jedisTemplate).del(service.getKeysBuilder().getAlertKey(AID));
    verify(jedisTemplate).del(service.getKeysBuilder().getReverseAlertKey(ALERT_ID));
    verify(jedisSequenceUtils).removeAid(ALERT_ID);
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
    when(jedisSequenceUtils.getAid(ALERT_ID)).thenReturn(null);
    when(jedisSequenceUtils.setAid(ALERT_ID)).thenReturn(AID);

    service.registerAlertIfNeedBe(catalogAlert, false);

    verify(jedisSequenceUtils).getAid(ALERT_ID);
    verify(jedisSequenceUtils).setAid(ALERT_ID);
    verify(jedisTemplate).hmSet(service.getKeysBuilder().getAlertKey(AID), fields);
    verify(jedisTemplate).set(service.getKeysBuilder().getReverseAlertKey(ALERT_ID), AID.toString());
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
    when(jedisSequenceUtils.getAid(ALERT_ID)).thenReturn(AID);

    service.registerAlertIfNeedBe(catalogAlert, false);

    verify(jedisSequenceUtils).getAid(ALERT_ID);
    verify(jedisSequenceUtils, times(0)).setAid(ALERT_ID);
    verify(jedisTemplate, times(0)).hmSet(service.getKeysBuilder().getAlertKey(AID), fields);
    verify(jedisTemplate, times(0)).set(service.getKeysBuilder().getReverseAlertKey(ALERT_ID), AID.toString());
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
    when(jedisSequenceUtils.getAid(ALERT_ID)).thenReturn(AID);

    service.registerAlertIfNeedBe(catalogAlert, true);

    verify(jedisSequenceUtils).getAid(ALERT_ID);
    verify(jedisSequenceUtils, times(0)).setAid(ALERT_ID);
    verify(jedisTemplate).hmSet(service.getKeysBuilder().getAlertKey(AID), fields);
    verify(jedisTemplate, times(0)).set(service.getKeysBuilder().getReverseAlertKey(ALERT_ID), AID.toString());
  }

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

    when(jedisTemplate.hGetAll(service.getKeysBuilder().getSensorKey(SID))).thenReturn(fields, fields2, fields3, fields4);
    when(jedisSequenceUtils.getSid(PROVIDER_ID, SENSOR_ID)).thenReturn(SID);

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

    when(jedisTemplate.hGetAll(service.getKeysBuilder().getAlertKey(AID))).thenReturn(fields, fields2, fields3);
    when(jedisSequenceUtils.getAid(ALERT_ID)).thenReturn(AID);

    final boolean disabled = service.isAlertDisabled(ALERT_ID);
    final boolean disabled2 = service.isAlertDisabled(ALERT_ID);
    final boolean disabled3 = service.isAlertDisabled(ALERT_ID);

    Assert.assertFalse(disabled);
    Assert.assertFalse(disabled2);
    Assert.assertTrue(disabled3);
  }

  @Test
  public void existAlert() {
    when(jedisSequenceUtils.getAid(ALERT_ID)).thenReturn(AID, (Long) null);

    final boolean exist = service.existsAlert(ALERT_ID);
    final boolean exist2 = service.existsAlert(ALERT_ID);

    Assert.assertTrue(exist);
    Assert.assertFalse(exist2);
  }
}
