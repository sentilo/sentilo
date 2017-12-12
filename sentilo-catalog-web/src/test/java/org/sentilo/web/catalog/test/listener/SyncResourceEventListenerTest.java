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
package org.sentilo.web.catalog.test.listener;

import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.enums.SensorState;
import org.sentilo.web.catalog.domain.Alert;
import org.sentilo.web.catalog.domain.Sensor;
import org.sentilo.web.catalog.listener.SyncResourceEventListener;
import org.sentilo.web.catalog.utils.Constants;
import org.springframework.data.mongodb.core.MongoOperations;

import com.mongodb.DBObject;

public class SyncResourceEventListenerTest {

  @Mock
  private MongoOperations mongoOps;

  @Mock
  private DBObject dbo;

  @InjectMocks
  private SyncResourceEventListener listener;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);

  }

  @Test
  public void onBeforeSaveNewSensor() {
    final String sensorId = "prov1.comp1.sensor1";
    final Sensor sensor = new Sensor(sensorId);

    when(mongoOps.findById(sensor.getId(), Sensor.class)).thenReturn(null);

    listener.onBeforeSave(sensor, dbo);

    verify(mongoOps).findById(sensorId, Sensor.class);
    verify(dbo).put(Constants.SYNC_FIELD, null);
  }

  @Test
  public void onBeforeSaveSensor() {
    final String sensorId = "prov1.comp1.sensor1";
    final Sensor sensor = new Sensor(sensorId);
    final Sensor currentSensor = new Sensor(sensorId);
    sensor.setState(SensorState.offline);
    currentSensor.setState(SensorState.online);

    when(mongoOps.findById(sensor.getId(), Sensor.class)).thenReturn(currentSensor);

    listener.onBeforeSave(sensor, dbo);

    verify(mongoOps).findById(sensorId, Sensor.class);
    verify(dbo).put(Constants.SYNC_FIELD, null);
  }

  @Test
  public void onBeforeSaveSensorWithEqState() {
    final String sensorId = "prov1.comp1.sensor1";
    final Sensor sensor = new Sensor(sensorId);
    final Sensor currentSensor = new Sensor(sensorId);
    sensor.setState(SensorState.offline);
    currentSensor.setState(SensorState.offline);

    when(mongoOps.findById(sensor.getId(), Sensor.class)).thenReturn(currentSensor);

    listener.onBeforeSave(sensor, dbo);

    verify(mongoOps).findById(sensorId, sensor.getClass());
    verify(dbo, times(0)).put(Constants.SYNC_FIELD, null);
  }

  @Test
  public void onBeforeSaveNewAlert() {
    final String alertId = "alert1";
    final Alert alert = new Alert(alertId);

    when(mongoOps.findById(alert.getId(), Alert.class)).thenReturn(null);

    listener.onBeforeSave(alert, dbo);

    verify(mongoOps).findById(alertId, Alert.class);
    verify(dbo).put(Constants.SYNC_FIELD, null);
  }

  @Test
  public void onBeforeSaveAlert() {
    final String alertId = "alert1";
    final Alert alert = new Alert(alertId);
    final Alert currentAlert = new Alert(alertId);
    alert.setActive(true);
    currentAlert.setActive(false);

    when(mongoOps.findById(alert.getId(), Alert.class)).thenReturn(currentAlert);

    listener.onBeforeSave(alert, dbo);

    verify(mongoOps).findById(alertId, Alert.class);
    verify(dbo).put(Constants.SYNC_FIELD, null);
  }

  @Test
  public void onBeforeSaveAlertWithEqState() {
    final String alertId = "alert1";
    final Alert alert = new Alert(alertId);
    final Alert currentAlert = new Alert(alertId);
    alert.setActive(true);
    currentAlert.setActive(true);

    when(mongoOps.findById(alert.getId(), Alert.class)).thenReturn(currentAlert);

    listener.onBeforeSave(alert, dbo);

    verify(mongoOps).findById(alertId, Alert.class);

    verify(dbo, times(0)).put(Constants.SYNC_FIELD, null);
  }

}
