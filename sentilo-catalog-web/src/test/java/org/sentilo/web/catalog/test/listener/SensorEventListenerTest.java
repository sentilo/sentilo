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

import static org.mockito.Matchers.anyInt;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.enums.SensorState;
import org.sentilo.web.catalog.domain.Sensor;
import org.sentilo.web.catalog.listener.SensorEventListener;
import org.sentilo.web.catalog.service.PlatformService;
import org.sentilo.web.catalog.service.SensorService;
import org.springframework.data.mongodb.core.mapping.event.BeforeConvertEvent;

public class SensorEventListenerTest {

  @Mock
  private SensorService sensorService;

  @Mock
  private PlatformService platformService;

  @Mock
  private BeforeConvertEvent<Sensor> eventConvert;

  @Mock
  private Sensor sensor;

  @InjectMocks
  private SensorEventListener listener;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    when(eventConvert.getSource()).thenReturn(sensor);
  }

  @Test
  public void checkStateChange_when_state_changes() {
    final Sensor bdSensor = Mockito.mock(Sensor.class);
    when(sensorService.find(sensor)).thenReturn(bdSensor);
    when(sensor.getState()).thenReturn(SensorState.online);
    when(bdSensor.getState()).thenReturn(SensorState.offline);

    listener.onBeforeConvert(eventConvert);

    verify(sensorService).notifyStateChange(sensor);
  }

  @Test
  public void checkStateChange_when_state_equals() {
    final Sensor bdSensor = Mockito.mock(Sensor.class);
    when(sensorService.find(sensor)).thenReturn(bdSensor);
    when(sensor.getState()).thenReturn(SensorState.online);
    when(bdSensor.getState()).thenReturn(SensorState.online);

    listener.onBeforeConvert(eventConvert);

    verify(sensorService, times(0)).notifyStateChange(bdSensor);
  }

  @Test
  public void checkTtl_when_ttl_null() {
    final Integer default_ttl = new Integer(10);
    when(platformService.getPlatformTtl()).thenReturn(default_ttl);
    when(sensor.getTtl()).thenReturn(null);

    listener.onBeforeConvert(eventConvert);

    verify(sensor).setTtl(default_ttl);
  }

  @Test
  public void checkTtl_when_ttl_not_null() {
    final Integer sensor_ttl = new Integer(10);
    when(sensor.getTtl()).thenReturn(sensor_ttl);

    listener.onBeforeConvert(eventConvert);

    verify(sensor, times(0)).setTtl(anyInt());
  }
}
