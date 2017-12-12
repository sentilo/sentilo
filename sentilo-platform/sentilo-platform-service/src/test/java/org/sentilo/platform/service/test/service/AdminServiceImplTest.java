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

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.domain.CatalogAlert;
import org.sentilo.common.domain.CatalogProvider;
import org.sentilo.common.domain.CatalogSensor;
import org.sentilo.common.enums.SensorState;
import org.sentilo.platform.common.domain.AdminInputMessage;
import org.sentilo.platform.common.service.ResourceService;
import org.sentilo.platform.common.service.SubscribeService;
import org.sentilo.platform.service.impl.AdminServiceImpl;

public class AdminServiceImplTest {

  private static final String PROVIDER_ID = "provider1";
  private static final String SENSOR_ID = "sensor1";
  private static final String ALERT_ID = "alert1";

  @Mock
  private ResourceService resourceService;
  @Mock
  private AdminInputMessage message;
  @Mock
  private SubscribeService subscribeService;
  @InjectMocks
  private AdminServiceImpl service;

  @Mock
  private CatalogProvider catalogProvider;
  @Mock
  private CatalogSensor catalogSensor;
  @Mock
  private CatalogAlert catalogAlert;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    when(catalogProvider.getProvider()).thenReturn(PROVIDER_ID);
    when(catalogSensor.getProvider()).thenReturn(PROVIDER_ID);
    when(catalogSensor.getSensor()).thenReturn(SENSOR_ID);
    when(catalogAlert.getId()).thenReturn(ALERT_ID);
    when(catalogAlert.getEntity()).thenReturn(PROVIDER_ID);
  }

  @Test
  public void deleteProviders() {
    final List<CatalogProvider> providers = buildMockList(catalogProvider, 10);
    when(message.getProviders()).thenReturn(providers);

    service.delete(message);

    verify(message, times(2)).getProviders();
    verify(message, times(0)).getSensors();
    verify(resourceService, times(providers.size())).removeProvider(anyString());
  }

  @Test
  public void deleteSensors() {
    final List<CatalogSensor> sensors = buildMockList(catalogSensor, 10);
    when(message.getProviders()).thenReturn(Collections.<CatalogProvider>emptyList());
    when(message.getSensors()).thenReturn(sensors);

    service.delete(message);

    verify(message, times(2)).getSensors();
    verify(message, times(1)).getProviders();
    verify(resourceService, times(sensors.size())).removeSensor(anyString(), anyString());
  }

  @Test
  public void deleteAlerts() {
    final List<CatalogAlert> alerts = buildMockList(catalogAlert, 10);
    when(message.getProviders()).thenReturn(null);
    when(message.getSensors()).thenReturn(null);
    when(message.getAlerts()).thenReturn(alerts);

    service.delete(message);

    verify(message, times(2)).getAlerts();
    verify(message, times(1)).getSensors();
    verify(message, times(1)).getProviders();
    verify(resourceService, times(alerts.size())).removeAlert(catalogAlert);
  }

  @Test
  public void saveSensors() {
    final List<CatalogSensor> sensors = buildMockList(catalogSensor, 10);
    when(message.getSensors()).thenReturn(sensors);

    service.save(message);

    verify(message, times(2)).getSensors();
    verify(resourceService, times(sensors.size())).registerProviderIfNeedBe(anyString());
    verify(resourceService, times(sensors.size())).registerSensorIfNeedBe(anyString(), anyString(), any(SensorState.class), eq(Boolean.TRUE));
  }

  @Test
  public void saveAlerts() {
    final List<CatalogAlert> alerts = buildMockList(catalogAlert, 10);
    when(message.getAlerts()).thenReturn(alerts);

    service.save(message);

    verify(message, times(2)).getAlerts();
    verify(resourceService, times(alerts.size())).registerAlertIfNeedBe(any(CatalogAlert.class), eq(Boolean.TRUE));
  }

  private <T> List<T> buildMockList(final T mockObject, final long total) {
    final List<T> resources = new ArrayList<T>();
    for (int i = 0; i < total; i++) {
      resources.add(mockObject);
    }

    return resources;
  }

}
