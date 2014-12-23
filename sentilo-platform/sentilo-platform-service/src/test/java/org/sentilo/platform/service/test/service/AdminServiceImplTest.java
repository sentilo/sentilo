/*
 * Sentilo
 * 
 * Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de Barcelona.
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

import static org.junit.Assert.assertEquals;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Random;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.domain.CatalogProvider;
import org.sentilo.common.domain.CatalogSensor;
import org.sentilo.platform.common.domain.AdminInputMessage;
import org.sentilo.platform.common.domain.Statistics;
import org.sentilo.platform.common.service.ResourceService;
import org.sentilo.platform.common.service.SubscribeService;
import org.sentilo.platform.service.dao.JedisSequenceUtils;
import org.sentilo.platform.service.impl.AdminServiceImpl;

public class AdminServiceImplTest {

  private static final String PROVIDER_ID = "provider1";
  private static final String SENSOR_ID = "sensor1";

  @Mock
  private JedisSequenceUtils jedisSequenceUtils;
  @Mock
  private ResourceService resourceService;
  @Mock
  private AdminInputMessage message;
  @Mock
  private SubscribeService subscribeService;
  @InjectMocks
  private AdminServiceImpl service;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void getStatistics() {
    final long alarms = 2;
    final long orders = 10;
    final long observations = 120;

    final long events = alarms + orders + observations;

    when(jedisSequenceUtils.getCurrentAmid()).thenReturn(alarms);
    when(jedisSequenceUtils.getCurrentSoid()).thenReturn(orders);
    when(jedisSequenceUtils.getCurrentSdid()).thenReturn(observations);

    final Statistics stats = service.getStatistics();

    verify(jedisSequenceUtils).getCurrentAmid();
    verify(jedisSequenceUtils).getCurrentSdid();
    verify(jedisSequenceUtils).getCurrentSoid();

    assertEquals(new Long(events), stats.getEvents().getTotal());
    assertEquals(new Long(alarms), stats.getEvents().getAlarms());
    assertEquals(new Long(orders), stats.getEvents().getOrders());
    assertEquals(new Long(observations), stats.getEvents().getObservations());

  }

  @Test
  public void deleteProviders() {
    final List<CatalogProvider> providers = buildMockProviderList();
    when(message.getProviders()).thenReturn(providers);

    service.delete(message);

    verify(message, times(2)).getProviders();
    verify(message, times(0)).getSensors();
    verify(resourceService, times(providers.size())).removeProvider(anyString());
  }

  @Test
  public void deleteSensors() {
    final List<CatalogSensor> sensors = buildMockSensorList();
    when(message.getProviders()).thenReturn(Collections.<CatalogProvider>emptyList());
    when(message.getSensors()).thenReturn(sensors);

    service.delete(message);

    verify(message, times(2)).getSensors();
    verify(message, times(1)).getProviders();
    verify(resourceService, times(sensors.size())).removeSensor(anyString(), anyString());
  }

  private List<CatalogProvider> buildMockProviderList() {
    final List<CatalogProvider> mockList = new ArrayList<CatalogProvider>();

    for (int i = 0; i < getRandomSize(); i++) {
      final CatalogProvider provider = new CatalogProvider();
      provider.setProvider(PROVIDER_ID);
      mockList.add(provider);
    }

    return mockList;
  }

  private List<CatalogSensor> buildMockSensorList() {
    final List<CatalogSensor> mockList = new ArrayList<CatalogSensor>();

    for (int i = 0; i < getRandomSize(); i++) {
      final CatalogSensor sensor = new CatalogSensor();
      sensor.setProvider(PROVIDER_ID);
      sensor.setSensor(SENSOR_ID);
      mockList.add(sensor);
    }

    return mockList;
  }

  private int getRandomSize() {
    final Random randomGenerator = new Random();
    int size = 0;
    do {
      size = randomGenerator.nextInt(5);
    } while (size == 0);

    return size;
  }
}
