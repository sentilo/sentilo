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
package org.sentilo.agent.activity.monitor.test.service;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.agent.activity.monitor.domain.CatalogAdditionalFields;
import org.sentilo.agent.activity.monitor.service.impl.CatalogServiceImpl;
import org.springframework.data.mongodb.core.MongoOperations;
import org.springframework.data.mongodb.core.query.Query;

public class CatalogServiceImplTest {

  @InjectMocks
  private CatalogServiceImpl catalogService;
  @Mock
  private MongoOperations mongoOperations;
  @Mock
  private CatalogAdditionalFields additionalFields;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void getSensorType() {
    final String sensor = "mockSensor1";
    final String provider = "mockProvider1";
    final String sensorType = "temperature";
    when(mongoOperations.findOne(any(Query.class), eq(CatalogAdditionalFields.class), eq("sensor"))).thenReturn(additionalFields);
    when(additionalFields.getType()).thenReturn(sensorType);

    final String result = catalogService.getSensorType(provider, sensor);

    verify(mongoOperations).findOne(any(Query.class), eq(CatalogAdditionalFields.class), eq("sensor"));
    assertEquals(sensorType, result);

  }

  @Test
  public void getSensorTypeFromCache() {
    final String sensor = "mockSensor1";
    final String provider = "mockProvider1";
    final String sensorType = "temperature";
    when(mongoOperations.findOne(any(Query.class), eq(CatalogAdditionalFields.class), eq("sensor"))).thenReturn(additionalFields);
    when(additionalFields.getType()).thenReturn(sensorType);

    final String result = catalogService.getSensorType(provider, sensor);
    // The second call must retrieve the sensor type from cache rather than MongoDB
    final String result2 = catalogService.getSensorType(provider, sensor);

    verify(mongoOperations, times(1)).findOne(any(Query.class), eq(CatalogAdditionalFields.class), eq("sensor"));
    assertEquals(sensorType, result);
    assertEquals(result2, result);

  }

  @Test
  public void getAlarmLocation() {
    final String alertId = "mockAlert1";
    final String componentId = "mockProvider1.mockComponent1";
    final String location = "41.21345 2.12345";
    when(mongoOperations.findOne(any(Query.class), eq(CatalogAdditionalFields.class), eq("alert"))).thenReturn(additionalFields);
    when(mongoOperations.findOne(any(Query.class), eq(CatalogAdditionalFields.class), eq("component"))).thenReturn(additionalFields);
    when(additionalFields.getComponentId()).thenReturn(componentId);
    when(additionalFields.getLocation()).thenReturn(location);

    final CatalogAdditionalFields result = catalogService.getAdditionalFields(alertId);

    verify(mongoOperations).findOne(any(Query.class), eq(CatalogAdditionalFields.class), eq("alert"));
    verify(mongoOperations).findOne(any(Query.class), eq(CatalogAdditionalFields.class), eq("component"));
    assertEquals(location, result.getLocation());
  }

  @Test
  public void getAlarmLocationFromCache() {
    final String alertId = "mockAlert1";
    final String componentId = "mockProvider1.mockComponent1";
    final String location = "41.21345 2.12345";
    when(mongoOperations.findOne(any(Query.class), eq(CatalogAdditionalFields.class), eq("alert"))).thenReturn(additionalFields);
    when(mongoOperations.findOne(any(Query.class), eq(CatalogAdditionalFields.class), eq("component"))).thenReturn(additionalFields);
    when(additionalFields.getComponentId()).thenReturn(componentId);
    when(additionalFields.getLocation()).thenReturn(location);

    final CatalogAdditionalFields result = catalogService.getAdditionalFields(alertId);
    // The second call must retrieve the location from the cache rather than MongoDB
    final CatalogAdditionalFields result2 = catalogService.getAdditionalFields(alertId);

    verify(mongoOperations, times(1)).findOne(any(Query.class), eq(CatalogAdditionalFields.class), eq("alert"));
    verify(mongoOperations, times(1)).findOne(any(Query.class), eq(CatalogAdditionalFields.class), eq("component"));
    assertEquals(location, result.getLocation());
    assertEquals(result2, result);
  }

  @Test
  public void getAlarmLocationForMobileComponent() {
    final String alertId = "mockAlert1";
    final String componentId = "mockProvider1.mockComponent1";
    final String location = "41.21345 2.12345";
    when(mongoOperations.findOne(any(Query.class), eq(CatalogAdditionalFields.class), eq("alert"))).thenReturn(additionalFields);
    when(mongoOperations.findOne(any(Query.class), eq(CatalogAdditionalFields.class), eq("component"))).thenReturn(additionalFields);
    when(additionalFields.getComponentId()).thenReturn(componentId);
    when(additionalFields.getLocation()).thenReturn(location);
    when(additionalFields.getMobile()).thenReturn(1);

    final CatalogAdditionalFields result = catalogService.getAdditionalFields(alertId);
    // As component is mobile, its location is not stored in the cache and it should be retrieved
    // from MongoDB
    final CatalogAdditionalFields result2 = catalogService.getAdditionalFields(alertId);

    verify(mongoOperations, times(2)).findOne(any(Query.class), eq(CatalogAdditionalFields.class), eq("alert"));
    verify(mongoOperations, times(2)).findOne(any(Query.class), eq(CatalogAdditionalFields.class), eq("component"));
    assertNull(result);
    assertEquals(result2, result);
  }

  @Test
  public void getAlarmLocationForExternalAlert() {
    final String alertId = "mockExternalAlert1";

    when(mongoOperations.findOne(any(Query.class), eq(CatalogAdditionalFields.class), eq("alert"))).thenReturn(null);

    final CatalogAdditionalFields result = catalogService.getAdditionalFields(alertId);

    // As external alerts has not a component associated with them, then they have not any location.
    verify(mongoOperations).findOne(any(Query.class), eq(CatalogAdditionalFields.class), eq("alert"));
    verify(mongoOperations, times(0)).findOne(any(Query.class), eq(CatalogAdditionalFields.class), eq("component"));
    assertNull(result);
  }
}
