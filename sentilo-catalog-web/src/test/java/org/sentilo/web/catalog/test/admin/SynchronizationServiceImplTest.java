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
package org.sentilo.web.catalog.test.admin;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.exception.RESTClientException;
import org.sentilo.web.catalog.admin.service.impl.SynchronizationServiceImpl;
import org.sentilo.web.catalog.domain.Alert;
import org.sentilo.web.catalog.domain.PlatformAdminInputMessage;
import org.sentilo.web.catalog.domain.Sensor;
import org.sentilo.web.catalog.service.PlatformService;
import org.springframework.data.mongodb.core.MongoOperations;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.data.mongodb.core.query.Update;
import org.springframework.test.util.ReflectionTestUtils;

public class SynchronizationServiceImplTest {

  @InjectMocks
  private SynchronizationServiceImpl service;

  @Mock
  private MongoOperations mongoOps;

  @Mock
  private PlatformService platformService;

  @Mock
  private Sensor sensor;

  @Mock
  private Alert alert;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void syncSensorsMetadata() {
    final long count = 10l;
    final String collectionName = "sensor";
    when(mongoOps.getCollectionName(eq(Sensor.class))).thenReturn(collectionName);
    when(mongoOps.count(any(Query.class), eq(Sensor.class))).thenReturn(count);
    when(mongoOps.find(any(Query.class), eq(Sensor.class), eq(collectionName))).thenReturn(buildMockList(sensor, count));

    service.syncSensorsMetadata();

    final boolean syncSensorsIsRunning = (Boolean) ReflectionTestUtils.getField(service, "syncSensorsIsRunning");
    Assert.assertFalse(syncSensorsIsRunning);
    verify(mongoOps).count(any(Query.class), eq(Sensor.class));
    verify(platformService).saveResources(any(PlatformAdminInputMessage.class));
    verify(mongoOps).updateMulti(any(Query.class), any(Update.class), eq(sensor.getClass()));
  }

  @Test
  public void syncSensorsMetadataWithPages() {
    final long count = 120l;
    final List<Sensor> resources = buildMockList(sensor, count);
    final String collectionName = "sensor";

    when(mongoOps.getCollectionName(eq(Sensor.class))).thenReturn(collectionName);
    when(mongoOps.count(any(Query.class), eq(Sensor.class))).thenReturn(count);
    when(mongoOps.find(any(Query.class), eq(Sensor.class), eq(collectionName))).thenReturn(resources.subList(0, 100))
        .thenReturn(resources.subList(100, 120));

    service.syncSensorsMetadata();

    final boolean syncSensorsIsRunning = (Boolean) ReflectionTestUtils.getField(service, "syncSensorsIsRunning");
    Assert.assertFalse(syncSensorsIsRunning);
    verify(mongoOps).count(any(Query.class), eq(Sensor.class));
    verify(platformService, times(2)).saveResources(any(PlatformAdminInputMessage.class));
    verify(mongoOps, times(2)).updateMulti(any(Query.class), any(Update.class), eq(sensor.getClass()));
  }

  @Test
  public void syncSensorsMetadataWithError() {
    final long count = 10l;
    final String collectionName = "sensor";
    when(mongoOps.getCollectionName(eq(Sensor.class))).thenReturn(collectionName);
    when(mongoOps.count(any(Query.class), eq(Sensor.class))).thenReturn(count);
    when(mongoOps.find(any(Query.class), eq(Sensor.class), eq(collectionName))).thenReturn(buildMockList(sensor, count));
    doThrow(RESTClientException.class).doNothing().when(platformService).saveResources(any(PlatformAdminInputMessage.class));

    service.syncSensorsMetadata();

    final boolean syncSensorsIsRunning = (Boolean) ReflectionTestUtils.getField(service, "syncSensorsIsRunning");
    Assert.assertFalse(syncSensorsIsRunning);
    verify(mongoOps).count(any(Query.class), eq(Sensor.class));
    verify(platformService, times(2)).saveResources(any(PlatformAdminInputMessage.class));
    verify(mongoOps, times(2)).updateMulti(any(Query.class), any(Update.class), eq(sensor.getClass()));
  }

  @Test
  public void concurrentSyncSensorsMetadata() {
    ReflectionTestUtils.setField(service, "syncSensorsIsRunning", Boolean.TRUE);

    service.syncSensorsMetadata();

    final boolean syncSensorsIsRunning = (Boolean) ReflectionTestUtils.getField(service, "syncSensorsIsRunning");
    Assert.assertTrue(syncSensorsIsRunning);
    verify(mongoOps, times(0)).count(any(Query.class), eq(Sensor.class));
  }

  @Test
  public void syncAlertsMetadata() {
    final long count = 10l;
    final String collectionName = "alert";
    when(mongoOps.getCollectionName(eq(Alert.class))).thenReturn(collectionName);
    when(mongoOps.count(any(Query.class), eq(Alert.class))).thenReturn(count);
    when(mongoOps.find(any(Query.class), eq(Alert.class), eq(collectionName))).thenReturn(buildMockList(alert, count));

    service.syncAlertsMetadata();

    final boolean syncAlertsIsRunning = (Boolean) ReflectionTestUtils.getField(service, "syncAlertsIsRunning");
    Assert.assertFalse(syncAlertsIsRunning);
    verify(mongoOps).count(any(Query.class), eq(Alert.class));
    verify(platformService).saveResources(any(PlatformAdminInputMessage.class));
    verify(mongoOps).updateMulti(any(Query.class), any(Update.class), eq(alert.getClass()));
  }

  @Test
  public void syncAlertsMetadataWithPages() {
    final long count = 120l;
    final List<Alert> resources = buildMockList(alert, count);
    final String collectionName = "alert";
    when(mongoOps.getCollectionName(eq(Alert.class))).thenReturn(collectionName);
    when(mongoOps.count(any(Query.class), eq(Alert.class))).thenReturn(count);
    when(mongoOps.find(any(Query.class), eq(Alert.class), eq(collectionName))).thenReturn(resources.subList(0, 100))
        .thenReturn(resources.subList(100, 120));

    service.syncAlertsMetadata();

    final boolean syncAlertsIsRunning = (Boolean) ReflectionTestUtils.getField(service, "syncAlertsIsRunning");
    Assert.assertFalse(syncAlertsIsRunning);
    verify(mongoOps).count(any(Query.class), eq(Alert.class));
    verify(platformService, times(2)).saveResources(any(PlatformAdminInputMessage.class));
    verify(mongoOps, times(2)).updateMulti(any(Query.class), any(Update.class), eq(alert.getClass()));
  }

  @Test
  public void syncAlertsMetadataWithError() {
    final long count = 10l;
    final String collectionName = "alert";
    when(mongoOps.getCollectionName(eq(Alert.class))).thenReturn(collectionName);
    when(mongoOps.count(any(Query.class), eq(Alert.class))).thenReturn(count);
    when(mongoOps.find(any(Query.class), eq(Alert.class), eq(collectionName))).thenReturn(buildMockList(alert, count));
    doThrow(RESTClientException.class).doNothing().when(platformService).saveResources(any(PlatformAdminInputMessage.class));

    service.syncAlertsMetadata();

    final boolean syncAlertsIsRunning = (Boolean) ReflectionTestUtils.getField(service, "syncAlertsIsRunning");
    Assert.assertFalse(syncAlertsIsRunning);
    verify(mongoOps).count(any(Query.class), eq(Alert.class));
    verify(platformService, times(2)).saveResources(any(PlatformAdminInputMessage.class));
    verify(mongoOps, times(2)).updateMulti(any(Query.class), any(Update.class), eq(alert.getClass()));
  }

  @Test
  public void concurrentSyncAlertsMetadata() {
    ReflectionTestUtils.setField(service, "syncAlertsIsRunning", Boolean.TRUE);

    service.syncAlertsMetadata();

    final boolean syncSensorsIsRunning = (Boolean) ReflectionTestUtils.getField(service, "syncAlertsIsRunning");
    Assert.assertTrue(syncSensorsIsRunning);
    verify(mongoOps, times(0)).count(any(Query.class), eq(alert.getClass()));
  }

  private <T> List<T> buildMockList(final T mockDocument, final long total) {
    final List<T> resources = new ArrayList<T>();
    for (int i = 0; i < total; i++) {
      resources.add(mockDocument);
    }

    return resources;
  }

}
