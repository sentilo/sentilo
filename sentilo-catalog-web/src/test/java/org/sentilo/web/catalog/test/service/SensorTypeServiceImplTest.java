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
package org.sentilo.web.catalog.test.service;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.List;

import org.bson.Document;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.sentilo.web.catalog.domain.Component;
import org.sentilo.web.catalog.domain.Sensor;
import org.sentilo.web.catalog.domain.SensorType;
import org.sentilo.web.catalog.repository.SensorTypesRepository;
import org.sentilo.web.catalog.service.impl.SensorTypesServiceImpl;
import org.springframework.data.mongodb.core.MongoOperations;
import org.springframework.data.mongodb.core.query.Query;

import com.mongodb.client.DistinctIterable;
import com.mongodb.client.MongoCollection;
import com.mongodb.client.MongoCursor;

public class SensorTypeServiceImplTest {

  @InjectMocks
  private SensorTypesServiceImpl service;

  @Mock
  private SensorTypesRepository repository;

  @Mock
  private MongoOperations mongoOps;

  @Mock
  private MongoCollection<Document> dbCollection;

  @Mock
  private Query query;

  @Mock
  private Sensor sensor;

  @Mock
  private SensorType sensorType1;

  @Mock
  private SensorType sensorType2;

  @Mock
  private SensorType sensorType3;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void getComponentsTypeFromNullProvider() {

    final List<SensorType> expectedSensorTypesList = Arrays.asList(sensorType1, sensorType2, sensorType3);
    when(repository.findAll()).thenReturn(expectedSensorTypesList);

    final List<SensorType> sensorTypesList = service.findSensorTypesByProvider(null);

    verify(repository).findAll();

    Assert.assertFalse("Expected sensor types list is empty", expectedSensorTypesList.isEmpty());
    Assert.assertEquals("Expected sensor types list is not correct", expectedSensorTypesList, sensorTypesList);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void getComponentsTypeFromProvider() {

    final String providerId = "privederTestId";
    final String type1 = "type1";
    final String type2 = "type2";
    final String type3 = "type3";
    final String sensorCollectionName = Component.class.getName();
    // final List<String> typeIdsList = Arrays.asList(sensorType1.getId(), sensorType3.getId());
    final List<SensorType> expectedSensorTypesList = Arrays.asList(sensorType1, sensorType3);
    final DistinctIterable<String> di = Mockito.mock(DistinctIterable.class);
    final MongoCursor<String> mc = Mockito.mock(MongoCursor.class);

    when(sensorType1.getId()).thenReturn(type1);
    when(sensorType2.getId()).thenReturn(type2);
    when(sensorType3.getId()).thenReturn(type3);
    when(mc.hasNext()).thenReturn(Boolean.TRUE, Boolean.TRUE, Boolean.FALSE);
    when(mongoOps.getCollectionName(Sensor.class)).thenReturn(sensorCollectionName);
    when(mongoOps.getCollection(sensorCollectionName)).thenReturn(dbCollection);
    when(dbCollection.distinct(any(String.class), any(Document.class), eq(String.class))).thenReturn(di);
    when(mongoOps.find(any(Query.class), eq(SensorType.class))).thenReturn(expectedSensorTypesList);
    when(di.iterator()).thenReturn(mc);

    final List<SensorType> sensorTypesList = service.findSensorTypesByProvider(providerId);

    verify(mongoOps).getCollectionName(Sensor.class);
    verify(mongoOps).getCollection(sensorCollectionName);
    verify(mongoOps).find(any(Query.class), eq(SensorType.class));

    Assert.assertFalse("Expected sensor types list is empty", expectedSensorTypesList.isEmpty());
    Assert.assertEquals("Expected sensor types list is not correct", expectedSensorTypesList, sensorTypesList);
  }

}
