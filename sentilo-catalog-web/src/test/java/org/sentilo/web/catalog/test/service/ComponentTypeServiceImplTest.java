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

import static org.junit.Assert.assertEquals;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.bson.Document;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.sentilo.web.catalog.domain.Component;
import org.sentilo.web.catalog.domain.ComponentType;
import org.sentilo.web.catalog.repository.ComponentTypesRepository;
import org.sentilo.web.catalog.service.impl.ComponentTypesServiceImpl;
import org.springframework.data.mongodb.core.query.Query;

import com.mongodb.client.DistinctIterable;
import com.mongodb.client.MongoCollection;
import com.mongodb.client.MongoCursor;

public class ComponentTypeServiceImplTest extends AbstractBaseCrudServiceImplTest {

  @InjectMocks
  private ComponentTypesServiceImpl service;

  @Mock
  private ComponentTypesRepository repository;

  @Mock
  private MongoCollection<Document> dbCollection;

  @Mock
  private Query query;

  @Mock
  private Component component;

  @Mock
  private ComponentType componentType1;

  @Mock
  private ComponentType componentType2;

  @Mock
  private ComponentType componentType3;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    service.init();
  }

  @Test
  public void getComponentsTypeFromNullProvider() {

    final List<ComponentType> expectedComponentTypesList = Arrays.asList(componentType1, componentType2, componentType3);
    when(repository.findAll()).thenReturn(expectedComponentTypesList);

    final List<ComponentType> componentTypesList = service.findComponentTypesByProvider(null);

    verify(repository).findAll();

    Assert.assertFalse("Expected component types list is empty", expectedComponentTypesList.isEmpty());
    Assert.assertEquals("Expected component types list is not correct", expectedComponentTypesList, componentTypesList);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void getComponentsTypeFromProvider() {

    final String providerId = "privederTestId";
    final String type1 = "type1";
    final String type2 = "type2";
    final String type3 = "type3";
    final String componentCollectionName = Component.class.getName();
    // final List<String> typeIdsList = Arrays.asList(componentType1.getId(),
    // componentType3.getId());
    final List<ComponentType> expectedComponentTypesList = Arrays.asList(componentType1, componentType3);
    final DistinctIterable<String> di = Mockito.mock(DistinctIterable.class);
    final MongoCursor<String> mc = Mockito.mock(MongoCursor.class);

    when(componentType1.getId()).thenReturn(type1);
    when(componentType2.getId()).thenReturn(type2);
    when(componentType3.getId()).thenReturn(type3);
    when(mc.hasNext()).thenReturn(Boolean.TRUE, Boolean.TRUE, Boolean.FALSE);

    when(mongoOps.getCollectionName(Component.class)).thenReturn(componentCollectionName);
    when(mongoOps.getCollection(componentCollectionName)).thenReturn(dbCollection);
    when(dbCollection.distinct(any(String.class), any(Document.class), eq(String.class))).thenReturn(di);
    when(mongoOps.find(any(Query.class), eq(ComponentType.class))).thenReturn(expectedComponentTypesList);
    when(di.iterator()).thenReturn(mc);

    final List<ComponentType> componentTypesList = service.findComponentTypesByProvider(providerId);

    verify(mongoOps).getCollectionName(Component.class);
    verify(mongoOps).getCollection(componentCollectionName);
    verify(mongoOps).find(any(Query.class), eq(ComponentType.class));

    Assert.assertFalse("Expected component types list is empty", expectedComponentTypesList.isEmpty());
    Assert.assertEquals("Expected component types list is not correct", expectedComponentTypesList, componentTypesList);
  }

  @Test
  public void getEntityId() {
    final String componentTypeId = "mockCompTypeId1";
    when(componentType1.getId()).thenReturn(componentTypeId);

    assertEquals(componentTypeId, service.getEntityId(componentType1));
  }

  @Test
  public void insertNewComponentTypesIfNotExists() {
    final Set<String> componentTypes = new HashSet<String>(Arrays.asList("compType1", "compType2"));
    when(repository.existsById("compType1")).thenReturn(false);
    when(repository.existsById("compType2")).thenReturn(true);

    service.insertNewComponentTypesIfNotExists(componentTypes);

    verify(repository).save(eq(new ComponentType("compType1", "compType1")));
    verify(repository, times(0)).save(eq(new ComponentType("compType2", "compType2")));
  }

}
