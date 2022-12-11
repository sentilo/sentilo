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
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.enums.SensorState;
import org.sentilo.platform.client.core.PlatformTemplate;
import org.sentilo.web.catalog.domain.Sensor;
import org.sentilo.web.catalog.event.SensorsStateChangeEvent;
import org.sentilo.web.catalog.repository.SensorRepository;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.security.CatalogUserDetails;
import org.sentilo.web.catalog.security.service.CatalogUserDetailsService;
import org.sentilo.web.catalog.service.AlertService;
import org.sentilo.web.catalog.service.impl.SensorServiceImpl;
import org.sentilo.web.catalog.validator.ResourceKeyValidator;
import org.springframework.context.ApplicationContext;
import org.springframework.data.mongodb.core.MongoOperations;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.data.mongodb.core.query.Update;

public class SensorServiceImplTest {

  @InjectMocks
  private SensorServiceImpl sensorService;

  @Mock
  private CatalogUserDetailsService userDetailsService;

  @Mock
  private CatalogUserDetails catalogUserDetails;

  @Mock
  private SensorRepository repository;

  @Mock
  private PlatformTemplate platformTemplate;

  @Mock
  private AlertService alertService;

  @Mock
  private MongoOperations mongoOperations;

  @Mock
  private ApplicationContext context;

  @Mock
  private SearchFilter filter;

  @Mock
  private Query query;

  @Mock
  private Sensor sensor;

  @Mock
  private ResourceKeyValidator resourceKeyValidator;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    when(userDetailsService.getCatalogUserDetails()).thenReturn(catalogUserDetails);
  }

  @Test
  public void notifyChangeState() {
    final String[] sensorsIds = {"mockProvider.mockComponent.mockSensor"};
    sensorService.changeState(sensorsIds, SensorState.online, null);

    verify(mongoOperations, times(1)).updateMulti(any(Query.class), any(Update.class), eq(Sensor.class));
    verify(context, times(1)).publishEvent(any(SensorsStateChangeEvent.class));
  }
}
