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
package org.sentilo.web.catalog.test.admin.service;

import static org.mockito.Matchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Collections;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.rest.RESTClient;
import org.sentilo.web.catalog.admin.service.impl.FederatedSynchronizationServiceImpl;
import org.sentilo.web.catalog.domain.FederationConfig;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.search.SearchFilterResult;
import org.sentilo.web.catalog.service.ComponentService;
import org.sentilo.web.catalog.service.FederationConfigService;
import org.sentilo.web.catalog.service.ProviderService;
import org.sentilo.web.catalog.service.SensorService;

public class FederatedSynchronizationServiceImplTest {

  @InjectMocks
  private FederatedSynchronizationServiceImpl service;

  @Mock
  private FederationConfigService fcService;

  @Mock
  private ProviderService providerService;

  @Mock
  private ComponentService componentService;

  @Mock
  private SensorService sensorService;

  @Mock
  private RESTClient restClient;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void nothing_to_sync() {
    when(fcService.search(any(SearchFilter.class))).thenReturn(new SearchFilterResult<FederationConfig>(Collections.emptyList()));

    service.syncCatalogs();

    verify(fcService, times(0)).update(any(FederationConfig.class));
  }

  @Test
  public void syncCatalogs() {
    final FederationConfig fc = Mockito.mock(FederationConfig.class);
    when(fcService.search(any(SearchFilter.class))).thenReturn(new SearchFilterResult<FederationConfig>(Collections.singletonList(fc)));

    service.syncCatalogs();

    verify(fcService, times(0)).update(any(FederationConfig.class));
  }


}
