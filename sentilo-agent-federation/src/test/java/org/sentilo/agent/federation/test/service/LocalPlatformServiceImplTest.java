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
package org.sentilo.agent.federation.test.service;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.List;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.agent.federation.domain.Application;
import org.sentilo.agent.federation.domain.FederationConfig;
import org.sentilo.agent.federation.service.impl.LocalPlatformServiceImpl;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.query.Query;

public class LocalPlatformServiceImplTest {

  private final static String MOCK_TOKEN = "123456789";

  @InjectMocks
  private LocalPlatformServiceImpl localService;

  @Mock
  private MongoTemplate mongoOps;

  @Mock
  private FederationConfig fConfig;

  @Mock
  private Application masterApp;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void getFederatedConfig() {
    final String federatedServerId = "FED1";

    when(fConfig.getId()).thenReturn(federatedServerId);
    when(mongoOps.findOne(any(Query.class), eq(FederationConfig.class))).thenReturn(fConfig);
    when(fConfig.getAppClientToken()).thenReturn(MOCK_TOKEN);

    localService.getFederatedConfig(federatedServerId);
    localService.getFederatedConfig(federatedServerId);

    verify(mongoOps, times(1)).findOne(any(Query.class), eq(FederationConfig.class));
  }

  @Test
  public void getFederatedConfigs() {
    final List<FederationConfig> fConfigs = Arrays.asList(fConfig);
    when(mongoOps.findAll(FederationConfig.class)).thenReturn(fConfigs);

    final List<FederationConfig> result = localService.getFederatedConfigs();

    verify(mongoOps).findAll(eq(FederationConfig.class));
    Assert.assertEquals(fConfigs, result);
  }

  @Test
  public void getTokenMasterApp() {
    when(mongoOps.findOne(any(Query.class), eq(Application.class))).thenReturn(masterApp);
    when(masterApp.getToken()).thenReturn(MOCK_TOKEN);

    final String result1 = localService.getTokenMasterApp();
    final String result2 = localService.getTokenMasterApp();

    Assert.assertEquals(MOCK_TOKEN, result1);
    Assert.assertEquals(MOCK_TOKEN, result2);
    verify(mongoOps, times(1)).findOne(any(Query.class), eq(Application.class));
  }

}
