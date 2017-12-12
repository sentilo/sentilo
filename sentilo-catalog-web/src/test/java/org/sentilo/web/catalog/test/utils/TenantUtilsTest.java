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
package org.sentilo.web.catalog.test.utils;

import static org.mockito.Mockito.when;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.web.catalog.context.TenantContextHolder;
import org.sentilo.web.catalog.context.TenantContextImpl;
import org.sentilo.web.catalog.domain.Sensor;
import org.sentilo.web.catalog.domain.SensorType;
import org.sentilo.web.catalog.domain.Tenant;
import org.sentilo.web.catalog.utils.TenantUtils;

public class TenantUtilsTest {

  private final String mockTenant = "mockTenant";

  @Mock
  private Tenant tenant;

  @Mock
  private Sensor tenantResource;

  @Mock
  private SensorType sensorType;

  @Before
  public void setUp() throws Exception {
    System.setProperty(SentiloConstants.SENTILO_MULTITENANT_PROP_KEY, Boolean.TRUE.toString());
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void getNullCurrentTenant() {
    TenantContextHolder.clearContext();
    Assert.assertNull(TenantUtils.getCurrentTenant());
  }

  @Test
  public void getCurrentTenant() {
    TenantContextHolder.setContext(new TenantContextImpl(mockTenant));
    Assert.assertEquals(mockTenant, TenantUtils.getCurrentTenant());
  }

  @Test
  public void getResourceTenant() {
    when(tenant.getId()).thenReturn(mockTenant);
    when(tenantResource.getTenantId()).thenReturn(mockTenant);
    Assert.assertNull(TenantUtils.getResourceTenantOwner(sensorType));
    Assert.assertEquals(mockTenant, TenantUtils.getResourceTenantOwner(tenant));
    Assert.assertEquals(mockTenant, TenantUtils.getResourceTenantOwner(tenantResource));
  }

  @Test
  public void isCurrentTenantResource() {
    TenantContextHolder.setContext(new TenantContextImpl(mockTenant));
    when(tenant.getId()).thenReturn(mockTenant);
    when(tenantResource.getTenantId()).thenReturn(mockTenant);
    Assert.assertFalse(TenantUtils.isCurrentTenantResource(sensorType));
    Assert.assertTrue(TenantUtils.isCurrentTenantResource(tenant));
    Assert.assertTrue(TenantUtils.isCurrentTenantResource(tenantResource));
  }

}
