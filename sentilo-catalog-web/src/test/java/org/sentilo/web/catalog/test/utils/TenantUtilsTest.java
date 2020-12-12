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

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.HashSet;
import java.util.Set;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.web.catalog.context.TenantContextHolder;
import org.sentilo.web.catalog.context.TenantContextImpl;
import org.sentilo.web.catalog.domain.Component;
import org.sentilo.web.catalog.domain.Sensor;
import org.sentilo.web.catalog.domain.SensorType;
import org.sentilo.web.catalog.domain.Tenant;
import org.sentilo.web.catalog.domain.User;
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

  @After
  public void tearDown() {
    System.clearProperty(SentiloConstants.SENTILO_MULTITENANT_PROP_KEY);
    TenantContextHolder.clearContext();
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

  @Test
  public void initTenantFields() {
    final Set<String> tenantsAuth = new HashSet<String>();
    final Set<String> tenantsListVisible = new HashSet<String>();
    when(tenantResource.getTenantsAuth()).thenReturn(tenantsAuth);
    when(tenantResource.getTenantsListVisible()).thenReturn(tenantsListVisible);

    TenantUtils.initTenantFields(tenantResource, mockTenant);

    verify(tenantResource).setTenantId(mockTenant);
    Assert.assertTrue(tenantsAuth.contains(mockTenant));
    Assert.assertTrue(tenantsListVisible.contains(mockTenant));
  }

  @Test
  public void initComponentTenantFields() {
    final Component component = Mockito.mock(Component.class);
    final Set<String> tenantsAuth = new HashSet<String>();
    final Set<String> tenantsListVisible = new HashSet<String>();
    final Set<String> tenantsMapVisible = new HashSet<String>();
    when(component.getTenantsAuth()).thenReturn(tenantsAuth);
    when(component.getTenantsListVisible()).thenReturn(tenantsListVisible);
    when(component.getTenantsMapVisible()).thenReturn(tenantsMapVisible);

    TenantUtils.initTenantFields(component, mockTenant);

    verify(component).setTenantId(mockTenant);
    Assert.assertTrue(tenantsAuth.contains(mockTenant));
    Assert.assertTrue(tenantsListVisible.contains(mockTenant));
    Assert.assertTrue(tenantsMapVisible.contains(mockTenant));
  }

  @Test
  public void initUserTenantFields_whenUserIsSuperAdmin() {
    final User user = Mockito.mock(User.class);
    final Set<String> tenantsAuth = new HashSet<String>();
    final Set<String> tenantsListVisible = new HashSet<String>();
    when(user.getTenantsAuth()).thenReturn(tenantsAuth);
    when(user.getTenantsListVisible()).thenReturn(tenantsListVisible);
    when(user.getTenantId()).thenReturn(mockTenant);

    TenantUtils.initTenantFields(user, null);

    Assert.assertEquals(mockTenant, user.getTenantId());
    Assert.assertTrue(tenantsAuth.contains(mockTenant));
    Assert.assertTrue(tenantsListVisible.contains(mockTenant));
  }
}
