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
package org.sentilo.web.catalog.test.security;

import static org.mockito.Mockito.when;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.web.catalog.context.TenantContextHolder;
import org.sentilo.web.catalog.context.TenantContextImpl;
import org.sentilo.web.catalog.domain.ComponentType;
import org.sentilo.web.catalog.domain.Sensor;
import org.sentilo.web.catalog.domain.SensorType;
import org.sentilo.web.catalog.domain.Tenant;
import org.sentilo.web.catalog.security.CatalogUserDetails;
import org.sentilo.web.catalog.security.SecurityUtils;
import org.sentilo.web.catalog.security.enums.ActionType;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;

public class SecurityUtilsTest {

  private final String mockTenant = "mockTenant";

  @Mock
  private CatalogUserDetails userDetails;

  @Mock
  private Authentication authentication;

  @Mock
  private Sensor tenantResource;

  @Mock
  private Tenant tenant;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    SecurityContextHolder.clearContext();

    SecurityContextHolder.getContext().setAuthentication(authentication);
    when(authentication.getPrincipal()).thenReturn(userDetails);
  }

  @After
  public void tearDown() {
    System.clearProperty(SentiloConstants.SENTILO_MULTITENANT_PROP_KEY);
    SecurityContextHolder.clearContext();
  }

  @Test
  public void userRoleCanViewAdminControls() {
    when(userDetails.isUser()).thenReturn(true);

    Assert.assertFalse(SecurityUtils.showAdminControls(ActionType.LIST, Tenant.class.getName()));
    Assert.assertFalse(SecurityUtils.showAdminControls(ActionType.CREATE, Tenant.class.getName()));
    Assert.assertFalse(SecurityUtils.showAdminControls(ActionType.READ, Tenant.class.getName()));
    Assert.assertFalse(SecurityUtils.showAdminControls(ActionType.EDIT, Tenant.class.getName()));

    Assert.assertFalse(SecurityUtils.showAdminControls(ActionType.LIST, Sensor.class.getName()));
    Assert.assertFalse(SecurityUtils.showAdminControls(ActionType.CREATE, Sensor.class.getName()));
    Assert.assertFalse(SecurityUtils.showAdminControls(ActionType.READ, Sensor.class.getName()));
    Assert.assertFalse(SecurityUtils.showAdminControls(ActionType.EDIT, Sensor.class.getName()));

    Assert.assertFalse(SecurityUtils.showAdminControls(ActionType.LIST, SensorType.class.getName()));
    Assert.assertFalse(SecurityUtils.showAdminControls(ActionType.CREATE, SensorType.class.getName()));
    Assert.assertFalse(SecurityUtils.showAdminControls(ActionType.READ, SensorType.class.getName()));
    Assert.assertFalse(SecurityUtils.showAdminControls(ActionType.EDIT, SensorType.class.getName()));

    Assert.assertFalse(SecurityUtils.showAdminControls(ActionType.LIST, ComponentType.class.getName()));
    Assert.assertFalse(SecurityUtils.showAdminControls(ActionType.CREATE, ComponentType.class.getName()));
    Assert.assertFalse(SecurityUtils.showAdminControls(ActionType.READ, ComponentType.class.getName()));
    Assert.assertFalse(SecurityUtils.showAdminControls(ActionType.EDIT, ComponentType.class.getName()));
  }

  @Test
  public void superAdminRoleCanViewAdminControls() {
    System.setProperty(SentiloConstants.SENTILO_MULTITENANT_PROP_KEY, Boolean.TRUE.toString());
    when(userDetails.isSuperAdminUser()).thenReturn(true);

    Assert.assertFalse(SecurityUtils.showAdminControls(ActionType.LIST, Sensor.class.getName()));
    Assert.assertFalse(SecurityUtils.showAdminControls(ActionType.CREATE, Sensor.class.getName()));
    Assert.assertFalse(SecurityUtils.showAdminControls(ActionType.READ, Sensor.class.getName()));
    Assert.assertFalse(SecurityUtils.showAdminControls(ActionType.EDIT, Sensor.class.getName()));

    Assert.assertTrue(SecurityUtils.showAdminControls(ActionType.LIST, Tenant.class.getName()));
    Assert.assertTrue(SecurityUtils.showAdminControls(ActionType.CREATE, Tenant.class.getName()));
    Assert.assertTrue(SecurityUtils.showAdminControls(ActionType.READ, Tenant.class.getName()));
    Assert.assertTrue(SecurityUtils.showAdminControls(ActionType.EDIT, Tenant.class.getName()));

    Assert.assertTrue(SecurityUtils.showAdminControls(ActionType.LIST, SensorType.class.getName()));
    Assert.assertTrue(SecurityUtils.showAdminControls(ActionType.CREATE, SensorType.class.getName()));
    Assert.assertTrue(SecurityUtils.showAdminControls(ActionType.READ, SensorType.class.getName()));
    Assert.assertTrue(SecurityUtils.showAdminControls(ActionType.EDIT, SensorType.class.getName()));

    Assert.assertTrue(SecurityUtils.showAdminControls(ActionType.LIST, ComponentType.class.getName()));
    Assert.assertTrue(SecurityUtils.showAdminControls(ActionType.CREATE, ComponentType.class.getName()));
    Assert.assertTrue(SecurityUtils.showAdminControls(ActionType.READ, ComponentType.class.getName()));
    Assert.assertTrue(SecurityUtils.showAdminControls(ActionType.EDIT, ComponentType.class.getName()));
  }

  @Test
  public void superAdminRoleCanViewAdminControlsNoMultitenant() {
    System.setProperty(SentiloConstants.SENTILO_MULTITENANT_PROP_KEY, Boolean.FALSE.toString());
    when(userDetails.isSuperAdminUser()).thenReturn(true);

    Assert.assertFalse(SecurityUtils.showAdminControls(ActionType.LIST, Sensor.class.getName()));
    Assert.assertFalse(SecurityUtils.showAdminControls(ActionType.CREATE, Sensor.class.getName()));
    Assert.assertFalse(SecurityUtils.showAdminControls(ActionType.READ, Sensor.class.getName()));
    Assert.assertFalse(SecurityUtils.showAdminControls(ActionType.EDIT, Sensor.class.getName()));

    Assert.assertFalse(SecurityUtils.showAdminControls(ActionType.LIST, Tenant.class.getName()));
    Assert.assertFalse(SecurityUtils.showAdminControls(ActionType.CREATE, Tenant.class.getName()));
    Assert.assertFalse(SecurityUtils.showAdminControls(ActionType.READ, Tenant.class.getName()));
    Assert.assertFalse(SecurityUtils.showAdminControls(ActionType.EDIT, Tenant.class.getName()));

    Assert.assertFalse(SecurityUtils.showAdminControls(ActionType.LIST, SensorType.class.getName()));
    Assert.assertFalse(SecurityUtils.showAdminControls(ActionType.CREATE, SensorType.class.getName()));
    Assert.assertFalse(SecurityUtils.showAdminControls(ActionType.READ, SensorType.class.getName()));
    Assert.assertFalse(SecurityUtils.showAdminControls(ActionType.EDIT, SensorType.class.getName()));

    Assert.assertFalse(SecurityUtils.showAdminControls(ActionType.LIST, ComponentType.class.getName()));
    Assert.assertFalse(SecurityUtils.showAdminControls(ActionType.CREATE, ComponentType.class.getName()));
    Assert.assertFalse(SecurityUtils.showAdminControls(ActionType.READ, ComponentType.class.getName()));
    Assert.assertFalse(SecurityUtils.showAdminControls(ActionType.EDIT, ComponentType.class.getName()));

  }

  @Test
  public void adminRoleCanViewAdminControls() {
    System.setProperty(SentiloConstants.SENTILO_MULTITENANT_PROP_KEY, Boolean.TRUE.toString());
    TenantContextHolder.setContext(new TenantContextImpl(mockTenant));
    when(userDetails.isAdminUser()).thenReturn(true);
    when(tenantResource.getTenantId()).thenReturn(mockTenant);
    when(tenant.getId()).thenReturn(mockTenant);

    Assert.assertTrue(SecurityUtils.showAdminControls(ActionType.READ, tenant));
    Assert.assertTrue(SecurityUtils.showAdminControls(ActionType.EDIT, tenant));
    Assert.assertFalse(SecurityUtils.showAdminControls(ActionType.LIST, tenant));
    Assert.assertFalse(SecurityUtils.showAdminControls(ActionType.CREATE, tenant));

    Assert.assertTrue(SecurityUtils.showAdminControls(ActionType.LIST, tenantResource));
    Assert.assertTrue(SecurityUtils.showAdminControls(ActionType.CREATE, tenantResource));
    Assert.assertTrue(SecurityUtils.showAdminControls(ActionType.READ, tenantResource));
    Assert.assertTrue(SecurityUtils.showAdminControls(ActionType.EDIT, tenantResource));

    Assert.assertFalse(SecurityUtils.showAdminControls(ActionType.LIST, SensorType.class.getName()));
    Assert.assertFalse(SecurityUtils.showAdminControls(ActionType.CREATE, SensorType.class.getName()));
    Assert.assertFalse(SecurityUtils.showAdminControls(ActionType.READ, SensorType.class.getName()));
    Assert.assertFalse(SecurityUtils.showAdminControls(ActionType.EDIT, SensorType.class.getName()));

    Assert.assertFalse(SecurityUtils.showAdminControls(ActionType.LIST, ComponentType.class.getName()));
    Assert.assertFalse(SecurityUtils.showAdminControls(ActionType.CREATE, ComponentType.class.getName()));
    Assert.assertFalse(SecurityUtils.showAdminControls(ActionType.READ, ComponentType.class.getName()));
    Assert.assertFalse(SecurityUtils.showAdminControls(ActionType.EDIT, ComponentType.class.getName()));
  }

  @Test
  public void adminRoleCanViewAdminControlsNoOwnResources() {
    System.setProperty(SentiloConstants.SENTILO_MULTITENANT_PROP_KEY, Boolean.TRUE.toString());
    TenantContextHolder.setContext(new TenantContextImpl(mockTenant));
    when(userDetails.isAdminUser()).thenReturn(true);
    when(tenantResource.getTenantId()).thenReturn("mockTenant2");

    Assert.assertTrue(SecurityUtils.showAdminControls(ActionType.LIST, tenantResource));
    Assert.assertTrue(SecurityUtils.showAdminControls(ActionType.CREATE, tenantResource));
    Assert.assertFalse(SecurityUtils.showAdminControls(ActionType.READ, tenantResource));
    Assert.assertFalse(SecurityUtils.showAdminControls(ActionType.EDIT, tenantResource));
  }

  @Test
  public void adminRoleCanViewAdminControlsNoMultitenant() {
    System.setProperty(SentiloConstants.SENTILO_MULTITENANT_PROP_KEY, Boolean.FALSE.toString());
    when(userDetails.isAdminUser()).thenReturn(true);

    Assert.assertTrue(SecurityUtils.showAdminControls(ActionType.READ, Tenant.class.getName()));
    Assert.assertTrue(SecurityUtils.showAdminControls(ActionType.EDIT, Tenant.class.getName()));
    Assert.assertTrue(SecurityUtils.showAdminControls(ActionType.LIST, Tenant.class.getName()));
    Assert.assertTrue(SecurityUtils.showAdminControls(ActionType.CREATE, Tenant.class.getName()));

    Assert.assertTrue(SecurityUtils.showAdminControls(ActionType.LIST, tenantResource));
    Assert.assertTrue(SecurityUtils.showAdminControls(ActionType.CREATE, tenantResource));
    Assert.assertTrue(SecurityUtils.showAdminControls(ActionType.READ, tenantResource));
    Assert.assertTrue(SecurityUtils.showAdminControls(ActionType.EDIT, tenantResource));

    Assert.assertTrue(SecurityUtils.showAdminControls(ActionType.LIST, SensorType.class.getName()));
    Assert.assertTrue(SecurityUtils.showAdminControls(ActionType.CREATE, SensorType.class.getName()));
    Assert.assertTrue(SecurityUtils.showAdminControls(ActionType.READ, SensorType.class.getName()));
    Assert.assertTrue(SecurityUtils.showAdminControls(ActionType.EDIT, SensorType.class.getName()));

    Assert.assertTrue(SecurityUtils.showAdminControls(ActionType.LIST, ComponentType.class.getName()));
    Assert.assertTrue(SecurityUtils.showAdminControls(ActionType.CREATE, ComponentType.class.getName()));
    Assert.assertTrue(SecurityUtils.showAdminControls(ActionType.READ, ComponentType.class.getName()));
    Assert.assertTrue(SecurityUtils.showAdminControls(ActionType.EDIT, ComponentType.class.getName()));
  }
}
