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
package org.sentilo.web.catalog.test.security.access;

import static org.mockito.Matchers.any;
import static org.mockito.Mockito.when;

import java.util.Set;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;
import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.web.catalog.domain.Alert;
import org.sentilo.web.catalog.domain.CatalogDocument;
import org.sentilo.web.catalog.domain.ComponentType;
import org.sentilo.web.catalog.domain.Provider;
import org.sentilo.web.catalog.domain.SensorType;
import org.sentilo.web.catalog.domain.Tenant;
import org.sentilo.web.catalog.domain.User;
import org.sentilo.web.catalog.exception.NotAllowedActionException;
import org.sentilo.web.catalog.security.CatalogUserDetails;
import org.sentilo.web.catalog.security.access.AccessControlContext;
import org.sentilo.web.catalog.security.access.impl.AccessControlRepositoryImpl;
import org.sentilo.web.catalog.security.access.impl.AccessControlServiceImpl;
import org.sentilo.web.catalog.security.enums.ActionType;
import org.sentilo.web.catalog.security.service.CatalogUserDetailsService;
import org.sentilo.web.catalog.service.CrudService;
import org.springframework.security.core.context.SecurityContextHolder;

public class AccessControlServiceImplTest {

  private static final String USER_TENANT_ID = "userTenant";

  @InjectMocks
  private AccessControlServiceImpl accessControlService;

  @Mock
  private CatalogUserDetailsService userDetailsService;

  @Mock
  private CatalogUserDetails userDetails;

  @Mock
  private CrudService<CatalogDocument> service;

  @Mock
  private Set<String> tenantsAuth;

  @Spy
  private AccessControlRepositoryImpl accessControlRepository;

  @Before
  public void setUp() {
    System.setProperty(SentiloConstants.SENTILO_MULTITENANT_PROP_KEY, Boolean.TRUE.toString());
    MockitoAnnotations.initMocks(this);
    when(userDetailsService.getCatalogUserDetails()).thenReturn(userDetails);
    when(userDetails.getTenantId()).thenReturn(USER_TENANT_ID);
    when(userDetails.isSuperAdminUser()).thenReturn(false);
    when(userDetails.isAdminUser()).thenReturn(false);
    when(userDetails.isUser()).thenReturn(false);

  }

  @After
  public void tearDown() {
    System.clearProperty(SentiloConstants.SENTILO_MULTITENANT_PROP_KEY);
    SecurityContextHolder.clearContext();
  }

  // Tests to check user access to tenants

  @Test
  public void checkSuperAdminAccessToTenant() {
    final Tenant resource = new Tenant("1");
    when(userDetails.isSuperAdminUser()).thenReturn(true);

    final boolean allowedCRUDL = checkAccess(resource, ActionType.LIST) | checkAccess(resource, ActionType.READ)
        | checkAccess(resource, ActionType.CREATE) | checkAccess(resource, ActionType.SAVE) | checkAccess(resource, ActionType.DELETE);

    Assert.assertTrue(allowedCRUDL);
  }

  @Test
  public void checkAdminAccessToTenant() {
    final Tenant resource = new Tenant(USER_TENANT_ID);
    when(userDetails.isAdminUser()).thenReturn(true);

    final boolean allowedRU = checkAccess(resource, ActionType.READ) | checkAccess(resource, ActionType.SAVE);
    final boolean allowedLCD =
        checkAccess(resource, ActionType.LIST) | checkAccess(resource, ActionType.CREATE) | checkAccess(resource, ActionType.DELETE);

    Assert.assertTrue(allowedRU);
    Assert.assertFalse(allowedLCD);
  }

  @Test
  public void checkAdminWithDiffTenantAccessToTenant() {
    final Tenant resource = new Tenant("1");
    when(userDetails.isAdminUser()).thenReturn(true);
    when(service.findAndThrowErrorIfNotExist(any(CatalogDocument.class))).thenReturn(resource);

    final boolean allowedCRUDL = checkAccess(resource, ActionType.LIST) | checkAccess(resource, ActionType.CREATE)
        | checkAccess(resource, ActionType.DELETE) | checkAccess(resource, ActionType.READ) | checkAccess(resource, ActionType.SAVE);

    Assert.assertFalse(allowedCRUDL);
  }

  @Test
  public void checkUserAccessToTenant() {
    final Tenant resource = new Tenant("1");
    when(userDetails.isUser()).thenReturn(true);

    final boolean allowedCRUDL = checkAccess(resource, ActionType.LIST) | checkAccess(resource, ActionType.READ)
        | checkAccess(resource, ActionType.CREATE) | checkAccess(resource, ActionType.SAVE) | checkAccess(resource, ActionType.DELETE);

    Assert.assertFalse(allowedCRUDL);
  }

  // Tests to check user access to users

  @Test
  public void checkSuperAdminAccessToUser() {
    final User resource = new User("1");
    resource.setTenantId(USER_TENANT_ID);
    when(userDetails.isSuperAdminUser()).thenReturn(true);
    when(service.findAndThrowErrorIfNotExist(any(CatalogDocument.class))).thenReturn(resource);

    final boolean allowedCRUDL = checkAccess(resource, ActionType.LIST) | checkAccess(resource, ActionType.READ)
        | checkAccess(resource, ActionType.CREATE) | checkAccess(resource, ActionType.SAVE) | checkAccess(resource, ActionType.DELETE);

    Assert.assertTrue(allowedCRUDL);
  }

  @Test
  public void checkAdminAccessToUser() {
    final User resource = new User("1");
    resource.setTenantId(USER_TENANT_ID);
    when(userDetails.isAdminUser()).thenReturn(true);
    when(service.findAndThrowErrorIfNotExist(any(CatalogDocument.class))).thenReturn(resource);

    final boolean allowedCRUDL = checkAccess(resource, ActionType.LIST) | checkAccess(resource, ActionType.READ)
        | checkAccess(resource, ActionType.CREATE) | checkAccess(resource, ActionType.SAVE) | checkAccess(resource, ActionType.DELETE);

    Assert.assertTrue(allowedCRUDL);
  }

  @Test
  public void checkAdminWithAuthTenantAccessToUser() {
    final User resource = new User("1");
    resource.setTenantId("mockTenant");
    resource.setTenantsAuth(tenantsAuth);
    when(userDetails.isAdminUser()).thenReturn(true);
    when(service.findAndThrowErrorIfNotExist(any(CatalogDocument.class))).thenReturn(resource);
    when(tenantsAuth.contains(USER_TENANT_ID)).thenReturn(true);

    final boolean allowedLC = checkAccess(resource, ActionType.LIST) | checkAccess(resource, ActionType.CREATE);
    final boolean allowedRUD =
        checkAccess(resource, ActionType.READ) | checkAccess(resource, ActionType.SAVE) | checkAccess(resource, ActionType.DELETE);

    Assert.assertTrue(allowedLC);
    Assert.assertFalse(allowedRUD);
  }

  @Test
  public void checkAdminWithNoAuthTenantAccessToUser() {
    final User resource = new User("1");
    resource.setTenantId("mockTenant");
    resource.setTenantsAuth(tenantsAuth);
    when(userDetails.isAdminUser()).thenReturn(true);
    when(service.findAndThrowErrorIfNotExist(any(CatalogDocument.class))).thenReturn(resource);
    when(tenantsAuth.contains(USER_TENANT_ID)).thenReturn(false);

    final boolean allowedLC = checkAccess(resource, ActionType.LIST) | checkAccess(resource, ActionType.CREATE);
    final boolean allowedRUD =
        checkAccess(resource, ActionType.READ) | checkAccess(resource, ActionType.SAVE) | checkAccess(resource, ActionType.DELETE);

    Assert.assertTrue(allowedLC);
    Assert.assertFalse(allowedRUD);
  }

  @Test
  public void checkUserAccessToUserFromOtherTenant() {
    final User resource = new User("1");
    when(userDetails.isUser()).thenReturn(true);
    when(userDetails.getTenantId()).thenReturn("anotherTenantId");
    when(service.findAndThrowErrorIfNotExist(any(CatalogDocument.class))).thenReturn(resource);

    final boolean allowedCRDUL = checkAccess(resource, ActionType.LIST) | checkAccess(resource, ActionType.READ)
        | checkAccess(resource, ActionType.CREATE) | checkAccess(resource, ActionType.SAVE) | checkAccess(resource, ActionType.DELETE);

    Assert.assertFalse(allowedCRDUL);
  }

  @Test
  public void checkUserAccessToEditItsData() {
    final User resource = new User("1");
    resource.setTenantId(USER_TENANT_ID);
    when(userDetails.isUser()).thenReturn(true);
    when(userDetails.getUsername()).thenReturn(resource.getUserName());
    when(service.findAndThrowErrorIfNotExist(any(CatalogDocument.class))).thenReturn(resource);

    final boolean allowedCDL =
        checkAccess(resource, ActionType.LIST) | checkAccess(resource, ActionType.CREATE) | checkAccess(resource, ActionType.DELETE);

    final boolean allowedRU = checkAccess(resource, ActionType.READ) | checkAccess(resource, ActionType.SAVE);

    Assert.assertFalse(allowedCDL);
    Assert.assertTrue(allowedRU);
  }

  // Tests to check user access to tenants resources (application, provider, component and sensor)
  @Test
  public void checkSuperAdminAccessToTenantResource() {
    final Provider resource = new Provider("1");
    resource.setTenantId(USER_TENANT_ID);
    when(userDetails.isSuperAdminUser()).thenReturn(true);
    when(service.findAndThrowErrorIfNotExist(any(CatalogDocument.class))).thenReturn(resource);

    final boolean allowedCRUDL = checkAccess(resource, ActionType.LIST) | checkAccess(resource, ActionType.READ)
        | checkAccess(resource, ActionType.CREATE) | checkAccess(resource, ActionType.SAVE) | checkAccess(resource, ActionType.DELETE);

    Assert.assertFalse(allowedCRUDL);
  }

  @Test
  public void checkAdminAccessToTenantResource() {
    final Provider resource = new Provider("1");
    resource.setTenantId(USER_TENANT_ID);
    when(userDetails.isAdminUser()).thenReturn(true);
    when(service.findAndThrowErrorIfNotExist(any(CatalogDocument.class))).thenReturn(resource);

    final boolean allowedCRUDL = checkAccess(resource, ActionType.LIST) | checkAccess(resource, ActionType.READ)
        | checkAccess(resource, ActionType.CREATE) | checkAccess(resource, ActionType.SAVE) | checkAccess(resource, ActionType.DELETE);

    Assert.assertTrue(allowedCRUDL);
  }

  @Test
  public void checkAdminWithAuthTenantAccessToTenantResource() {
    final Provider resource = new Provider("1");
    resource.setTenantId("mockTenant");
    resource.setTenantsAuth(tenantsAuth);
    when(userDetails.isAdminUser()).thenReturn(true);
    when(service.findAndThrowErrorIfNotExist(any(CatalogDocument.class))).thenReturn(resource);
    when(tenantsAuth.contains(USER_TENANT_ID)).thenReturn(true);

    final boolean allowedUD = checkAccess(resource, ActionType.SAVE) | checkAccess(resource, ActionType.DELETE);
    final boolean allowedCRL =
        checkAccess(resource, ActionType.CREATE) | checkAccess(resource, ActionType.LIST) | checkAccess(resource, ActionType.READ);

    Assert.assertFalse(allowedUD);
    Assert.assertTrue(allowedCRL);
  }

  @Test
  public void checkAdminWithNoAuthTenantAccessToTenantResource() {
    final Provider resource = new Provider("1");
    resource.setTenantId("mockTenant");
    resource.setTenantsAuth(tenantsAuth);
    when(userDetails.isAdminUser()).thenReturn(true);
    when(service.findAndThrowErrorIfNotExist(any(CatalogDocument.class))).thenReturn(resource);
    when(tenantsAuth.contains(USER_TENANT_ID)).thenReturn(false);

    final boolean allowedRUD =
        checkAccess(resource, ActionType.READ) | checkAccess(resource, ActionType.SAVE) | checkAccess(resource, ActionType.DELETE);
    final boolean allowedCL = checkAccess(resource, ActionType.CREATE) | checkAccess(resource, ActionType.LIST);

    Assert.assertFalse(allowedRUD);
    Assert.assertTrue(allowedCL);
  }

  @Test
  public void checkUserAccessToTenantResource() {
    final Provider resource = new Provider("1");
    resource.setTenantId(USER_TENANT_ID);
    when(userDetails.isUser()).thenReturn(true);
    when(service.findAndThrowErrorIfNotExist(any(CatalogDocument.class))).thenReturn(resource);

    final boolean allowedCUD =
        checkAccess(resource, ActionType.CREATE) | checkAccess(resource, ActionType.SAVE) | checkAccess(resource, ActionType.DELETE);
    final boolean allowedLR = checkAccess(resource, ActionType.LIST) | checkAccess(resource, ActionType.READ);

    Assert.assertFalse(allowedCUD);
    Assert.assertTrue(allowedLR);
  }

  @Test
  public void checkUserWithAuthTenantAccessToTenantResource() {
    final Provider resource = new Provider("1");
    resource.setTenantId("mockTenant");
    resource.setTenantsAuth(tenantsAuth);
    when(userDetails.isUser()).thenReturn(true);
    when(service.findAndThrowErrorIfNotExist(any(CatalogDocument.class))).thenReturn(resource);
    when(tenantsAuth.contains(USER_TENANT_ID)).thenReturn(true);

    final boolean allowedCUD =
        checkAccess(resource, ActionType.CREATE) | checkAccess(resource, ActionType.SAVE) | checkAccess(resource, ActionType.DELETE);
    final boolean allowedLR = checkAccess(resource, ActionType.LIST) | checkAccess(resource, ActionType.READ);

    Assert.assertFalse(allowedCUD);
    Assert.assertTrue(allowedLR);
  }

  @Test
  public void checkUserWithNoAuthTenantAccessToTenantResource() {
    final Provider resource = new Provider("1");
    resource.setTenantId("mockTenant");
    resource.setTenantsAuth(tenantsAuth);
    when(userDetails.isUser()).thenReturn(true);
    when(service.findAndThrowErrorIfNotExist(any(CatalogDocument.class))).thenReturn(resource);
    when(tenantsAuth.contains(USER_TENANT_ID)).thenReturn(false);

    final boolean allowedCRUD = checkAccess(resource, ActionType.CREATE) | checkAccess(resource, ActionType.READ)
        | checkAccess(resource, ActionType.SAVE) | checkAccess(resource, ActionType.DELETE);
    final boolean allowedL = checkAccess(resource, ActionType.LIST);

    Assert.assertFalse(allowedCRUD);
    Assert.assertTrue(allowedL);
  }

  // Tests to check user access to alerts
  @Test
  public void checkSuperAdminAccessToAlert() {
    final Alert resource = new Alert("1");
    resource.setTenantId(USER_TENANT_ID);
    when(userDetails.isSuperAdminUser()).thenReturn(true);
    when(service.findAndThrowErrorIfNotExist(any(CatalogDocument.class))).thenReturn(resource);

    final boolean allowedCRUDL = checkAccess(resource, ActionType.LIST) | checkAccess(resource, ActionType.READ)
        | checkAccess(resource, ActionType.CREATE) | checkAccess(resource, ActionType.SAVE) | checkAccess(resource, ActionType.DELETE);

    Assert.assertFalse(allowedCRUDL);
  }

  @Test
  public void checkAdminAccessToAlert() {
    final Alert resource = new Alert("1");
    resource.setTenantId(USER_TENANT_ID);
    when(userDetails.isAdminUser()).thenReturn(true);
    when(service.findAndThrowErrorIfNotExist(any(CatalogDocument.class))).thenReturn(resource);

    final boolean allowedCRUDL = checkAccess(resource, ActionType.LIST) | checkAccess(resource, ActionType.READ)
        | checkAccess(resource, ActionType.CREATE) | checkAccess(resource, ActionType.SAVE) | checkAccess(resource, ActionType.DELETE);

    Assert.assertTrue(allowedCRUDL);
  }

  @Test
  public void checkAdminWithAuthTenantAccessToAlert() {
    final Alert resource = new Alert("1");
    resource.setTenantId("mockTenant");
    when(userDetails.isAdminUser()).thenReturn(true);
    when(service.findAndThrowErrorIfNotExist(any(CatalogDocument.class))).thenReturn(resource);
    when(tenantsAuth.contains(USER_TENANT_ID)).thenReturn(true);

    final boolean allowedRUD =
        checkAccess(resource, ActionType.READ) | checkAccess(resource, ActionType.SAVE) | checkAccess(resource, ActionType.DELETE);
    final boolean allowedCL = checkAccess(resource, ActionType.CREATE) | checkAccess(resource, ActionType.LIST);

    Assert.assertFalse(allowedRUD);
    Assert.assertTrue(allowedCL);
  }

  @Test
  public void checkAdminWithNoAuthTenantAccessToAlert() {
    final Alert resource = new Alert("1");
    resource.setTenantId("mockTenant");
    when(userDetails.isAdminUser()).thenReturn(true);
    when(service.findAndThrowErrorIfNotExist(any(CatalogDocument.class))).thenReturn(resource);
    when(tenantsAuth.contains(USER_TENANT_ID)).thenReturn(false);

    final boolean allowedRUD =
        checkAccess(resource, ActionType.READ) | checkAccess(resource, ActionType.SAVE) | checkAccess(resource, ActionType.DELETE);
    final boolean allowedCL = checkAccess(resource, ActionType.CREATE) | checkAccess(resource, ActionType.LIST);

    Assert.assertFalse(allowedRUD);
    Assert.assertTrue(allowedCL);
  }

  @Test
  public void checkUserAccessToAlert() {
    final Alert resource = new Alert("1");
    resource.setTenantId(USER_TENANT_ID);
    when(userDetails.isUser()).thenReturn(true);
    when(service.findAndThrowErrorIfNotExist(any(CatalogDocument.class))).thenReturn(resource);

    final boolean allowedCUD =
        checkAccess(resource, ActionType.CREATE) | checkAccess(resource, ActionType.SAVE) | checkAccess(resource, ActionType.DELETE);
    final boolean allowedRL = checkAccess(resource, ActionType.READ) | checkAccess(resource, ActionType.LIST);

    Assert.assertFalse(allowedCUD);
    Assert.assertTrue(allowedRL);
  }

  @Test
  public void checkUserWithAuthTenantAccessToAlert() {
    final Alert resource = new Alert("1");
    resource.setTenantId("mockTenant");
    when(userDetails.isUser()).thenReturn(true);
    when(service.findAndThrowErrorIfNotExist(any(CatalogDocument.class))).thenReturn(resource);
    when(tenantsAuth.contains(USER_TENANT_ID)).thenReturn(true);

    final boolean allowedCRUD = checkAccess(resource, ActionType.CREATE) | checkAccess(resource, ActionType.READ)
        | checkAccess(resource, ActionType.SAVE) | checkAccess(resource, ActionType.DELETE);
    final boolean allowedL = checkAccess(resource, ActionType.LIST);

    Assert.assertFalse(allowedCRUD);
    Assert.assertTrue(allowedL);
  }

  @Test
  public void checkUserWithNoAuthTenantAccessToAlert() {
    final Alert resource = new Alert("1");
    resource.setTenantId("mockTenant");
    when(userDetails.isUser()).thenReturn(true);
    when(service.findAndThrowErrorIfNotExist(any(CatalogDocument.class))).thenReturn(resource);
    when(tenantsAuth.contains(USER_TENANT_ID)).thenReturn(false);

    final boolean allowedCRUD = checkAccess(resource, ActionType.CREATE) | checkAccess(resource, ActionType.READ)
        | checkAccess(resource, ActionType.SAVE) | checkAccess(resource, ActionType.DELETE);
    final boolean allowedL = checkAccess(resource, ActionType.LIST);

    Assert.assertFalse(allowedCRUD);
    Assert.assertTrue(allowedL);
  }

  // Tests to check user access to sensor types
  @Test
  public void checkSuperAdminAccessToSensorType() {
    final SensorType resource = new SensorType("1");
    when(userDetails.isSuperAdminUser()).thenReturn(true);
    when(service.findAndThrowErrorIfNotExist(any(CatalogDocument.class))).thenReturn(resource);

    final boolean allowedCRUL = checkAccess(resource, ActionType.LIST) | checkAccess(resource, ActionType.READ)
        | checkAccess(resource, ActionType.CREATE) | checkAccess(resource, ActionType.SAVE);

    Assert.assertTrue(allowedCRUL);
  }

  @Test
  public void checkAdminAccessToSensorType() {
    final SensorType resource = new SensorType("1");
    when(userDetails.isAdminUser()).thenReturn(true);
    when(service.findAndThrowErrorIfNotExist(any(CatalogDocument.class))).thenReturn(resource);

    final boolean allowedCU = checkAccess(resource, ActionType.CREATE) | checkAccess(resource, ActionType.SAVE);
    final boolean allowedLR = checkAccess(resource, ActionType.LIST) | checkAccess(resource, ActionType.READ);

    Assert.assertFalse(allowedCU);
    Assert.assertTrue(allowedLR);
  }

  @Test
  public void checkUserAccessToSensorType() {
    final SensorType resource = new SensorType("1");
    when(userDetails.isUser()).thenReturn(true);
    when(service.findAndThrowErrorIfNotExist(any(CatalogDocument.class))).thenReturn(resource);

    final boolean allowedCU = checkAccess(resource, ActionType.CREATE) | checkAccess(resource, ActionType.SAVE);
    final boolean allowedLR = checkAccess(resource, ActionType.LIST) | checkAccess(resource, ActionType.READ);

    Assert.assertFalse(allowedCU);
    Assert.assertTrue(allowedLR);
  }

  // Tests to check user access to component types
  @Test
  public void checkSuperAdminAccessToComponentType() {
    final ComponentType resource = new ComponentType("1");
    when(userDetails.isSuperAdminUser()).thenReturn(true);
    when(service.findAndThrowErrorIfNotExist(any(CatalogDocument.class))).thenReturn(resource);

    final boolean allowedCRUL = checkAccess(resource, ActionType.LIST) | checkAccess(resource, ActionType.READ)
        | checkAccess(resource, ActionType.CREATE) | checkAccess(resource, ActionType.SAVE);

    Assert.assertTrue(allowedCRUL);
  }

  @Test
  public void checkAdminAccessToComponentType() {
    final ComponentType resource = new ComponentType("1");
    when(userDetails.isAdminUser()).thenReturn(true);
    when(service.findAndThrowErrorIfNotExist(any(CatalogDocument.class))).thenReturn(resource);

    final boolean allowedCU = checkAccess(resource, ActionType.CREATE) | checkAccess(resource, ActionType.SAVE);
    final boolean allowedLR = checkAccess(resource, ActionType.LIST) | checkAccess(resource, ActionType.READ);

    Assert.assertFalse(allowedCU);
    Assert.assertTrue(allowedLR);
  }

  @Test
  public void checkUserAccessToComponentType() {
    final ComponentType resource = new ComponentType("1");
    when(userDetails.isUser()).thenReturn(true);
    when(service.findAndThrowErrorIfNotExist(any(CatalogDocument.class))).thenReturn(resource);

    final boolean allowedCU = checkAccess(resource, ActionType.CREATE) | checkAccess(resource, ActionType.SAVE);
    final boolean allowedLR = checkAccess(resource, ActionType.LIST) | checkAccess(resource, ActionType.READ);

    Assert.assertFalse(allowedCU);
    Assert.assertTrue(allowedLR);
  }

  private boolean checkAccess(final CatalogDocument resource, final ActionType action) {
    boolean allowed = true;
    try {
      accessControlService.checkAccess(new AccessControlContext(resource, action, service));
    } catch (final NotAllowedActionException naae) {
      allowed = false;
    }

    return allowed;
  }

}
