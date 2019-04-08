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
package org.sentilo.web.catalog.test.validator;

import static org.mockito.Matchers.any;
import static org.mockito.Mockito.when;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.web.catalog.context.TenantContextHolder;
import org.sentilo.web.catalog.context.TenantContextImpl;
import org.sentilo.web.catalog.domain.Application;
import org.sentilo.web.catalog.domain.Permission;
import org.sentilo.web.catalog.domain.Provider;
import org.sentilo.web.catalog.domain.TenantPermission;
import org.sentilo.web.catalog.dto.PermissionsDTO;
import org.sentilo.web.catalog.service.ApplicationService;
import org.sentilo.web.catalog.service.ProviderService;
import org.sentilo.web.catalog.service.TenantPermissionService;
import org.sentilo.web.catalog.validator.PermissionsValidator;
import org.springframework.validation.BeanPropertyBindingResult;
import org.springframework.validation.Errors;

public class PermissionsValidatorTest {

  @Mock
  private ApplicationService applicationService;

  @Mock
  private ProviderService providerService;

  @Mock
  private PermissionsDTO permissions;

  @Mock
  private TenantPermissionService tenantPermissionService;

  @Mock
  private TenantPermission tenantPermission;

  @Mock
  private Application application;

  @Mock
  private Provider provider;

  @InjectMocks
  private PermissionsValidator validator;

  private final String currentTenant = "mockTenant";

  private final String applicationId = "applicationId";

  private final String providerId = "providerId";

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    System.setProperty(SentiloConstants.SENTILO_MULTITENANT_PROP_KEY, Boolean.TRUE.toString());
    TenantContextHolder.setContext(new TenantContextImpl(currentTenant));
  }

  @After
  public void tearDown() {
    System.clearProperty(SentiloConstants.SENTILO_MULTITENANT_PROP_KEY);
    TenantContextHolder.clearContext();
  }

  @Test
  public void validateEmptySelectionsPermissionsApplicationsTest() {
    final Errors errors = new BeanPropertyBindingResult(permissions, "permissions");
    when(permissions.getSelectedApplicationsIds()).thenReturn(new String[] {});
    when(permissions.getSelectedProvidersIds()).thenReturn(new String[] {});

    validator.validate(permissions, errors);

    Assert.assertTrue(errors.hasGlobalErrors() && errors.getGlobalError().getCode().equals(PermissionsValidator.ERROR_CODE_EMPTY_SELECTED_IDS));
  }

  @Test
  public void validateAdminPermissionsApplicationsTest() {

    final Errors errors = new BeanPropertyBindingResult(permissions, "permissions");

    when(permissions.getPermissionType()).thenReturn(Permission.Type.ADMIN);
    when(permissions.getSelectedApplicationsIds()).thenReturn(new String[] {applicationId});
    when(permissions.getSelectedProvidersIds()).thenReturn(new String[] {});
    when(applicationService.isApplicationFromTenant(any(String.class), any(String.class))).thenReturn(false);

    validator.validate(permissions, errors);

    Assert.assertTrue(errors.hasGlobalErrors() && errors.getGlobalError().getCode().equals(PermissionsValidator.ERROR_CODE_ADMIN_TYPE));
  }

  @Test
  public void validateAllowedAdminPermissionsApplicationsTest() {

    final Errors errors = new BeanPropertyBindingResult(permissions, "permissions");

    when(permissions.getPermissionType()).thenReturn(Permission.Type.ADMIN);
    when(permissions.getSelectedApplicationsIds()).thenReturn(new String[] {applicationId});
    when(permissions.getSelectedProvidersIds()).thenReturn(new String[] {});
    when(applicationService.isApplicationFromTenant(any(String.class), any(String.class))).thenReturn(true);

    validator.validate(permissions, errors);

    Assert.assertFalse(errors.hasGlobalErrors());
  }

  @Test
  public void validateAdminPermissionsProviderTest() {

    final Errors errors = new BeanPropertyBindingResult(permissions, "permissions");

    when(permissions.getPermissionType()).thenReturn(Permission.Type.ADMIN);
    when(permissions.getSelectedApplicationsIds()).thenReturn(new String[] {});
    when(permissions.getSelectedProvidersIds()).thenReturn(new String[] {providerId});
    when(providerService.isProviderFromTenant(any(String.class), any(String.class))).thenReturn(false);

    validator.validate(permissions, errors);

    Assert.assertTrue(errors.hasGlobalErrors() && errors.getGlobalError().getCode().equals(PermissionsValidator.ERROR_CODE_ADMIN_TYPE));
  }

  @Test
  public void validateAllowedAdminPermissionsProviderTest() {

    final Errors errors = new BeanPropertyBindingResult(permissions, "permissions");

    when(permissions.getPermissionType()).thenReturn(Permission.Type.ADMIN);
    when(permissions.getSelectedApplicationsIds()).thenReturn(new String[] {});
    when(permissions.getSelectedProvidersIds()).thenReturn(new String[] {providerId});
    when(providerService.isProviderFromTenant(any(String.class), any(String.class))).thenReturn(true);

    validator.validate(permissions, errors);

    Assert.assertFalse(errors.hasGlobalErrors());
  }

  @Test
  public void validateWriteApplicationPermissions() {

    final Errors errors = new BeanPropertyBindingResult(permissions, "permissions");

    when(permissions.getPermissionType()).thenReturn(Permission.Type.WRITE);
    when(permissions.getSelectedApplicationsIds()).thenReturn(new String[] {applicationId});
    when(permissions.getSelectedProvidersIds()).thenReturn(new String[] {});
    when(applicationService.findAndThrowErrorIfNotExist(any(Application.class))).thenReturn(application);
    when(application.getTenantId()).thenReturn("mockTenant2");
    when(tenantPermissionService.findFromPermissionsByEntity(any(String.class), any(String.class))).thenReturn(tenantPermission);
    when(tenantPermission.getType()).thenReturn(TenantPermission.Type.READ);

    validator.validate(permissions, errors);

    Assert.assertTrue(errors.hasGlobalErrors() && errors.getGlobalError().getCode().equals(PermissionsValidator.ERROR_CODE_WRITE_TYPE));
  }

  @Test
  public void validateAllowedWriteApplicationPermissions() {

    final Errors errors = new BeanPropertyBindingResult(permissions, "permissions");

    when(permissions.getPermissionType()).thenReturn(Permission.Type.WRITE);
    when(permissions.getSelectedApplicationsIds()).thenReturn(new String[] {applicationId});
    when(permissions.getSelectedProvidersIds()).thenReturn(new String[] {});
    when(applicationService.findAndThrowErrorIfNotExist(any(Application.class))).thenReturn(application);
    when(application.getTenantId()).thenReturn("mockTenant2");
    when(tenantPermissionService.findFromPermissionsByEntity(any(String.class), any(String.class))).thenReturn(tenantPermission);
    when(tenantPermission.getType()).thenReturn(TenantPermission.Type.WRITE);

    validator.validate(permissions, errors);

    Assert.assertFalse(errors.hasGlobalErrors());
  }

  @Test
  public void validateWriteProviderPermissions() {

    final Errors errors = new BeanPropertyBindingResult(permissions, "permissions");

    when(permissions.getPermissionType()).thenReturn(Permission.Type.WRITE);
    when(permissions.getSelectedApplicationsIds()).thenReturn(new String[] {});
    when(permissions.getSelectedProvidersIds()).thenReturn(new String[] {providerId});
    when(providerService.findAndThrowErrorIfNotExist(any(Provider.class))).thenReturn(provider);
    when(provider.getTenantId()).thenReturn("mockTenant2");
    when(tenantPermissionService.findFromPermissionsByEntity(any(String.class), any(String.class))).thenReturn(tenantPermission);
    when(tenantPermission.getType()).thenReturn(TenantPermission.Type.READ);

    validator.validate(permissions, errors);

    Assert.assertTrue(errors.hasGlobalErrors() && errors.getGlobalError().getCode().equals(PermissionsValidator.ERROR_CODE_WRITE_TYPE));
  }

  @Test
  public void validateAllowedWriteProviderPermissions() {

    final Errors errors = new BeanPropertyBindingResult(permissions, "permissions");

    when(permissions.getPermissionType()).thenReturn(Permission.Type.WRITE);
    when(permissions.getSelectedApplicationsIds()).thenReturn(new String[] {});
    when(permissions.getSelectedProvidersIds()).thenReturn(new String[] {providerId});
    when(providerService.findAndThrowErrorIfNotExist(any(Provider.class))).thenReturn(provider);
    when(provider.getTenantId()).thenReturn("mockTenant2");
    when(tenantPermissionService.findFromPermissionsByEntity(any(String.class), any(String.class))).thenReturn(tenantPermission);
    when(tenantPermission.getType()).thenReturn(TenantPermission.Type.WRITE);

    validator.validate(permissions, errors);

    Assert.assertFalse(errors.hasGlobalErrors());
  }

}
