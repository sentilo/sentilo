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

import static org.mockito.Mockito.when;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.test.AbstractBaseTest;
import org.sentilo.web.catalog.domain.TenantPermission;
import org.sentilo.web.catalog.dto.TenantPermissionsDTO;
import org.sentilo.web.catalog.validator.TenantPermissionValidator;
import org.springframework.validation.BeanPropertyBindingResult;
import org.springframework.validation.Errors;

public class TenantPermissionValidatorTest extends AbstractBaseTest {

  @InjectMocks
  private TenantPermissionValidator tenantPermissionValidator;

  @Mock
  private TenantPermissionsDTO tenantPermissionsDTO;

  private final String tenantId = "tenantId";

  private final String providerId = "providerId";

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void supports() {
    Assert.assertFalse(tenantPermissionValidator.supports(TenantPermission.class));
    Assert.assertTrue(tenantPermissionValidator.supports(TenantPermissionsDTO.class));
  }

  @Test
  public void validateNoProviderSelectedTest() {
    final Errors errors = new BeanPropertyBindingResult(tenantPermissionsDTO, "tenantPermissions");

    when(tenantPermissionsDTO.getSelectedProvidersIds()).thenReturn(null);
    when(tenantPermissionsDTO.getSelectedEntitiesIds()).thenReturn(new String[] {tenantId});

    tenantPermissionValidator.validate(tenantPermissionsDTO, errors);

    Assert.assertTrue(errors.hasGlobalErrors() && errors.getGlobalError().getCode().equals(TenantPermissionValidator.ERROR_CODE_EMPTY_SELECTED_IDS));
  }

  @Test
  public void validateNoOrganizationSelectedTest() {
    final Errors errors = new BeanPropertyBindingResult(tenantPermissionsDTO, "tenantPermissions");

    when(tenantPermissionsDTO.getSelectedProvidersIds()).thenReturn(new String[] {providerId});
    when(tenantPermissionsDTO.getSelectedEntitiesIds()).thenReturn(null);

    tenantPermissionValidator.validate(tenantPermissionsDTO, errors);

    Assert.assertTrue(errors.hasGlobalErrors() && errors.getGlobalError().getCode().equals(TenantPermissionValidator.ERROR_CODE_EMPTY_SELECTED_IDS));
  }

  @Test
  public void validateNoneSelectedTest() {
    final Errors errors = new BeanPropertyBindingResult(tenantPermissionsDTO, "tenantPermissions");

    when(tenantPermissionsDTO.getSelectedProvidersIds()).thenReturn(null);
    when(tenantPermissionsDTO.getSelectedEntitiesIds()).thenReturn(null);

    tenantPermissionValidator.validate(tenantPermissionsDTO, errors);

    Assert.assertTrue(errors.hasGlobalErrors() && errors.getGlobalError().getCode().equals(TenantPermissionValidator.ERROR_CODE_EMPTY_SELECTED_IDS));
  }

  @Test
  public void validateSelectedTest() {
    final Errors errors = new BeanPropertyBindingResult(tenantPermissionsDTO, "tenantPermissions");

    when(tenantPermissionsDTO.getSelectedProvidersIds()).thenReturn(new String[] {providerId});
    when(tenantPermissionsDTO.getSelectedEntitiesIds()).thenReturn(new String[] {tenantId});

    tenantPermissionValidator.validate(tenantPermissionsDTO, errors);

    Assert.assertTrue(!errors.hasGlobalErrors());
  }
}
