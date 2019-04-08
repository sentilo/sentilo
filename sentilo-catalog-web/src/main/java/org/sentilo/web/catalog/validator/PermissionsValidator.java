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
package org.sentilo.web.catalog.validator;

import org.sentilo.common.utils.SentiloUtils;
import org.sentilo.web.catalog.context.TenantContextHolder;
import org.sentilo.web.catalog.domain.Application;
import org.sentilo.web.catalog.domain.Permission;
import org.sentilo.web.catalog.domain.Provider;
import org.sentilo.web.catalog.domain.TenantPermission;
import org.sentilo.web.catalog.domain.TenantResource;
import org.sentilo.web.catalog.dto.PermissionsDTO;
import org.sentilo.web.catalog.service.ApplicationService;
import org.sentilo.web.catalog.service.ProviderService;
import org.sentilo.web.catalog.service.TenantPermissionService;
import org.sentilo.web.catalog.utils.TenantUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

@SentiloValidator
public class PermissionsValidator implements Validator {

  public static final String ERROR_CODE_EMPTY_SELECTED_IDS = "permission.error.emptyIdsLists";
  public static final String ERROR_CODE_WRITE_TYPE = "permission.error.add.writeType";
  public static final String ERROR_CODE_ADMIN_TYPE = "permission.error.add.adminType";

  @Autowired
  private ApplicationService applicationService;

  @Autowired
  private ProviderService providerService;

  @Autowired
  private TenantPermissionService tenantPermissionService;

  @Override
  public boolean supports(final Class<?> clazz) {
    return PermissionsDTO.class.equals(clazz);
  }

  @Override
  public void validate(final Object target, final Errors errors) {
    final PermissionsDTO permissions = (PermissionsDTO) target;

    if (SentiloUtils.arrayIsEmpty(permissions.getSelectedApplicationsIds()) && SentiloUtils.arrayIsEmpty(permissions.getSelectedProvidersIds())) {
      errors.reject(ERROR_CODE_EMPTY_SELECTED_IDS);
      return;
    }

    if (TenantContextHolder.isEnabled()) {
      final Permission.Type type = permissions.getPermissionType();
      switch (type) {
        case ADMIN:
          validateAdminPermissions(permissions, errors);
          break;
        case WRITE:
          validateWritePermissions(permissions, errors);
          break;
        default:
          break;
      }
    }

  }

  private void validateAdminPermissions(final PermissionsDTO permissions, final Errors errors) {
    final String currentTenant = TenantUtils.getCurrentTenant();
    // It isn't possible to create ADMIN permissions over third party entities
    for (final String applicationId : permissions.getSelectedApplicationsIds()) {
      if (!applicationService.isApplicationFromTenant(applicationId, currentTenant)) {
        errors.reject(ERROR_CODE_ADMIN_TYPE);
        return;
      }
    }
    for (final String providerId : permissions.getSelectedProvidersIds()) {
      if (!providerService.isProviderFromTenant(providerId, currentTenant)) {
        errors.reject(ERROR_CODE_ADMIN_TYPE);
        return;
      }
    }
  }

  private void validateWritePermissions(final PermissionsDTO permissions, final Errors errors) {
    final String currentTenant = TenantUtils.getCurrentTenant();

    for (final String applicationId : permissions.getSelectedApplicationsIds()) {
      final TenantResource tenantResource = applicationService.findAndThrowErrorIfNotExist(new Application(applicationId));
      validateWritePermission(currentTenant, tenantResource, errors);
    }

    for (final String providerId : permissions.getSelectedProvidersIds()) {
      final TenantResource tenantResource = providerService.findAndThrowErrorIfNotExist(new Provider(providerId));
      validateWritePermission(currentTenant, tenantResource, errors);
    }
  }

  private void validateWritePermission(final String currentTenant, final TenantResource tenantResource, final Errors errors) {
    if (!tenantResource.getTenantId().equals(currentTenant)) {
      final TenantPermission permission = tenantPermissionService.findFromPermissionsByEntity(currentTenant, tenantResource.getId());
      if (permission == null || !TenantPermission.Type.WRITE.equals(permission.getType())) {
        errors.reject(ERROR_CODE_WRITE_TYPE);
        return;
      }
    }
  }
}
