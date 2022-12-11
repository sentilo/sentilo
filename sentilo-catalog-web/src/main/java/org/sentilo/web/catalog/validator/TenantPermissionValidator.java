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
import org.sentilo.web.catalog.dto.TenantPermissionsDTO;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

@SentiloValidator
public class TenantPermissionValidator implements Validator {

  public static final String ERROR_CODE_EMPTY_SELECTED_IDS = "permission.error.emptyIdsLists";

  @Override
  public boolean supports(final Class<?> clazz) {
    return TenantPermissionsDTO.class.equals(clazz);
  }

  @Override
  public void validate(final Object target, final Errors errors) {
    final TenantPermissionsDTO permission = (TenantPermissionsDTO) target;
    if (permission != null && arraysIdsAreEmpty(permission)) {
      errors.reject(ERROR_CODE_EMPTY_SELECTED_IDS);
    }
  }

  private boolean arraysIdsAreEmpty(final TenantPermissionsDTO permission) {
    return SentiloUtils.arrayIsEmpty(permission.getSelectedProvidersIds()) || SentiloUtils.arrayIsEmpty(permission.getSelectedEntitiesIds());
  }

}
