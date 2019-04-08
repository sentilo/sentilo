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

import java.util.List;

import org.sentilo.web.catalog.context.TenantContextHolder;
import org.sentilo.web.catalog.domain.User;
import org.sentilo.web.catalog.security.Role;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

@SentiloValidator
public class UserValidator implements Validator {

  @Override
  public boolean supports(final Class<?> clazz) {
    return User.class.equals(clazz);
  }

  @Override
  public void validate(final Object target, final Errors errors) {
    final User user = (User) target;
    if (TenantContextHolder.isEnabled()) {
      checkUserTenant(user, errors);
    }

  }

  private void checkUserTenant(final User user, final Errors errors) {
    if (!CollectionUtils.isEmpty(user.getRoles())) {
      final List<Role> roles = user.getRoles();
      if ((roles.contains(Role.ADMIN) || roles.contains(Role.USER)) && !StringUtils.hasText(user.getTenantId())) {
        errors.rejectValue("tenantId", "user.validation.error.tenantMandatory");
      } else if ((roles.contains(Role.PLATFORM) || roles.contains(Role.SUPER_ADMIN)) && StringUtils.hasText(user.getTenantId())) {
        errors.rejectValue("tenantId", "user.validation.error.tenantNoApplicable");
      }
    }
  }
}
