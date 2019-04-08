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
package org.sentilo.web.catalog.security;

import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.web.catalog.context.TenantContextHolder;
import org.sentilo.web.catalog.domain.CatalogDocument;
import org.sentilo.web.catalog.domain.Tenant;
import org.sentilo.web.catalog.domain.TenantResource;
import org.sentilo.web.catalog.domain.User;
import org.sentilo.web.catalog.security.enums.ActionType;
import org.sentilo.web.catalog.utils.TenantUtils;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.util.ClassUtils;

public class SecurityUtils {

  public static boolean isFederationEnabled() {
    return "true".equals(System.getProperty(SentiloConstants.SENTILO_FEDERATION_ENABLED_PROP_KEY, "false"));
  }

  /**
   * Utility method used in the views to decide if "admin controls" should be displayed
   *
   * @param action
   * @param target: in the list views, it is the name of the resource class (i.e. a String); in the
   *        other views it is directly the domain object
   * @return
   */
  public static boolean showAdminControls(final ActionType action, final Object target) {
    if (target instanceof String) {
      return _showAdminControls(action, (String) target);
    } else if (target instanceof CatalogDocument) {
      return _showAdminControls(action, (CatalogDocument) target);
    } else {
      return false;
    }
  }

  private static boolean _showAdminControls(final ActionType action, final String resourceClass) {
    try {
      final CatalogDocument resource = (CatalogDocument) ClassUtils.forName(resourceClass, ClassUtils.getDefaultClassLoader()).newInstance();
      return _showAdminControls(action, resource);
    } catch (final Exception e) {
      return false;
    }
  }

  private static boolean _showAdminControls(final ActionType action, final CatalogDocument resource) {
    // Depending on the logged in user role, the page displayed and the resource's tenant owner the
    // admin controls will be displayed.
    final CatalogUserDetails catalogUser = (CatalogUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
    final boolean multitenantEnabled = TenantContextHolder.isEnabled();

    if (catalogUser.isSuperAdminUser()) {
      return multitenantEnabled && allowSuperAdminUserAdminResource(resource);
    } else if (catalogUser.isAdminUser()) {
      return !multitenantEnabled || allowAdminUserAdminResource(action, catalogUser, resource);
    } else {
      return false;
    }
  }

  private static boolean allowSuperAdminUserAdminResource(final CatalogDocument resource) {
    return TenantContextHolder.isEnabled() && (!(resource instanceof TenantResource) || resource instanceof User);
  }

  private static boolean allowAdminUserAdminResource(final ActionType action, final CatalogUserDetails catalogUser, final CatalogDocument resource) {
    final boolean isTenantResource = resource instanceof TenantResource;
    final boolean isTenant = resource instanceof Tenant;

    boolean allowAdmin = false;

    switch (action) {
      case LIST:
      case CREATE:
        allowAdmin = isTenantResource;
        break;
      case READ:
      case EDIT:
        allowAdmin = (isTenantResource || isTenant) && TenantUtils.isCurrentTenantResource(resource);
        break;
      default:
        break;
    }

    return allowAdmin;
  }

}
