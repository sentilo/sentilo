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
package org.sentilo.web.catalog.utils;

import org.sentilo.common.utils.SentiloUtils;
import org.sentilo.web.catalog.context.TenantContextHolder;
import org.sentilo.web.catalog.domain.CatalogDocument;
import org.sentilo.web.catalog.domain.Tenant;
import org.sentilo.web.catalog.domain.TenantResource;

public abstract class TenantUtils extends CatalogUtils {

  private TenantUtils() {
    // this prevents even the native class from calling this ctor as well :
    throw new AssertionError();
  }

  /**
   * Returns the resource's tenant owner if resource is a tenant resource and multitenant feature is
   * enabled.
   *
   * @param resource
   * @return resource's tenant owner
   */
  public static String getResourceTenantOwner(final CatalogDocument resource) {
    String resourceTenantOwner = null;

    if (TenantContextHolder.isEnabled() && (resource instanceof Tenant || resource instanceof TenantResource)) {
      resourceTenantOwner = resource instanceof Tenant ? ((Tenant) resource).getId() : ((TenantResource) resource).getTenantId();
    }

    return resourceTenantOwner;
  }

  /**
   * Checks if the current tenant is the owner of the resource.
   *
   * @param resource
   * @return
   */
  public static boolean isCurrentTenantResource(final CatalogDocument resource) {
    boolean isCurrentTenantResource = true;

    if (TenantContextHolder.isEnabled()) {
      final String currentTenant = getCurrentTenant();
      final String resourceTenant = getResourceTenantOwner(resource);

      isCurrentTenantResource = SentiloUtils.areEquals(currentTenant, resourceTenant);
    }

    return isCurrentTenantResource;
  }

  /**
   * Return the current tenant associated with the site that the user is browsing or null if
   * multitenant feature is disabled.
   *
   * @return
   */
  public static String getCurrentTenant() {
    return TenantContextHolder.hasContext() ? TenantContextHolder.getContext().getCurrentTenant() : null;
  }

  public static String getRequestTenant() {
    return TenantContextHolder.hasContext() ? TenantContextHolder.getContext().getRequestTenant() : null;
  }

  public static String getUserTenant() {
    return TenantContextHolder.hasContext() ? TenantContextHolder.getContext().getUserTenant() : null;
  }

  public static String getMenuCurrentTenant() {
    return TenantContextHolder.hasContext() ? TenantContextHolder.getContext().getCurrentTenant() : "sentilo";
  }

}
