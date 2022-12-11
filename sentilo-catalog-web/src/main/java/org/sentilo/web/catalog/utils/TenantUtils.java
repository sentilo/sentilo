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
import org.sentilo.web.catalog.domain.Component;
import org.sentilo.web.catalog.domain.Tenant;
import org.sentilo.web.catalog.domain.TenantResource;
import org.sentilo.web.catalog.domain.User;
import org.springframework.util.StringUtils;

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
   * Copy tenant fields from <code>sourceResource</code> to <code>targetResource</code>
   *
   * @param targetResource
   * @param sourceResource
   */
  public static void copyTenantFields(final TenantResource targetResource, final TenantResource sourceResource) {
    if (sourceResource != null) {
      targetResource.setTenantsAuth(sourceResource.getTenantsAuth());
      targetResource.setTenantsListVisible(sourceResource.getTenantsListVisible());
      targetResource.setTenantId(sourceResource.getTenantId());
      // And finally, if tenantResource is Component, also copy tenantsMapVisible members
      if (targetResource instanceof Component) {
        ((Component) targetResource).getTenantsMapVisible().addAll(((Component) sourceResource).getTenantsMapVisible());
      }
    }
  }

  /**
   * Add/set <code>tenantIdOwner</code> in each of the tenant fields
   *
   * @param targetResource
   * @param tenantIdOwner
   */
  public static void initTenantFields(final TenantResource targetResource, final String tenantIdOwner) {
    if (StringUtils.hasText(tenantIdOwner)) {
      targetResource.getTenantsAuth().add(tenantIdOwner);
      targetResource.getTenantsListVisible().add(tenantIdOwner);
      targetResource.setTenantId(tenantIdOwner);
      // And finally, if targetResource is Component, also add tenantIdOwner as member of
      // tenantsMapVisible
      if (targetResource instanceof Component) {
        ((Component) targetResource).getTenantsMapVisible().add(tenantIdOwner);
      }
    } else if (targetResource instanceof User) {
      // This case occurs when a super admin user creates a new user
      final String resourceTenant = targetResource.getTenantId();
      targetResource.getTenantsAuth().add(resourceTenant);
      targetResource.getTenantsListVisible().add(resourceTenant);
    }
  }

  /**
   * Add tenant fields from <code>sourceResource</code> to <code>targetResource</code>, preserving
   * the existing ones.
   *
   * @param targetResource
   * @param parentSourceResource
   */
  public static void copyTenantFieldsFromEntityParent(final TenantResource targetResource, final TenantResource parentSourceResource) {
    // Set tenantId owner equals to parentSourceResource's tenant owner
    targetResource.setTenantId(parentSourceResource.getTenantId());
    // Add all tenantsAuth from parentSourceResource (i.e. Provider / Application) as authorized
    // tenants
    targetResource.getTenantsAuth().addAll(parentSourceResource.getTenantsAuth());
    // Add all tenantsListVisible from parentSourceResource (i.e. Provider / Application) as visible
    // resources on lists
    targetResource.getTenantsListVisible().addAll(parentSourceResource.getTenantsListVisible());
    // And finally, if targetResource is a Component, add parentSourceResource tenants auth to its
    // tenantsMapVisible map
    if (targetResource instanceof Component) {
      ((Component) targetResource).getTenantsMapVisible().addAll(parentSourceResource.getTenantsAuth());
    }
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

  public static String buildResourceIdWithTenant(final String tenantId, final String currentResourceId) {
    // Only if currentResourceId is not already start with tenantId@ value prepend this prefix to it
    final String prefix = tenantId + Constants.MULTITENANT_ENTITY_ID_PREPEND_TOKEN;
    return currentResourceId.startsWith(prefix) ? currentResourceId : prefix + currentResourceId;
  }

}
