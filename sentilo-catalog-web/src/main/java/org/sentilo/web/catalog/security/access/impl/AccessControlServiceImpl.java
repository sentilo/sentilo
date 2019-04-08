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
package org.sentilo.web.catalog.security.access.impl;

import org.sentilo.common.utils.SentiloUtils;
import org.sentilo.web.catalog.context.TenantContextHolder;
import org.sentilo.web.catalog.domain.CatalogDocument;
import org.sentilo.web.catalog.domain.Tenant;
import org.sentilo.web.catalog.domain.TenantResource;
import org.sentilo.web.catalog.domain.User;
import org.sentilo.web.catalog.exception.NotAllowedActionException;
import org.sentilo.web.catalog.security.CatalogUserDetails;
import org.sentilo.web.catalog.security.access.AccessControlContext;
import org.sentilo.web.catalog.security.access.AccessControlRepository;
import org.sentilo.web.catalog.security.access.AccessControlService;
import org.sentilo.web.catalog.security.access.ActionGrant;
import org.sentilo.web.catalog.security.enums.ActionType;
import org.sentilo.web.catalog.security.service.CatalogUserDetailsService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class AccessControlServiceImpl implements AccessControlService {

  private static final Logger LOGGER = LoggerFactory.getLogger(AccessControlServiceImpl.class);

  private static final int TENANT_LINK_EQ = 0;
  private static final int TENANT_LINK_AUTH = 1;
  private static final int TENANT_LINK_OTHER = 2;

  @Autowired
  private CatalogUserDetailsService userDetailsService;

  @Autowired
  private AccessControlRepository aclRepository;

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.security.access.AccessControlService#checkAccess(org.sentilo.web.
   * catalog .security.access.AccessControlContext)
   */
  public void checkAccess(final AccessControlContext acc) {

    if (userDetailsService.getCatalogUserDetails() == null) {
      LOGGER.error("Anonymous users are not allowed to {} resources of type {}", acc.getAction().name(), acc.getResourceClass().getName());
      throw new NotAllowedActionException();
    }

    final CatalogUserDetails userDetails = userDetailsService.getCatalogUserDetails();
    final String userRole = userDetails.isSuperAdminUser() ? "SA" : userDetails.isAdminUser() ? "A" : "U";
    final ActionGrant[] grants = aclRepository.getGrants(acc.getResourceClass(), userRole);

    if (SentiloUtils.arrayIsEmpty(grants) || !checkGrants(grants, userDetails, acc) || !checkUser(userDetails, acc)) {
      LOGGER.error("User {} is not allowed to {} resources of type {} ", userDetails.getUsername(), acc.getAction().name(),
          acc.getResourceClass().getName());
      throw new NotAllowedActionException();
    }
  }

  private boolean checkUser(final CatalogUserDetails userDetails, final AccessControlContext acc) {
    // If currently logged user has the role ROLE_USER then only he can modify its data
    if (acc.getResource() instanceof User && userDetails.isUser()) {
      return ((User) acc.getResource()).getUserName().equals(userDetails.getUsername());
    } else {
      return true;
    }
  }

  private boolean checkGrants(final ActionGrant[] grants, final CatalogUserDetails userDetails, final AccessControlContext acc) {
    boolean allowed = false;
    final int tenantsLink =
        userDetails.isSuperAdminUser() || !TenantContextHolder.isEnabled() ? TENANT_LINK_OTHER : findTenantsLink(userDetails.getTenantId(), acc);

    for (final ActionGrant grant : grants) {
      allowed = allowed | checkGrant(grant, acc.getAction(), tenantsLink);
    }

    return allowed;
  }

  private boolean checkGrant(final ActionGrant grant, final ActionType action, final int linkBetweenUserAndResourceTenant) {
    return grant.isActionAllowed(action.getCode(), linkBetweenUserAndResourceTenant);
  }

  private int findTenantsLink(final String userTenant, final AccessControlContext acc) {
    // If action is LIST or CREATE then the link between tenants is irrelevant. In this case the
    // method returns 2 (the value with less restriction)
    // Otherwise, returns 0, 1 or 2
    int tenantsLink = getDefaultTenantsLink();
    switch (acc.getAction()) {
      case LIST:
      case CREATE:
        tenantsLink = getDefaultTenantsLink();
        break;
      case SAVE_NEW:
        final String resourceTenant = getResourceTenant(acc.getResource());
        tenantsLink = userTenant.equals(resourceTenant) ? TENANT_LINK_EQ : getDefaultTenantsLink();
        break;
      case SAVE:
      case EDIT:
      case READ:
      case DELETE:
        tenantsLink = findTenantsLinkForAdminAction(userTenant, acc);
        break;
      default:
        throw new IllegalArgumentException("Unknown action type");
    }

    return tenantsLink;
  }

  private int getDefaultTenantsLink() {
    return TENANT_LINK_OTHER;
  }

  private int findTenantsLinkForAdminAction(final String userTenant, final AccessControlContext acc) {
    int tenantsLink = getDefaultTenantsLink();
    if (acc.getResource() instanceof TenantResource) {
      final TenantResource aux = (TenantResource) acc.getService().findAndThrowErrorIfNotExist(acc.getResource());
      if (userTenant.equals(aux.getTenantId())) {
        tenantsLink = TENANT_LINK_EQ;
      } else if (aux.getTenantsAuth().contains(userTenant)) {
        tenantsLink = TENANT_LINK_AUTH;
      }
    } else if (acc.getResource() instanceof Tenant) {
      tenantsLink = userTenant.equals(acc.getResource().getId()) ? TENANT_LINK_EQ : TENANT_LINK_OTHER;
    }

    return tenantsLink;
  }

  private String getResourceTenant(final CatalogDocument resource) {
    return resource instanceof TenantResource ? ((TenantResource) resource).getTenantId() : resource.getId();
  }

}
