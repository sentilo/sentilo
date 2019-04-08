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
package org.sentilo.web.catalog.security.service.impl;

import org.sentilo.web.catalog.context.TenantContextHolder;
import org.sentilo.web.catalog.context.TenantContextImpl;
import org.sentilo.web.catalog.domain.User;
import org.sentilo.web.catalog.exception.UserLoginNotAllowedException;
import org.sentilo.web.catalog.security.CatalogUserDetails;
import org.sentilo.web.catalog.security.service.CatalogUserDetailsService;
import org.sentilo.web.catalog.service.UserService;
import org.sentilo.web.catalog.utils.Constants;
import org.sentilo.web.catalog.utils.TenantUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.authentication.LockedException;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

@Service("userDetailsService")
public class CatalogUserDetailsServiceImpl implements UserDetailsService, CatalogUserDetailsService {

  private static final Logger LOGGER = LoggerFactory.getLogger(CatalogUserDetailsServiceImpl.class);

  @Autowired
  private UserService userService;

  /*
   * (non-Javadoc)
   *
   * @see
   * org.springframework.security.core.userdetails.UserDetailsService#loadUserByUsername(java.lang
   * .String)
   */
  public UserDetails loadUserByUsername(final String userName) throws UsernameNotFoundException {
    final User user = userService.find(new User(userName));

    if (user == null) {
      LOGGER.warn("User {} not found!", userName);
      final String errorMessage = String.format("%s - Unknown user %s", Constants.AUTH_BAD_CREDENTIALS_CODE, userName);
      throw new UsernameNotFoundException(errorMessage);
    }

    if (!user.isActive()) {
      LOGGER.warn("User {} is locked.", userName);
      final String errorMessage =
          String.format("%s - User account %s is locked. Contact with administrator platform.", Constants.AUTH_LOCKED_ACCOUNT_CODE, userName);
      throw new LockedException(errorMessage);
    }

    final CatalogUserDetails catalogUser = new CatalogUserDetails(user);
    LOGGER.debug("TenantContextHolder.isEnabled()? {}", TenantContextHolder.isEnabled());
    if (TenantContextHolder.isEnabled()) {
      checkUserTenant(catalogUser);
    } else if (catalogUser.isSuperAdminUser()) {
      // If user has role SUPER_ADMIN and multitenant feature is disabled then user login is
      // rejected: SUPER_ADMIN users only can access to multitenant instances
      final String errorMessage = String.format("%s - Super admin user %s is not allowed to access to a no multitenant instance of Sentilo.",
          Constants.AUTH_NO_MULTITENANT_ACCESS_CODE, userName);
      throw new UserLoginNotAllowedException(errorMessage);
    }

    return catalogUser;
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.security.CatalogUserDetailsService#getCatalogUserDetails()
   */
  public CatalogUserDetails getCatalogUserDetails() {
    final Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
    final Object user = authentication != null ? authentication.getPrincipal() : null;
    return user != null && user instanceof CatalogUserDetails ? (CatalogUserDetails) user : null;
  }

  private void checkUserTenant(final CatalogUserDetails catalogUser) {
    final String currentTenantId = TenantUtils.getCurrentTenant();
    final boolean superAdminOrPlatformUser = catalogUser.isSuperAdminUser() || catalogUser.isPlatformUser();

    LOGGER.debug("checkUserTenant: user [{}] - tenant user [{}] - current tenant [{}]", catalogUser.getUsername(), catalogUser.getTenantId(),
        currentTenantId);
    LOGGER.debug("checkUserTenant: superAdminOrPlatformUser? {} ", superAdminOrPlatformUser);

    if (StringUtils.hasText(currentTenantId)) {
      checkAccessToTenantSite(catalogUser, currentTenantId);
    } else if (TenantContextHolder.inferTenantFromLogin() && !superAdminOrPlatformUser && StringUtils.hasText(catalogUser.getTenantId())) {
      TenantContextHolder.setContext(new TenantContextImpl(catalogUser.getTenantId()));
    } else if (!superAdminOrPlatformUser) {
      final String errorMessage = String.format("%s - User %s only can access to their organization site.", Constants.AUTH_ACCESS_NOT_ALLOWED_CODE,
          catalogUser.getUsername());
      throw new UserLoginNotAllowedException(errorMessage);
    }
  }

  /**
   * Only users from the site <code>currentTenantId</code> can access to it.
   *
   * @param catalogUser
   * @param currentTenantId
   */
  private void checkAccessToTenantSite(final CatalogUserDetails catalogUser, final String currentTenantId) {
    if (catalogUser.isSuperAdminUser()) {
      final String errorMessage = String.format("%s - Super admin can't access to an organization site", Constants.AUTH_ACCESS_NOT_ALLOWED_CODE);
      throw new UserLoginNotAllowedException(errorMessage);
    } else if (!currentTenantId.equals(catalogUser.getTenantId())) {
      final String errorMessage =
          String.format("%s - User %s don't belong to site %s", Constants.AUTH_ACCESS_NOT_ALLOWED_CODE, catalogUser.getUsername(), currentTenantId);
      throw new UserLoginNotAllowedException(errorMessage);
    }
  }

}
