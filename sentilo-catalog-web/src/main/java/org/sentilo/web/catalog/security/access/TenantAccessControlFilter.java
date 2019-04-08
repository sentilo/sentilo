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
package org.sentilo.web.catalog.security.access;

import java.io.IOException;
import java.util.Map;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;

import org.sentilo.web.catalog.context.TenantContextHolder;
import org.sentilo.web.catalog.security.CatalogUserDetails;
import org.sentilo.web.catalog.security.service.CatalogUserDetailsService;
import org.sentilo.web.catalog.utils.TenantUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.authentication.AnonymousAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.authority.AuthorityUtils;
import org.springframework.security.web.access.WebInvocationPrivilegeEvaluator;
import org.springframework.web.context.support.WebApplicationContextUtils;
import org.springframework.web.filter.GenericFilterBean;

/**
 * The goal of this filter is to ensure that users only access to restricted pages related to its
 * own tenant site.
 *
 * A secondary goal is to ensure that a logged in user automatically access to his tenant site if
 * the tenant is not filled in at the request
 */
public class TenantAccessControlFilter extends GenericFilterBean {

  @Autowired
  private CatalogUserDetailsService userDetailsService;

  private WebInvocationPrivilegeEvaluator requestEvaluator;

  private final Authentication anonymousAuth =
      new AnonymousAuthenticationToken("_KEY", "anonymousUser", AuthorityUtils.createAuthorityList("ROLE_ANONYMOUS"));

  @Override
  public void doFilter(final ServletRequest req, final ServletResponse res, final FilterChain chain) throws IOException, ServletException {
    final HttpServletRequest request = (HttpServletRequest) req;
    if (allowUserAccess(request)) {
      chain.doFilter(req, res);
    } else {
      throw new AccessDeniedException("You are not allowed to access this page!");
    }
  }

  private boolean allowUserAccess(final HttpServletRequest request) {
    boolean allowAccess = true;

    if (filterByTenantEnabled() && userIsLoggedIn() && !isAnonymousRequest(request)) {
      allowAccess = userBelongsToRequestTenant(request);
    }

    return allowAccess;
  }

  private boolean filterByTenantEnabled() {
    return TenantContextHolder.isEnabled();
  }

  private boolean isAnonymousRequest(final HttpServletRequest request) {
    final String requestUri = request.getRequestURI().substring(request.getContextPath().length());
    return requestUri.startsWith("/WEB-INF/") || getRequestEvaluator().isAllowed(requestUri, anonymousAuth);
  }

  private boolean userIsLoggedIn() {
    return userDetailsService.getCatalogUserDetails() != null;
  }

  private boolean userBelongsToRequestTenant(final HttpServletRequest request) {
    boolean userBelongsToTenant = false;
    final CatalogUserDetails userDetails = userDetailsService.getCatalogUserDetails();

    if (userDetails != null) {
      final String requestTenant = TenantUtils.getRequestTenant();

      userBelongsToTenant = userDetails.isSuperAdminUser() && requestTenant == null
          || !userDetails.isSuperAdminUser() && userDetails.getTenantId().equals(requestTenant);
    }

    return userBelongsToTenant;
  }

  private WebInvocationPrivilegeEvaluator getRequestEvaluator() {
    if (requestEvaluator == null) {
      final ApplicationContext ctx = WebApplicationContextUtils.getRequiredWebApplicationContext(getServletContext());
      final Map<String, WebInvocationPrivilegeEvaluator> wipes = ctx.getBeansOfType(WebInvocationPrivilegeEvaluator.class);
      requestEvaluator = (WebInvocationPrivilegeEvaluator) wipes.values().toArray()[1];
    }

    return requestEvaluator;
  }

}
