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
package org.sentilo.web.catalog.web;

import java.io.IOException;
import java.util.Arrays;

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;

import org.sentilo.web.catalog.context.TenantContext;
import org.sentilo.web.catalog.context.TenantContextHolder;
import org.sentilo.web.catalog.context.TenantContextImpl;
import org.sentilo.web.catalog.security.CatalogUserDetails;
import org.sentilo.web.catalog.utils.CatalogUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.util.StringUtils;

/**
 * The goal of this filter is to detect and extract the tenant identifier if it is present at URL.
 */
public class TenantInterceptorFilter implements Filter {

  private static final Logger LOGGER = LoggerFactory.getLogger(TenantInterceptorFilter.class);
  private static final String SPRING_SECURITY_CONTEXT = "SPRING_SECURITY_CONTEXT";

  private final String[] mappings = {"/", "/home", "/admin", "/auth", "/api", "/component", "/stats", "/status", "/static", "/WEB-INF",
      "/j_spring_security_check", "/j_spring_security_logout"};

  @Override
  public void destroy() {
    LOGGER.info("destroy called");
  }

  @Override
  public void doFilter(final ServletRequest request, final ServletResponse response, final FilterChain chain) throws IOException, ServletException {
    final HttpServletRequest hsRequest = (HttpServletRequest) request;
    isMultiTenantRequest(hsRequest);
    final boolean isStaticRequest = CatalogUtils.isStaticRequest(hsRequest);
    final boolean userIsLoggedIn = hsRequest.getSession().getAttribute(SPRING_SECURITY_CONTEXT) != null;

    if (TenantContextHolder.isEnabled() && !isStaticRequest) {
      final String requestTenant = lookupRequestTenant(hsRequest);
      final String userTenant = lookupUserTenant(hsRequest);
      final TenantContext tenantContext = new TenantContextImpl(requestTenant, userTenant);

      LOGGER.debug("Request [{}] checked and lookup userTenant [{}] and requestTenant [{}]", hsRequest.getRequestURI(), userTenant, requestTenant);
      TenantContextHolder.setContext(tenantContext);

      hsRequest.setAttribute("tenant-identifier", formatTenantTokenContextPath(requestTenant));
      hsRequest.setAttribute("user-tenant-identifier", formatTenantTokenContextPath(userTenant));
      hsRequest.setAttribute("f-tenant-identifier", StringUtils.hasText(requestTenant) ? "1" : "0");
      hsRequest.setAttribute("f-user-tenant-identifier", StringUtils.hasText(userTenant) || userIsLoggedIn ? "1" : "0");

    }

    chain.doFilter(request, response);

    // if (isMultiTenantRequest) {
    TenantContextHolder.clearContext();
    // }
  }

  @Override
  public void init(final FilterConfig arg0) throws ServletException {
    LOGGER.info("Init filter");
  }

  private String formatTenantTokenContextPath(final String tenant) {
    return StringUtils.hasText(tenant) ? tenant + "/" : "";
  }

  private boolean isMultiTenantRequest(final HttpServletRequest request) {
    final String requestUri = request.getRequestURI();
    final String requestContextPath = request.getContextPath();

    LOGGER.debug("Checking request [{}]", requestUri);

    final String mappingPath = requestUri.substring(requestContextPath.length());
    final String[] tokens = mappingPath.trim().split("/");

    return tokens.length > 0 && !Arrays.asList(mappings).contains("/" + tokens[1]);
  }

  private String lookupRequestTenant(final HttpServletRequest request) {
    final String requestUri = request.getRequestURI();
    final String requestContextPath = request.getContextPath();
    final String mappingPath = requestUri.substring(requestContextPath.length());
    final String[] tokens = mappingPath.trim().split("/");

    // return tokens[1];
    return tokens.length > 0 && !Arrays.asList(mappings).contains("/" + tokens[1]) ? tokens[1] : null;
  }

  private String lookupUserTenant(final HttpServletRequest request) {
    String userTenant = null;
    if (request.getSession().getAttribute(SPRING_SECURITY_CONTEXT) != null) {
      final SecurityContext ctx = (SecurityContext) request.getSession().getAttribute(SPRING_SECURITY_CONTEXT);
      final CatalogUserDetails userDetails = (CatalogUserDetails) ctx.getAuthentication().getPrincipal();
      userTenant = userDetails.getTenantId();
    }

    return userTenant;
  }
}
