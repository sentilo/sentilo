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

import java.io.IOException;
import java.util.Base64;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.codec.digest.DigestUtils;
import org.sentilo.web.catalog.context.TenantContextHolder;
import org.sentilo.web.catalog.context.TenantContextImpl;
import org.sentilo.web.catalog.security.audit.AuditHandler;
import org.sentilo.web.catalog.utils.TenantUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationListener;
import org.springframework.security.authentication.event.AuthenticationSuccessEvent;
import org.springframework.security.core.Authentication;
import org.springframework.security.web.authentication.AuthenticationSuccessHandler;
import org.springframework.security.web.authentication.SimpleUrlAuthenticationSuccessHandler;
import org.springframework.security.web.authentication.logout.LogoutSuccessHandler;
import org.springframework.security.web.authentication.logout.SimpleUrlLogoutSuccessHandler;
import org.springframework.util.StringUtils;

public class TenantLoginLogoutSuccessHandler implements ApplicationListener<AuthenticationSuccessEvent>, LogoutSuccessHandler, AuthenticationSuccessHandler {

  private static final String DEFAULT_TARGET_URL = "/";
  private static final String COOKIE_NAME = "SLID";

  private final SimpleUrlAuthenticationSuccessHandler loginSuccessHandler = new SimpleUrlAuthenticationSuccessHandler();
  private final SimpleUrlLogoutSuccessHandler logoutSuccessHandler = new SimpleUrlLogoutSuccessHandler();
  private SentiloRedirectStrategy redirectStrategy = new SentiloRedirectStrategy();

  @Autowired
  private AuditHandler auditHandler;

  public TenantLoginLogoutSuccessHandler() {
    loginSuccessHandler.setRedirectStrategy(redirectStrategy);
    logoutSuccessHandler.setRedirectStrategy(redirectStrategy);
  }

  @Override
  public void onApplicationEvent(final AuthenticationSuccessEvent event) {
    auditHandler.logUserLogin(event.getAuthentication().getName());
  }

  @Override
  public void onAuthenticationSuccess(final HttpServletRequest request, final HttpServletResponse response, final Authentication authentication)
      throws IOException, ServletException {
    setLoginCookie(request, response, authentication);
    // Update TenantContext to set the userTenant
    TenantContextHolder
        .setContext(new TenantContextImpl(TenantUtils.getRequestTenant(), ((CatalogUserDetails) authentication.getPrincipal()).getTenantId()));
    loginSuccessHandler.setDefaultTargetUrl(buildDefaultTargetUrl());
    loginSuccessHandler.onAuthenticationSuccess(request, response, authentication);
  }

  @Override
  public void onLogoutSuccess(final HttpServletRequest request, final HttpServletResponse response, final Authentication authentication)
      throws IOException, ServletException {

    auditHandler.logUserLogout(authentication.getName());
    removeLoginCookie(request, response, authentication);
    logoutSuccessHandler.setDefaultTargetUrl(buildDefaultTargetUrl());
    logoutSuccessHandler.onLogoutSuccess(request, response, authentication);
  }

  private void setLoginCookie(final HttpServletRequest request, final HttpServletResponse response, final Authentication authentication) {
    final String userName = authentication.getName();
    final String cookieValue = DigestUtils.md5Hex(request.getSession(false).getId() + ":" + userName);
    final StringBuilder value = new StringBuilder(Base64.getEncoder().encodeToString(cookieValue.getBytes()));

    response.addHeader("Set-Cookie", COOKIE_NAME + "=" + value.toString() + "; Path=" + getCookiePath(request) + "; HttpOnly; Secure");
  }

  private String getCookiePath(final HttpServletRequest request) {
    final String contextPath = request.getContextPath();
    return contextPath.length() > 0 ? contextPath : "/";
  }

  private void removeLoginCookie(final HttpServletRequest request, final HttpServletResponse response, final Authentication authentication) {
    final String cookieValue = "void";

    response.addHeader("Set-Cookie",
        COOKIE_NAME + "=" + cookieValue + "; Expires=Wed, 01 Jun 2016 10:00:00 GMT; Path=" + getCookiePath(request) + "; HttpOnly; Secure");
  }

  private String buildDefaultTargetUrl() {
    return StringUtils.hasText(TenantUtils.getUserTenant()) ? DEFAULT_TARGET_URL + TenantUtils.getUserTenant() : DEFAULT_TARGET_URL;
  }

}
