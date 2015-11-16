/*
 * Sentilo
 *  
 * Original version 1.4 Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de Barcelona.
 * Modified by Opentrends adding support for multitenant deployments and SaaS. Modifications on version 1.5 Copyright (C) 2015 Opentrends Solucions i Sistemes, S.L.
 * 
 *   
 * This program is licensed and may be used, modified and redistributed under the
 * terms  of the European Public License (EUPL), either version 1.1 or (at your 
 * option) any later version as soon as they are approved by the European 
 * Commission.
 *   
 * Alternatively, you may redistribute and/or modify this program under the terms
 * of the GNU Lesser General Public License as published by the Free Software 
 * Foundation; either  version 3 of the License, or (at your option) any later 
 * version. 
 *   
 * Unless required by applicable law or agreed to in writing, software distributed
 * under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR 
 * CONDITIONS OF ANY KIND, either express or implied. 
 *   
 * See the licenses for the specific language governing permissions, limitations 
 * and more details.
 *   
 * You should have received a copy of the EUPL1.1 and the LGPLv3 licenses along 
 * with this program; if not, you may find them at: 
 *   
 *   https://joinup.ec.europa.eu/software/page/eupl/licence-eupl
 *   http://www.gnu.org/licenses/ 
 *   and 
 *   https://www.gnu.org/licenses/lgpl.txt
 */
package org.sentilo.web.catalog.security;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.sentilo.web.catalog.context.TenantContextHolder;
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

public class TenantLoginLogoutSuccessHandler implements ApplicationListener<AuthenticationSuccessEvent>, LogoutSuccessHandler, AuthenticationSuccessHandler {

  private static final String DEFAULT_TARGET_URL = "/";
  private final SimpleUrlAuthenticationSuccessHandler loginSuccessHandler = new SimpleUrlAuthenticationSuccessHandler();
  private final SimpleUrlLogoutSuccessHandler logoutSuccessHandler = new SimpleUrlLogoutSuccessHandler();

  @Autowired
  private AuditHandler auditHandler;

  @Override
  public void onApplicationEvent(final AuthenticationSuccessEvent event) {
    auditHandler.logUserLogin(event.getAuthentication().getName());

  }

  @Override
  public void onAuthenticationSuccess(final HttpServletRequest request, final HttpServletResponse response, final Authentication authentication)
      throws IOException, ServletException {
    loginSuccessHandler.setDefaultTargetUrl(buildDefaultTargetUrl());
    loginSuccessHandler.onAuthenticationSuccess(request, response, authentication);

  }

  @Override
  public void onLogoutSuccess(final HttpServletRequest request, final HttpServletResponse response, final Authentication authentication)
      throws IOException, ServletException {
    logoutSuccessHandler.setDefaultTargetUrl(buildDefaultTargetUrl());
    logoutSuccessHandler.onLogoutSuccess(request, response, authentication);
  }

  private String buildDefaultTargetUrl() {
    return (TenantContextHolder.hasContext() ? DEFAULT_TARGET_URL + TenantUtils.getCurrentTenant() : DEFAULT_TARGET_URL);
  }

}
