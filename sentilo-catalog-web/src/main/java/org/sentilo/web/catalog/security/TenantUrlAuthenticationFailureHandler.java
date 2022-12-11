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

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.sentilo.web.catalog.context.TenantContextHolder;
import org.sentilo.web.catalog.utils.Constants;
import org.sentilo.web.catalog.utils.TenantUtils;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.web.authentication.SimpleUrlAuthenticationFailureHandler;
import org.springframework.util.StringUtils;

public class TenantUrlAuthenticationFailureHandler extends SimpleUrlAuthenticationFailureHandler {

  private static final String DEFAULT_FAILURE_URL = "/auth/login?error=true";
  private static final String CUSTOM_AUTH_ERROR_CODE = "_CUSTOM_AUTH_ERROR_CODE";

  public TenantUrlAuthenticationFailureHandler() {
    super(DEFAULT_FAILURE_URL);
    setRedirectStrategy(new SentiloRedirectStrategy());
  }

  @Override
  public void onAuthenticationFailure(final HttpServletRequest request, final HttpServletResponse response, final AuthenticationException exception)
      throws IOException, ServletException {
    if (TenantContextHolder.hasContext() && StringUtils.hasText(TenantUtils.getCurrentTenant())) {
      setDefaultFailureUrl("/" + TenantUtils.getCurrentTenant() + DEFAULT_FAILURE_URL);
    } else {
      setDefaultFailureUrl(DEFAULT_FAILURE_URL);
    }

    super.onAuthenticationFailure(request, response, exception);

    setErrorMessageKey(request, response, exception);
  }

  private void setErrorMessageKey(final HttpServletRequest request, final HttpServletResponse response, final AuthenticationException exception) {
    // Build and set localized error message to display it in the login page
    final String defaultErrorMessageKey = "login.error";
    String errorMessageKey = defaultErrorMessageKey;
    if (exception.getMessage().startsWith(Constants.AUTH_LOCKED_ACCOUNT_CODE)) {
      errorMessageKey = "login.blocked.admin";;
    }

    request.getSession().setAttribute(CUSTOM_AUTH_ERROR_CODE, errorMessageKey);
  }

}
