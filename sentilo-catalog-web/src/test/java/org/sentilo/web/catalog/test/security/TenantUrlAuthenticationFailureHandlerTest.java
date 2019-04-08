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
package org.sentilo.web.catalog.test.security;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import org.sentilo.web.catalog.context.TenantContextHolder;
import org.sentilo.web.catalog.security.TenantUrlAuthenticationFailureHandler;
import org.sentilo.web.catalog.utils.TenantUtils;
import org.springframework.security.core.AuthenticationException;

@RunWith(PowerMockRunner.class)
@PrepareForTest({TenantContextHolder.class, TenantUtils.class})
public class TenantUrlAuthenticationFailureHandlerTest {

  @Mock
  private HttpServletRequest request;

  @Mock
  private HttpServletResponse response;

  @Mock
  private AuthenticationException exception;

  @Mock
  private HttpSession session;

  @InjectMocks
  private TenantUrlAuthenticationFailureHandler handler;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    when(request.getSession()).thenReturn(session);
    when(request.getContextPath()).thenReturn("sentilo-catalog-web");
    when(exception.getMessage()).thenReturn("mockExceptionErrorMessage");
  }

  @Test
  public void onAuthenticationFailure() throws Exception {
    final String redirectUrl = "sentilo-catalog-web/auth/login?error=true";
    when(response.encodeRedirectURL(redirectUrl)).thenReturn(redirectUrl);

    handler.onAuthenticationFailure(request, response, exception);

    verify(response).sendRedirect(redirectUrl);
  }

  @Test
  public void multitenantOnAuthenticationFailure() throws Exception {
    final String redirectUrl = "sentilo-catalog-web/mockTenant/auth/login?error=true";
    PowerMockito.mockStatic(TenantContextHolder.class);
    PowerMockito.mockStatic(TenantUtils.class);

    when(response.encodeRedirectURL(redirectUrl)).thenReturn(redirectUrl);
    when(TenantContextHolder.hasContext()).thenReturn(true);
    when(TenantUtils.getCurrentTenant()).thenReturn("mockTenant");

    handler.onAuthenticationFailure(request, response, exception);

    verify(response).sendRedirect(redirectUrl);
  }

}
