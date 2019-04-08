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
package org.sentilo.web.catalog.test.web;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import javax.servlet.FilterChain;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.web.catalog.context.TenantContextHolder;
import org.sentilo.web.catalog.security.CatalogUserDetails;
import org.sentilo.web.catalog.web.TenantInterceptorFilter;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContext;

public class TenantInterceptorFilterTest {

  @InjectMocks
  private TenantInterceptorFilter filter;

  @Mock
  private HttpServletRequest request;

  @Mock
  private HttpServletResponse response;

  @Mock
  private HttpSession session;

  @Mock
  private SecurityContext sc;

  @Mock
  private Authentication authentication;

  @Mock
  private CatalogUserDetails userDetails;

  @Mock
  private FilterChain chain;

  @Before
  public void setUp() {
    System.setProperty(SentiloConstants.SENTILO_MULTITENANT_PROP_KEY, Boolean.TRUE.toString());

    MockitoAnnotations.initMocks(this);
    when(request.getSession()).thenReturn(session);
  }

  @After
  public void tearDown() {
    System.clearProperty(SentiloConstants.SENTILO_MULTITENANT_PROP_KEY);
    TenantContextHolder.clearContext();
  }

  @Test
  public void filterNoMultitenantRequest() throws Exception {
    when(request.getRequestURI()).thenReturn("/sentilo-catalog-web/stats");
    when(request.getContextPath()).thenReturn("/sentilo-catalog-web");
    filter.doFilter(request, response, chain);

    verify(request).setAttribute("tenant-identifier", "");
    verify(request).setAttribute("user-tenant-identifier", "");
    verify(request).setAttribute("f-tenant-identifier", "0");
    verify(request).setAttribute("f-user-tenant-identifier", "0");
  }

  @Test
  public void filterMultitenantRequest() throws Exception {
    when(request.getRequestURI()).thenReturn("/sentilo-catalog-web/mockTenant/stats");
    when(request.getContextPath()).thenReturn("/sentilo-catalog-web");
    filter.doFilter(request, response, chain);

    verify(request).setAttribute("tenant-identifier", "mockTenant/");
    verify(request).setAttribute("user-tenant-identifier", "");
    verify(request).setAttribute("f-tenant-identifier", "1");
    verify(request).setAttribute("f-user-tenant-identifier", "0");
  }

  @Test
  public void filterModifiedMultitenantRequest() throws Exception {
    when(request.getRequestURI()).thenReturn("/sentilo-catalog-web/mockTenant/stats");
    when(request.getContextPath()).thenReturn("/sentilo-catalog-web");
    when(session.getAttribute("SPRING_SECURITY_CONTEXT")).thenReturn(sc);
    when(sc.getAuthentication()).thenReturn(authentication);
    when(authentication.getPrincipal()).thenReturn(userDetails);
    when(userDetails.getTenantId()).thenReturn("userTenantId");

    filter.doFilter(request, response, chain);

    verify(userDetails).getTenantId();
    verify(request).setAttribute("tenant-identifier", "mockTenant/");
    verify(request).setAttribute("user-tenant-identifier", "userTenantId/");
    verify(request).setAttribute("f-tenant-identifier", "1");
    verify(request).setAttribute("f-user-tenant-identifier", "1");
  }
}
