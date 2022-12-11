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
package org.sentilo.web.catalog.test.security.access;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import javax.servlet.FilterChain;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.web.catalog.context.TenantContext;
import org.sentilo.web.catalog.context.TenantContextHolder;
import org.sentilo.web.catalog.security.CatalogUserDetails;
import org.sentilo.web.catalog.security.access.TenantAccessControlFilter;
import org.sentilo.web.catalog.security.service.CatalogUserDetailsService;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.authentication.AnonymousAuthenticationToken;
import org.springframework.security.web.access.WebInvocationPrivilegeEvaluator;

public class TenantAccessControlFilterTest {

  private final String publicRequest = "/sentilo-catalog-web/stats";
  private final String restrictedRequest = "/sentilo-catalog-web/admin/stats";
  private final String contextPath = "/sentilo-catalog-web";

  @InjectMocks
  private TenantAccessControlFilter filter;

  @Mock
  private CatalogUserDetailsService userDetailsService;

  @Mock
  private CatalogUserDetails userDetails;

  @Mock
  private HttpServletRequest req;

  @Mock
  private HttpServletResponse res;

  @Mock
  private FilterChain chain;

  @Mock
  private WebInvocationPrivilegeEvaluator requestEvaluator;

  @Mock
  private TenantContext tenantContext;

  @Before
  public void setUp() {
    System.setProperty(SentiloConstants.SENTILO_MULTITENANT_PROP_KEY, Boolean.TRUE.toString());

    MockitoAnnotations.initMocks(this);
    when(req.getContextPath()).thenReturn(contextPath);

    TenantContextHolder.setContext(tenantContext);

  }

  @After
  public void tearDown() {
    System.clearProperty(SentiloConstants.SENTILO_MULTITENANT_PROP_KEY);
    TenantContextHolder.clearContext();
  }

  @Test
  public void userAnonymousAccessToPublicPage() throws Exception {
    when(userDetailsService.getCatalogUserDetails()).thenReturn(null);

    filter.doFilter(req, res, chain);

    verify(chain).doFilter(req, res);
  }

  @Test
  public void userLoggedInAccessToPublicPage() throws Exception {
    when(userDetailsService.getCatalogUserDetails()).thenReturn(userDetails);
    when(req.getRequestURI()).thenReturn(publicRequest);
    when(requestEvaluator.isAllowed(eq("/stats"), any(AnonymousAuthenticationToken.class))).thenReturn(true);

    filter.doFilter(req, res, chain);

    verify(chain).doFilter(req, res);
  }

  @Test
  public void superAdminAccessToOwnPrivatePage() throws Exception {
    when(userDetailsService.getCatalogUserDetails()).thenReturn(userDetails);
    when(userDetails.isSuperAdminUser()).thenReturn(true);
    when(req.getRequestURI()).thenReturn(restrictedRequest);
    when(requestEvaluator.isAllowed(eq("/admin/stats"), any(AnonymousAuthenticationToken.class))).thenReturn(false);
    when(tenantContext.getCurrentTenant()).thenReturn(null);

    filter.doFilter(req, res, chain);

    verify(chain).doFilter(req, res);
  }

  @Test(expected = AccessDeniedException.class)
  public void superAdminAccessToForeignPrivatePage() throws Exception {
    when(userDetailsService.getCatalogUserDetails()).thenReturn(userDetails);
    when(userDetails.isSuperAdminUser()).thenReturn(true);
    when(req.getRequestURI()).thenReturn(restrictedRequest);
    when(requestEvaluator.isAllowed(eq("/admin/stats"), any(AnonymousAuthenticationToken.class))).thenReturn(false);
    when(tenantContext.getRequestTenant()).thenReturn("mockTenant");

    filter.doFilter(req, res, chain);
  }

  @Test
  public void userLoggedInAccessToOwnPrivatePage() throws Exception {
    when(userDetailsService.getCatalogUserDetails()).thenReturn(userDetails);
    when(userDetails.isSuperAdminUser()).thenReturn(false);
    when(req.getRequestURI()).thenReturn(restrictedRequest);
    when(requestEvaluator.isAllowed(eq("/admin/stats"), any(AnonymousAuthenticationToken.class))).thenReturn(false);
    when(tenantContext.getRequestTenant()).thenReturn("userTenant");
    when(userDetails.getTenantId()).thenReturn("userTenant");

    filter.doFilter(req, res, chain);

    verify(chain).doFilter(req, res);
  }

  @Test(expected = AccessDeniedException.class)
  public void userLoggedInAccessToForeignPrivatePage() throws Exception {
    when(userDetailsService.getCatalogUserDetails()).thenReturn(userDetails);
    when(userDetails.isSuperAdminUser()).thenReturn(false);
    when(req.getRequestURI()).thenReturn(restrictedRequest);
    when(requestEvaluator.isAllowed(eq("/admin/stats"), any(AnonymousAuthenticationToken.class))).thenReturn(false);
    when(tenantContext.getRequestTenant()).thenReturn("mockTenant");
    when(userDetails.getTenantId()).thenReturn("userTenant");

    filter.doFilter(req, res, chain);
  }
}
