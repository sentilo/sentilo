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
package org.sentilo.web.catalog.test.filter;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.io.DefaultResourceLoader;
import org.springframework.core.io.Resource;
import org.springframework.core.io.ResourceLoader;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;
import org.tuckey.web.filters.urlrewrite.Conf;
import org.tuckey.web.filters.urlrewrite.RewrittenUrl;
import org.tuckey.web.filters.urlrewrite.UrlRewriteWrappedResponse;
import org.tuckey.web.filters.urlrewrite.UrlRewriter;
import org.tuckey.web.filters.urlrewrite.utils.Log;

public class URLRewriteRulesTest {

  private static final Logger log = LoggerFactory.getLogger(URLRewriteRulesTest.class);
  private static final String REWRITE_CONF = "urlrewrite-test.xml";

  private UrlRewriter rewriter;
  private MockHttpServletRequest request;
  private MockHttpServletResponse response;

  /**
   * Setup the UrlRewriteFilter configuration.
   */
  @Before
  public void setUp() throws Exception {
    Log.setLevel("DEBUG"); // to make the RewriteFilter code log messages
    final ResourceLoader loader = new DefaultResourceLoader();
    final Resource confStream = loader.getResource("classpath:" + REWRITE_CONF);

    final Conf conf = new Conf(confStream.getInputStream(), REWRITE_CONF);
    rewriter = new UrlRewriter(conf);

    request = new MockHttpServletRequest();
    request.setContextPath("/sentilo-catalog-web");
  }

  @Test
  public void testRewriteMultitenantRequests() throws Exception {
    request.setAttribute("tenant-identifier", "tenant1/");
    request.setAttribute("f-tenant-identifier", "1");

    assertRewriteSuccess("/tenant1/component/map", "/component/map");
    assertRewriteSuccess("/tenant1/stats/", "/stats/");
    assertRewriteSuccess("/tenant1/stats/json", "/stats/json");
    assertRewriteSuccess("/tenant1/admin/user/list?tableName=users", "/admin/user/list?tableName=users");
  }

  @Test
  public void testNoRewriteSimpleRequests() throws Exception {
    assertNoRewrite("/component/map");
    assertNoRewrite("/stats/");
    assertNoRewrite("/admin/user/list?tableName=users");
  }

  @Test
  public void testNoRewriteMultitenantRequests() throws Exception {
    request.setAttribute("tenant-identifier", "tenant1/");
    request.setAttribute("f-tenant-identifier", "1");

    assertNoRewrite("/WEB-INF/sensor/sensor_detail.jsp");
    assertNoRewrite("/j_spring_security_check");
  }

  @Test
  public void testOutboundRules1() {
    // All outbound urls begin with context-path because urlEncoded has been applied before url
    // rewrite starts to inspect the response
    assertOutboundRewriteSuccess("/sentilo-catalog-web/", "/sentilo-catalog-web/tenant1/");
    assertOutboundRewriteSuccess("/sentilo-catalog-web/WEB-INF/sensor/sensor_detail.jsp", "/sentilo-catalog-web/WEB-INF/sensor/sensor_detail.jsp");
    assertOutboundRewriteSuccess("/sentilo-catalog-web/static/css/sentilo.css", "/sentilo-catalog-web/static/css/sentilo.css");
    assertOutboundRewriteSuccess("/sentilo-catalog-web/auth/login", "/sentilo-catalog-web/tenant1/auth/login");
    assertOutboundRewriteSuccess("/sentilo-catalog-web/component/map", "/sentilo-catalog-web/tenant1/component/map");

    assertOutboundRewriteSuccess(
        "/sentilo-catalog-web/admin/component/list/json?ts=1467722891236&page.page=1&page.size=10&page.sort=1&page.sort.dir=asc&sEcho=1&tableName=componentTable",
        "/sentilo-catalog-web/tenant1/admin/component/list/json?ts=1467722891236&page.page=1&page.size=10&page.sort=1&page.sort.dir=asc&sEcho=1&tableName=componentTable");

    assertOutboundRewriteSuccess("/sentilo-catalog-web/admin/component/list?nameTableRecover=componentTable&uar=1",
        "/sentilo-catalog-web/tenant2/admin/component/list?nameTableRecover=componentTable&uar=1");
    assertOutboundRewriteSuccess("/sentilo-catalog-web/admin/tenant/tenant2/detail?uar=1",
        "/sentilo-catalog-web/tenant2/admin/tenant/tenant2/detail?uar=1");
    assertOutboundRewriteSuccess("/sentilo-catalog-web/j_spring_security_logout", "/sentilo-catalog-web/tenant2/j_spring_security_logout");

  }

  private void assertOutboundRewriteSuccess(final String fromUrl, final String toUrl) {
    request.clearAttributes();
    request.setAttribute("tenant-identifier", "tenant1/");
    request.setAttribute("f-tenant-identifier", "1");
    request.setAttribute("user-tenant-identifier", "tenant2/");
    request.setAttribute("f-user-tenant-identifier", "1");
    request.setContextPath("/sentilo-catalog-web");
    // Outbound rules verifies check conditions over the request URL and check from match over the
    // outbound url
    request.setRequestURI("/WEB-INF/sensor/sensor_detail.jsp");

    response = new MockHttpServletResponse();

    final UrlRewriteWrappedResponse wrappedResponse = new UrlRewriteWrappedResponse(response, request, rewriter);

    final String result = wrappedResponse.encodeURL(fromUrl);
    log.debug("URL Rewrite from:[" + fromUrl + "] to [" + result + "]");
    Assert.assertEquals("Rewrite from:" + fromUrl + " to:" + toUrl + " did not succeed", toUrl, result);
  }

  private void assertRewriteSuccess(final String fromUrl, final String toUrl) throws Exception {
    // Clear possible previous attribute value
    request.removeAttribute("tenant-uri-modified");
    request.setRequestURI(fromUrl);
    response = new MockHttpServletResponse();

    final RewrittenUrl rewrittenUrl = rewriter.processRequest(request, response);

    Assert.assertNotNull("Could not rewrite URL from:" + fromUrl + " to:" + toUrl, rewrittenUrl);
    log.debug("URL Rewrite from:[" + fromUrl + "] to [" + rewrittenUrl.getTarget() + "]");
    Assert.assertEquals(toUrl, rewrittenUrl.getTarget());
    Assert.assertEquals("1", request.getAttribute("tenant-uri-modified"));
  }

  private void assertNoRewrite(final String fromUrl) throws Exception {
    // Clear possible previous attribute value
    request.removeAttribute("tenant-uri-modified");
    request.setRequestURI(fromUrl);
    response = new MockHttpServletResponse();

    final RewrittenUrl rewrittenUrl = rewriter.processRequest(request, response);

    Assert.assertNull("URL has been rewriten:" + fromUrl, rewrittenUrl);
    Assert.assertNull(request.getAttribute("tenant-uri-modified"));
  }

}
