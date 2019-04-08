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

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.security.web.DefaultRedirectStrategy;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.web.util.UriComponentsBuilder;

/**
 * Redirect strategy aimed to modify a redirect URL when the request was initiated in a secured
 * channel (identified by the x-forwarded-proto header).
 *
 */
@Component
public class SentiloRedirectStrategy extends DefaultRedirectStrategy {

  private final static String FORWARD_PROTO_HEADER = "x-forwarded-proto";
  private final static String FORWARD_HOST_HEADER = "x-forwarded-host";
  private final static String HTTPS_SCHEME = "https";

  private static final Logger LOGGER = LoggerFactory.getLogger(SentiloRedirectStrategy.class);

  @Override
  public void sendRedirect(final HttpServletRequest request, final HttpServletResponse response, final String url) throws IOException {
    final String redirectUrl = buildRedirectUrl(request, url);
    super.sendRedirect(request, response, redirectUrl);
  }

  public String buildRedirectUrl(final HttpServletRequest request, final String url) {
    String redirectUrl = url;
    if (isReverseProxyRequest(request)) {
      final String header = request.getHeader(FORWARD_PROTO_HEADER);
      final boolean isHttps = StringUtils.hasText(header) && HTTPS_SCHEME.equalsIgnoreCase(header);

      // Sentilo url redirects are all them relative to context-path, so they don't begin with
      // context-path or schema.
      if (isHttps) {
        final String newScheme = HTTPS_SCHEME;
        final String host = request.getHeader(FORWARD_HOST_HEADER);

        final UriComponentsBuilder ucb = UriComponentsBuilder.fromPath(request.getContextPath());
        ucb.path(url);
        ucb.scheme(newScheme);
        ucb.host(host);
        ucb.build();

        redirectUrl = ucb.build().toUriString();

        LOGGER.debug("Redirect URL {} to {}", url, redirectUrl);
      }
    }

    return redirectUrl;
  }

  /**
   * Return true/false depends on request was forwarded to Sentilo from a reverse proxy.
   *
   * @param request
   * @return
   */
  private boolean isReverseProxyRequest(final HttpServletRequest request) {
    final String protoHeader = request.getHeader(FORWARD_PROTO_HEADER);
    final String hostHeader = request.getHeader(FORWARD_HOST_HEADER);
    return StringUtils.hasText(hostHeader) && StringUtils.hasText(protoHeader);
  }
}
