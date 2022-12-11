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
package org.sentilo.web.catalog.interceptor;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.sentilo.common.enums.HttpHeader;
import org.sentilo.web.catalog.utils.CatalogUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.StringUtils;
import org.springframework.web.filter.AbstractRequestLoggingFilter;
import org.springframework.web.servlet.handler.HandlerInterceptorAdapter;

/**
 * Custom class that performs logging operations before a request is handled by a controller.
 */
public class RequestLoggingInterceptor extends HandlerInterceptorAdapter {

  private static final Logger LOGGER = LoggerFactory.getLogger(RequestLoggingInterceptor.class);

  public static final String DEFAULT_BEFORE_MESSAGE_PREFIX = "Incoming request [";

  public static final String DEFAULT_BEFORE_MESSAGE_SUFFIX = "]";

  /** By default, static requests are not logged */
  public boolean showStaticRequests = false;

  @Override
  public boolean preHandle(final HttpServletRequest request, final HttpServletResponse response, final Object handler) throws Exception {
    if (!CatalogUtils.isStaticRequest(request) || showStaticRequests) {
      LOGGER.info(createMessage(request));
    }
    return true;
  }

  /**
   * This method is based on {@link AbstractRequestLoggingFilter#createMessage} but differs in how
   * it gets the client address (takes X-Forwarded-For header into account) and that it writes also
   * the request method. *
   *
   * @param request
   * @param prefix
   * @param suffix
   * @return
   */
  protected String createMessage(final HttpServletRequest request) {
    final StringBuilder msg = new StringBuilder();
    msg.append(DEFAULT_BEFORE_MESSAGE_PREFIX);
    final String client = getRemoteClientAddress(request);
    if (StringUtils.hasLength(client)) {
      msg.append("remote_addr=").append(client);
    }

    msg.append(";request=").append(request.getMethod()).append(" ").append(request.getRequestURI());
    final String queryString = request.getQueryString();
    if (queryString != null) {
      msg.append('?').append(queryString);
    }

    msg.append(DEFAULT_BEFORE_MESSAGE_SUFFIX);
    return msg.toString();
  }

  private String getRemoteClientAddress(final HttpServletRequest request) {
    String remoteAddr = request.getHeader(HttpHeader.X_FORWARDED_FOR.name());
    if (!StringUtils.hasText(remoteAddr)) {
      remoteAddr = request.getRemoteAddr();
    } else {
      // The X-Forwarded-For request header takes the following form: client-ip-address,
      // proxy(0)-ip-address,...,proxy(N)-ip-address
      remoteAddr = remoteAddr.split(",")[0];
    }

    return remoteAddr;
  }
}
