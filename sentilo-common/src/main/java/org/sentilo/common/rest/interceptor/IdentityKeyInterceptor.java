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
package org.sentilo.common.rest.interceptor;

import java.io.IOException;

import org.apache.http.Header;
import org.apache.http.HttpException;
import org.apache.http.HttpRequest;
import org.apache.http.HttpRequestInterceptor;
import org.apache.http.protocol.HttpContext;
import org.sentilo.common.utils.RESTUtils;
import org.sentilo.common.utils.SentiloConstants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.Assert;

public class IdentityKeyInterceptor implements HttpRequestInterceptor {

  private static final Logger LOGGER = LoggerFactory.getLogger(IdentityKeyInterceptor.class);

  private Header identityHeader;

  /*
   * (non-Javadoc)
   *
   * @see org.apache.http.HttpRequestInterceptor#process(org.apache.http.HttpRequest,
   * org.apache.http.protocol.HttpContext)
   */
  @Override
  public void process(final HttpRequest request, final HttpContext context) throws HttpException, IOException {
    LOGGER.debug("Adding header {} with value {}", identityHeader.getName(), identityHeader.getValue());
    if (!request.containsHeader(SentiloConstants.IDENTITY_KEY_HEADER)) {
      request.addHeader(identityHeader);
    }
  }

  public void setIdentityToken(final String identityToken) {
    Assert.notNull(identityToken, "IdentityToken must not be null");
    identityHeader = RESTUtils.buildIdentityHeader(identityToken);
  }

}
