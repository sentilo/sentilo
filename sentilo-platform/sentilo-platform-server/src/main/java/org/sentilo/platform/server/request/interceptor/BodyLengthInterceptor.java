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
package org.sentilo.platform.server.request.interceptor;

import org.sentilo.common.config.SentiloArtifactConfigService;
import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.platform.server.exception.MessageValidationException;
import org.sentilo.platform.server.request.SentiloRequest;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Component
public class BodyLengthInterceptor implements SentiloRequestHandlerInterceptor {

  private static final Logger LOGGER = LoggerFactory.getLogger(BodyLengthInterceptor.class);

  @Value("${api.body.max_length:0}")
  private long maxLength;

  @Autowired
  private SentiloArtifactConfigService serviceConfig;

  @Override
  public void invoke(final SentiloRequest request) throws MessageValidationException {
    if (apply(request)) {
      final long reqBodyLength = request.getContentLength();
      LOGGER.debug("Request content-length: {}", reqBodyLength);
      if (maxLength > 0 && reqBodyLength > maxLength) {
        final String message =
            String.format("Unable to read message. Request payload size (%s bytes) exceeds the limit (%s bytes)", reqBodyLength, maxLength);
        throw new MessageValidationException(message);
      }
    }
  }

  // body-length limit only applies to foreign inbound requests, i.e., does not apply to requests
  // from other Sentilo's modules
  // These requests are done by entity with id $catalog.id
  private boolean apply(final SentiloRequest request) {
    return !request.getEntitySource().equals(serviceConfig.getConfigValue("catalog.id", SentiloConstants.DEFAULT_CATALOG_ID));
  }

}
