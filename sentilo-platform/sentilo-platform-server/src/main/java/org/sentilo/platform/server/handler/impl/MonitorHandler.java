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
package org.sentilo.platform.server.handler.impl;

import org.sentilo.common.domain.PlatformConfigMessage;
import org.sentilo.common.enums.HttpMethod;
import org.sentilo.common.metrics.SentiloArtifactsMetricsMessage;
import org.sentilo.platform.common.domain.AdminInputMessage;
import org.sentilo.platform.common.ratelimiter.RateLimiterStatus;
import org.sentilo.platform.common.ratelimiter.service.RateLimiterService;
import org.sentilo.platform.common.service.AdminService;
import org.sentilo.platform.common.service.MonitorService;
import org.sentilo.platform.server.converter.AdminConverter;
import org.sentilo.platform.server.exception.MessageValidationException;
import org.sentilo.platform.server.exception.MethodNotAllowedException;
import org.sentilo.platform.server.handler.AbstractHandler;
import org.sentilo.platform.server.request.SentiloRequest;
import org.sentilo.platform.server.response.SentiloResponse;
import org.sentilo.platform.server.validation.AdminValidator;
import org.sentilo.platform.server.validation.RequestMessageValidator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Controller;

@Controller
public class MonitorHandler extends AbstractHandler {

  private static final Logger LOGGER = LoggerFactory.getLogger(MonitorHandler.class);

  @Autowired
  private AdminService adminService;

  @Autowired
  private MonitorService monitorService;

  @Autowired
  @Qualifier("inboundRateLimiting")
  private RateLimiterService rateLimitingService;

  private final AdminConverter parser = new AdminConverter();
  private final RequestMessageValidator<AdminInputMessage> validator = new AdminValidator();

  @Override
  public void onDelete(final SentiloRequest request, final SentiloResponse response) {
    throw new MethodNotAllowedException(HttpMethod.DELETE);
  }

  @Override
  public void onGet(final SentiloRequest request, final SentiloResponse response) {
    LOGGER.debug("Executing monitor GET request");
    debug(request);

    validateResourceNumberParts(request, 1, 2);
    validateApiAdminInvoke(request.getEntitySource());
    final AdminInputMessage inputMessage = parser.parseGetRequest(request);
    validator.validateRequestMessageOnGet(inputMessage);

    switch (inputMessage.getType()) {
      case config:
        final PlatformConfigMessage config = adminService.getPlatformConfig();
        parser.writeResponse(response, config);
        break;
      case metrics:
        final SentiloArtifactsMetricsMessage artifactsMetrics = adminService.getSentiloArtifactsMetrics();
        parser.writeResponse(response, artifactsMetrics);
        break;
      case rl_input_status:
        final RateLimiterStatus rateLimitingState = rateLimitingService.getAccountsStatus();
        parser.writeResponse(response, rateLimitingState);
        break;
      case ping:
        break;
      default:
        throw new MessageValidationException(String.format("Request %s not supported", request.getUri()));
    }
  }

  @Override
  public void onPost(final SentiloRequest request, final SentiloResponse response) {
    LOGGER.debug("Executing monitor POST request");
    debug(request);

    validateResourceNumberParts(request, 1, 2);
    validateApiAdminInvoke(request.getEntitySource());

    final String action = request.getResourcePart(0);

    switch (action) {
      case "restart":
        monitorService.restartServer(false);
        break;
      case "force-restart":
        monitorService.restartServer(true);
        break;
      case "config":
        break;
      default:
        throw new MessageValidationException(String.format("Request %s not supported", request.getUri()));
    }

  }

  @Override
  public void onPut(final SentiloRequest request, final SentiloResponse response) {
    throw new MethodNotAllowedException(HttpMethod.PUT);
  }
}
