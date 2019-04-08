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

import java.util.List;

import org.sentilo.common.domain.CatalogAlert;
import org.sentilo.common.domain.CatalogAlertInputMessage;
import org.sentilo.common.domain.CatalogResponseMessage;
import org.sentilo.platform.common.service.CatalogService;
import org.sentilo.platform.server.converter.CatalogAlertConverter;
import org.sentilo.platform.server.exception.CatalogErrorException;
import org.sentilo.platform.server.exception.ForbiddenAccessException;
import org.sentilo.platform.server.handler.AbstractHandler;
import org.sentilo.platform.server.request.SentiloRequest;
import org.sentilo.platform.server.response.SentiloResponse;
import org.sentilo.platform.server.validation.CatalogAlertValidator;
import org.sentilo.platform.server.validation.RequestMessageValidator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

import com.google.common.base.Function;
import com.google.common.collect.Multimap;
import com.google.common.collect.Multimaps;

@Controller
public class CatalogAlertHandler extends AbstractHandler {

  private static final Logger LOGGER = LoggerFactory.getLogger(CatalogAlertHandler.class);

  @Autowired
  private CatalogService catalogService;

  private CatalogAlertConverter parser = new CatalogAlertConverter();
  private RequestMessageValidator<CatalogAlertInputMessage> validator = new CatalogAlertValidator();

  @Override
  public void onDelete(final SentiloRequest request, final SentiloResponse response) {
    LOGGER.debug("Executing catalog alert DELETE request");
    debug(request);

    doRealOnDelete(request, false);
  }

  @Override
  public void onGet(final SentiloRequest request, final SentiloResponse response) {
    LOGGER.debug("Executing catalog alert GET request");
    debug(request);

    // The request follows the following pattern:
    // GET /catalog/alert/{entityId}
    // where {entityId} is not mandatory.
    // Furthermore, it could have parameters used to filter the alerts on the response

    validateResourceNumberParts(request, 0, 1);
    final CatalogAlertInputMessage inputMessage = parser.parseGetRequest(request);
    validator.validateRequestMessageOnGet(inputMessage);
    validateAdminAcess(request.getEntitySource(), inputMessage.getEntityId());

    final CatalogResponseMessage responseMessage = catalogService.getAuthorizedAlerts(inputMessage);
    checkCatalogResponseMessage(responseMessage);

    parser.writeResponse(response, responseMessage);
  }

  @Override
  public void onPost(final SentiloRequest request, final SentiloResponse response) {
    LOGGER.debug("Executing catalog alert POST request");
    debug(request);

    // The request follows the following pattern:
    // POST /catalog/alert/{entityId}
    // where {entityId} is not mandatory and there must be a message on the body.

    validateResourceNumberParts(request, 0, 1);
    final CatalogAlertInputMessage inputMessage = parser.parsePostRequest(request);
    validator.validateRequestMessageOnPost(inputMessage);
    validateAdminAcess(request.getEntitySource(), inputMessage.getEntityId());
    validateAuthorization(inputMessage, request);

    final CatalogResponseMessage responseMessage = catalogService.insertAlerts(inputMessage);
    checkCatalogResponseMessage(responseMessage);
  }

  @Override
  public void onPut(final SentiloRequest request, final SentiloResponse response) {
    LOGGER.debug("Executing catalog alert PUT request");
    debug(request);

    final String method = request.getRequestParameter("method");
    if (StringUtils.hasText(method) && "delete".equals(method)) {
      doRealOnDelete(request, true);
    } else {
      doRealOnPut(request);
    }
  }

  private void doRealOnDelete(final SentiloRequest request, final boolean simulate) {
    LOGGER.debug("Executing catalog alert DELETE request");
    debug(request);

    // The request follows the following pattern:
    // DELETE /catalog/alert/{entityId}
    // where {entityId} is not mandatory

    validateResourceNumberParts(request, 0, 1);
    final CatalogAlertInputMessage inputMessage = parser.parseDeleteRequest(request, simulate);
    validator.validateRequestMessageOnDelete(inputMessage);
    validateAdminAcess(request.getEntitySource(), inputMessage.getEntityId());

    final CatalogResponseMessage responseMessage = catalogService.deleteAlerts(inputMessage);
    checkCatalogResponseMessage(responseMessage);
  }

  private void doRealOnPut(final SentiloRequest request) {
    LOGGER.debug("Executing catalog alert PUT request");
    debug(request);

    // The request follows the following pattern:
    // PUT /catalog/alert/{entityId}
    // where {entityId} is not mandatory and there must be a message on the body.

    validateResourceNumberParts(request, 0, 1);
    final CatalogAlertInputMessage inputMessage = parser.parsePutRequest(request);
    validator.validateRequestMessageOnPut(inputMessage);
    validateAdminAcess(request.getEntitySource(), inputMessage.getEntityId());
    validateAuthorization(inputMessage, request);

    final CatalogResponseMessage responseMessage = catalogService.updateAlerts(inputMessage);
    checkCatalogResponseMessage(responseMessage);
  }

  protected void validateAuthorization(final CatalogAlertInputMessage inputMessage, final SentiloRequest request) throws ForbiddenAccessException {
    // Internal alerts only could be inserted/updated by catalog entity
    final Multimap<String, CatalogAlert> groups = groupAlertsByType(inputMessage.getAlerts());
    if (!CollectionUtils.isEmpty(groups.get("INTERNAL")) && !getCatalogId().equals(request.getEntitySource())) {
      final String errorMessage = String.format("You are not authorized to insert/update internal alerts.");
      throw new ForbiddenAccessException(errorMessage);
    }
  }

  /**
   * Group alerts list by type (INTERNAL or EXTERNAL)
   *
   * @param alerts
   * @return
   */
  protected Multimap<String, CatalogAlert> groupAlertsByType(final List<CatalogAlert> alerts) {
    final Function<CatalogAlert, String> internalFunction = new Function<CatalogAlert, String>() {

      @Override
      public String apply(final CatalogAlert alert) {
        // alert type could be null if it is wrong, so return empty string if it is null
        return alert.getType() != null ? alert.getType() : "";
      }
    };
    return Multimaps.index(alerts, internalFunction);
  }

  private void checkCatalogResponseMessage(final CatalogResponseMessage responseMessage) throws CatalogErrorException {
    if (!responseMessage.getCode().equals(CatalogResponseMessage.OK)) {
      throw new CatalogErrorException(responseMessage.getCode(), responseMessage.getErrorMessage(), responseMessage.getErrorDetails());
    }
  }
}
