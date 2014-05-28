/*
 * Sentilo
 * 
 * Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de Barcelona.
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

import org.apache.http.HttpStatus;
import org.sentilo.platform.common.domain.AdminInputMessage;
import org.sentilo.platform.common.domain.Statistics;
import org.sentilo.platform.common.domain.Subscription;
import org.sentilo.platform.common.exception.PlatformException;
import org.sentilo.platform.common.service.AdminService;
import org.sentilo.platform.server.exception.MessageValidationException;
import org.sentilo.platform.server.handler.AbstractHandler;
import org.sentilo.platform.server.parser.AdminParser;
import org.sentilo.platform.server.request.SentiloRequest;
import org.sentilo.platform.server.response.SentiloResponse;
import org.sentilo.platform.server.validation.AdminValidator;
import org.sentilo.platform.server.validation.RequestMessageValidator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.util.CollectionUtils;

/**
 * Methods published by this controller only could be invoked by the catalog user. These methods are
 * used to synchronize the data between the PubSub server and the catalog.
 */
@Controller
public class AdminHandler extends AbstractHandler {

  private final Logger logger = LoggerFactory.getLogger(AdminHandler.class);

  @Autowired
  private AdminService adminService;

  private AdminParser parser = new AdminParser();
  private final RequestMessageValidator<AdminInputMessage> validator = new AdminValidator();

  @Override
  public void onDelete(final SentiloRequest request, final SentiloResponse response) throws PlatformException {
    throw new PlatformException(HttpStatus.SC_METHOD_NOT_ALLOWED, "HTTP DELETE method not allowed for the requested resource");

  }

  @Override
  public void onGet(final SentiloRequest request, final SentiloResponse response) throws PlatformException {
    logger.debug("Executing admin GET request");
    debug(request);

    // Este metodo permite al catalogo recuperar informacion sobre el estado del servidor PubSub:
    // estadisticas, subscripciones, ...
    // El formato de las peticiones será:
    // 1. /admin/stats
    // 2. /admin/subscriptions/entityId
    // ...
    // en funcion de la peticion, la respuesta será una u otra.
    validateResourceNumberParts(request, 1, 2);
    validateApiAdminInvoke(request.getEntitySource());
    final AdminInputMessage inputMessage = parser.parseGetRequest(request);
    validator.validateRequestMessageOnGet(inputMessage);

    switch (inputMessage.getType()) {
      case stats:
        final Statistics stats = adminService.getStatistics();
        parser.writeStatsResponse(request, response, stats);
        break;
      case subscriptions:
        final List<Subscription> subscriptions = adminService.getSubscriptions(inputMessage.getEntity());
        parser.writeSubscriptionsResponse(request, response, subscriptions);
        break;
      default:
        throw new MessageValidationException(String.format("Request %s not supported", request.getUri()));

    }
  }

  @Override
  public void onPost(final SentiloRequest request, final SentiloResponse response) throws PlatformException {
    throw new PlatformException(HttpStatus.SC_METHOD_NOT_ALLOWED, "HTTP POST method not allowed for the requested resource");

  }

  @Override
  public void onPut(final SentiloRequest request, final SentiloResponse response) throws PlatformException {
    logger.debug("Executing admin PUT request");
    debug(request);

    // Este metodo permite actualizar la información de Redis a partir de la actualización llevada a
    // cabo en el catalago

    validateResourceNumberParts(request, 1, 1);
    validateApiAdminInvoke(request.getEntitySource());
    final AdminInputMessage inputMessage = parser.parsePutRequest(request);
    validator.validateRequestMessageOnPut(inputMessage);
    logger.debug("Type message: {}", inputMessage.getType());
    logger.debug("Sensors: {}", (!CollectionUtils.isEmpty(inputMessage.getSensors()) ? inputMessage.getSensors().size() : 0));
    logger.debug("Providers: {}", (!CollectionUtils.isEmpty(inputMessage.getProviders()) ? inputMessage.getProviders().size() : 0));
    switch (inputMessage.getType()) {
      case delete:
        adminService.delete(inputMessage);
        break;
      default:
        throw new MessageValidationException(String.format("Request %s not supported", request.getUri()));

    }

  }

  public void setAdminService(final AdminService adminService) {
    this.adminService = adminService;
  }

  public void setAdminParser(final AdminParser parser) {
    this.parser = parser;
  }

}
