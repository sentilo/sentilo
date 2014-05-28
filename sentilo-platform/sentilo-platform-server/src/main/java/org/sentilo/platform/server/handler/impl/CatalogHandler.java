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

import org.sentilo.common.domain.CatalogDeleteInputMessage;
import org.sentilo.common.domain.CatalogInputMessage;
import org.sentilo.common.domain.CatalogResponseMessage;
import org.sentilo.platform.common.exception.PlatformException;
import org.sentilo.platform.common.service.CatalogService;
import org.sentilo.platform.server.exception.CatalogErrorException;
import org.sentilo.platform.server.handler.AbstractHandler;
import org.sentilo.platform.server.parser.CatalogParser;
import org.sentilo.platform.server.request.SentiloRequest;
import org.sentilo.platform.server.response.SentiloResponse;
import org.sentilo.platform.server.validation.CatalogValidator;
import org.sentilo.platform.server.validation.RequestMessageValidator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.util.StringUtils;

@Controller
public class CatalogHandler extends AbstractHandler {

  private final Logger logger = LoggerFactory.getLogger(CatalogHandler.class);

  @Autowired
  private CatalogService catalogService;

  private CatalogParser parser = new CatalogParser();
  private final RequestMessageValidator<CatalogInputMessage> validator = new CatalogValidator();

  @Override
  public void onDelete(final SentiloRequest request, final SentiloResponse response) throws PlatformException {
    logger.debug("Executing catalog DELETE request");
    debug(request);

    doRealOnDelete(request, false);
  }

  @Override
  public void onGet(final SentiloRequest request, final SentiloResponse response) throws PlatformException {
    logger.debug("Executing catalog GET request");
    debug(request);

    // La peticion sólo puede ser de la siguiente manera
    // GET /catalog
    // Ademas, puede haber parametros en la URL

    validateResourceNumberParts(request, 0, 0);
    final CatalogInputMessage inputMessage = parser.parseGetRequest(request);
    validator.validateRequestMessageOnGet(inputMessage);
    // Aqui no tiene sentido hacer ninguna validación de autorizacion ya que se aplica sobre la
    // misma entidad que hace la peticion
    final CatalogResponseMessage responseMessage = catalogService.getAuthorizedProviders(inputMessage);
    checkCatalogResponseMessage(responseMessage);

    parser.writeResponse(request, response, responseMessage);

  }

  @Override
  public void onPost(final SentiloRequest request, final SentiloResponse response) throws PlatformException {
    logger.debug("Executing catalog POST request");
    debug(request);

    // La peticion sólo puede ser de la siguiente manera
    // POST /catalog/provider_id

    validateResourceNumberParts(request, 1, 1);
    final CatalogInputMessage inputMessage = parser.parsePostRequest(request);
    validator.validateRequestMessageOnPost(inputMessage);
    validateAdminAcess(request.getEntitySource(), inputMessage.getProviderId());
    // Redirigir peticion al catálogo --> Cliente REST para el catálogo.
    final CatalogResponseMessage responseMessage = catalogService.insertSensors(inputMessage);
    checkCatalogResponseMessage(responseMessage);

  }

  @Override
  public void onPut(final SentiloRequest request, final SentiloResponse response) throws PlatformException {
    // Tanto las peticiones de actualizacion como las de borrado pueden acabar recibiendose via una
    // llamada HTTP PUT ya que el metodo HTTP DELETE no es implementado/soportado por todos los
    // clientes o bien no soportan el envío de un body junto a la petición de tipo DELETE.
    // En estos casos, utilizaremos el parametro method para indicar que se está simulando una
    // llamada DELETE (method=delete)

    logger.debug("Executing catalog PUT request");
    debug(request);
    final String method = request.getRequestParameter("method");
    if (StringUtils.hasText(method) && method.equals("delete")) {
      doRealOnDelete(request, true);
    } else {
      doRealOnPut(request);
    }
  }

  private void doRealOnDelete(final SentiloRequest request, final boolean simulate) throws PlatformException {
    logger.debug("Executing catalog DELETE request");
    debug(request);

    // La peticion sólo puede ser de la sigiente manera
    // DELETE /catalog/provider_id

    validateResourceNumberParts(request, 1, 1);
    final CatalogDeleteInputMessage inputMessage = (CatalogDeleteInputMessage) parser.parseDeleteRequest(request, simulate);
    validator.validateRequestMessageOnDelete(inputMessage);
    validateAdminAcess(request.getEntitySource(), inputMessage.getProviderId());

    final CatalogResponseMessage responseMessage = catalogService.deleteProvider(inputMessage);
    checkCatalogResponseMessage(responseMessage);
  }

  private void doRealOnPut(final SentiloRequest request) throws PlatformException {
    logger.debug("Executing catalog PUT request");
    debug(request);

    // La peticion sólo puede ser de la sigiente manera
    // PUT /catalog/provider_id

    validateResourceNumberParts(request, 1, 1);
    final CatalogInputMessage inputMessage = parser.parsePutRequest(request);
    validator.validateRequestMessageOnPut(inputMessage);
    validateAdminAcess(request.getEntitySource(), inputMessage.getProviderId());
    // Redirigir peticion al catálogo --> Cliente REST para el catálogo.
    final CatalogResponseMessage responseMessage = catalogService.updateSensorsOrComponents(inputMessage);
    checkCatalogResponseMessage(responseMessage);
  }

  private void checkCatalogResponseMessage(final CatalogResponseMessage responseMessage) throws CatalogErrorException {
    if (!responseMessage.getCode().equals(CatalogResponseMessage.OK)) {
      throw new CatalogErrorException(responseMessage.getCode(), responseMessage.getErrorMessage(), responseMessage.getErrorDetails());
    }
  }

  public void setCatalogService(final CatalogService catalogService) {
    this.catalogService = catalogService;
  }

  public void setCatalogParser(final CatalogParser parser) {
    this.parser = parser;
  }
}
