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

import org.sentilo.common.enums.HttpMethod;
import org.sentilo.platform.common.domain.DataInputMessage;
import org.sentilo.platform.common.domain.Observation;
import org.sentilo.platform.common.service.DataService;
import org.sentilo.platform.server.converter.DataConverter;
import org.sentilo.platform.server.exception.MethodNotAllowedException;
import org.sentilo.platform.server.handler.AbstractHandler;
import org.sentilo.platform.server.request.SentiloRequest;
import org.sentilo.platform.server.response.SentiloResponse;
import org.sentilo.platform.server.validation.DataValidator;
import org.sentilo.platform.server.validation.RequestMessageValidator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.util.StringUtils;

@Controller
public class DataHandler extends AbstractHandler {

  private static final Logger LOGGER = LoggerFactory.getLogger(DataHandler.class);

  @Autowired
  private DataService dataService;

  private DataConverter parser = new DataConverter();
  private final RequestMessageValidator<DataInputMessage> validator = new DataValidator();

  @Override
  public void onDelete(final SentiloRequest request, final SentiloResponse response) {
    LOGGER.debug("Executing data DELETE request");
    debug(request);

    // The request follows the following pattern:
    // DELETE /data/{providerId}/{sensorId}
    // where URI parameter {sensorId} is not mandatory

    validateResourceNumberParts(request, 1, 2);
    final DataInputMessage inputMessage = parser.parseDeleteRequest(request);
    validator.validateRequestMessageOnDelete(inputMessage);
    validateWriteAccess(request.getEntitySource(), inputMessage.getProviderId());

    dataService.deleteLastObservations(inputMessage);
  }

  @Override
  public void onGet(final SentiloRequest request, final SentiloResponse response) {
    LOGGER.debug("Executing data GET request");
    debug(request);

    // The request follows the following pattern:
    // GET /data/{providerId}/{sensorId}
    // where URI parameter {sensorId} is not mandatory
    // Furthermore, it could have parameters

    validateResourceNumberParts(request, 1, 2);
    final DataInputMessage inputMessage = parser.parseGetRequest(request);
    validator.validateRequestMessageOnGet(inputMessage);
    validateReadAccess(request.getEntitySource(), inputMessage.getProviderId());

    final List<Observation> lastObservations = dataService.getLastObservations(inputMessage);

    parser.writeResponse(request, response, lastObservations);
  }

  @Override
  public void onPost(final SentiloRequest request, final SentiloResponse response) {
    final String method = request.getRequestParameter("method");
    if (StringUtils.hasText(method) && "put".equals(method)) {
      onPut(request, response);
    } else {
      throw new MethodNotAllowedException(HttpMethod.POST);
    }
  }

  @Override
  public void onPut(final SentiloRequest request, final SentiloResponse response) {
    LOGGER.debug("Executing data PUT request");
    debug(request);

    // The request follows the following pattern:
    // PUT /data/{providerId}/{sensorId}/{sensorValue}
    // where URI parameters {sensorId} and {sensorValue} are not mandatory.
    // If {sensorId} is not filled in then request will have a message on the body

    validateResourceNumberParts(request, 1, 3);
    final DataInputMessage inputMessage = parser.parsePutRequest(request);
    validator.validateRequestMessageOnPut(inputMessage);
    validateWriteAccess(request.getEntitySource(), inputMessage.getProviderId());

    LOGGER.info("Entity {} has published a message with {} observations associated with {} sensors", request.getEntitySource(),
        inputMessage.getObservations().size(), inputMessage.getProviderId());

    dataService.setObservations(inputMessage);
  }

}
