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
import org.sentilo.platform.common.domain.Order;
import org.sentilo.platform.common.domain.OrderInputMessage;
import org.sentilo.platform.common.exception.PlatformException;
import org.sentilo.platform.common.service.OrderService;
import org.sentilo.platform.server.handler.AbstractHandler;
import org.sentilo.platform.server.parser.OrderParser;
import org.sentilo.platform.server.request.SentiloRequest;
import org.sentilo.platform.server.response.SentiloResponse;
import org.sentilo.platform.server.validation.OrderValidator;
import org.sentilo.platform.server.validation.RequestMessageValidator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;

@Controller
public class OrderHandler extends AbstractHandler {

  private final Logger logger = LoggerFactory.getLogger(OrderHandler.class);
  @Autowired
  private OrderService orderService;

  private OrderParser parser = new OrderParser();
  private final RequestMessageValidator<OrderInputMessage> validator = new OrderValidator();

  @Override
  public void onDelete(final SentiloRequest request, final SentiloResponse response) throws PlatformException {
    throw new PlatformException(HttpStatus.SC_METHOD_NOT_ALLOWED, "HTTP DELETE method not allowed for the requested resource");
  }

  @Override
  public void onGet(final SentiloRequest request, final SentiloResponse response) throws PlatformException {
    logger.debug("Executing order GET request");
    debug(request);

    // La peticion puede ser de dos tipos
    // GET /order/idProv
    // GET /order/idProv/idSensor
    // Ademas, en ambos casos pueden haber parametros en la URL

    validateResourceNumberParts(request, 1, 2);
    final OrderInputMessage inputMessage = parser.parseGetRequest(request);
    validator.validateRequestMessageOnGet(inputMessage);
    validateReadAccess(request.getEntitySource(), inputMessage.getProviderId());
    final List<Order> lastOrders = orderService.getLastOrders(inputMessage);

    parser.writeResponse(request, response, lastOrders);
  }

  @Override
  public void onPost(final SentiloRequest request, final SentiloResponse response) throws PlatformException {
    throw new PlatformException(HttpStatus.SC_METHOD_NOT_ALLOWED, "HTTP POST method not allowed for the requested resource");
  }

  @Override
  public void onPut(final SentiloRequest request, final SentiloResponse response) throws PlatformException {
    logger.debug("Executing order PUT request");
    debug(request);

    // La peticion tiene el siguiente formato
    // PUT /order/providerId/sensorId con sensorId opcional
    // En el body del mensaje tiene que venir la expresion de la orden

    validateResourceNumberParts(request, 1, 2);
    final OrderInputMessage inputMessage = parser.parseRequest(request);
    validator.validateRequestMessageOnPut(inputMessage);
    validateWriteAccess(request.getEntitySource(), inputMessage.getProviderId());
    orderService.setOrder(inputMessage);
  }

  public void setOrderService(final OrderService orderService) {
    this.orderService = orderService;
  }

  public void setOrderParser(final OrderParser parser) {
    this.parser = parser;
  }
}
