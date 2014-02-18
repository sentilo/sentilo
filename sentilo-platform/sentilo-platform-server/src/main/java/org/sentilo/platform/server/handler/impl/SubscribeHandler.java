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
import org.sentilo.common.domain.SubscribeType;
import org.sentilo.platform.common.domain.AlarmSubscription;
import org.sentilo.platform.common.domain.SubscribeInputMessage;
import org.sentilo.platform.common.domain.Subscription;
import org.sentilo.platform.common.exception.PlatformException;
import org.sentilo.platform.common.service.AlarmService;
import org.sentilo.platform.common.service.SubscribeService;
import org.sentilo.platform.server.handler.AbstractHandler;
import org.sentilo.platform.server.parser.SubscribeParser;
import org.sentilo.platform.server.request.SentiloRequest;
import org.sentilo.platform.server.response.SentiloResponse;
import org.sentilo.platform.server.validation.RequestMessageValidator;
import org.sentilo.platform.server.validation.SubscribeValidator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;

@Controller
public class SubscribeHandler extends AbstractHandler {

  private final Logger logger = LoggerFactory.getLogger(SubscribeHandler.class);

  @Autowired
  private SubscribeService subscribeService;

  @Autowired
  private AlarmService alarmService;

  private SubscribeParser parser = new SubscribeParser();
  private final RequestMessageValidator<SubscribeInputMessage> validator = new SubscribeValidator();

  @Override
  public void onDelete(final SentiloRequest request, final SentiloResponse response) throws PlatformException {
    logger.debug("Executing subscribe DELETE request");
    debug(request);
    // La peticion puede ser:
    // DEL /subscribe
    // DEL /subscribe/eventType
    // DEL /subscribe/eventType/resourceId con resourceId pudiendo ser compuesto por 2 ids (caso
    // provider+sensor)

    validateResourceNumberParts(request, 0, 3);
    final Subscription subscription = parser.parseBasicRequest(request);
    validator.validateRequestMessageOnDelete(new SubscribeInputMessage(subscription));
    // En este caso no hace falta validar la autorizacion ya que el remove solo se aplica sobre las
    // subscripciones de quien hace la peticion
    subscribeService.remove(subscription);
  }

  @Override
  public void onGet(final SentiloRequest request, final SentiloResponse response) throws PlatformException {
    logger.debug("Executing subscribe GET request");
    debug(request);

    // La peticion puede ser:
    // GET /subscribe
    // GET /subscribe/eventType

    validateResourceNumberParts(request, 0, 1);
    final Subscription subscription = parser.parseBasicRequest(request);
    validator.validateRequestMessageOnGet(new SubscribeInputMessage(subscription));
    // En este caso no hace falta validar la autorizacion ya que el get solo retorna subscripciones
    // de quien hace la peticion
    final List<Subscription> subscriptions = subscribeService.get(subscription);
    parser.writeResponse(response, subscriptions);
  }

  @Override
  public void onPost(final SentiloRequest request, final SentiloResponse response) throws PlatformException {
    throw new PlatformException(HttpStatus.SC_METHOD_NOT_ALLOWED, "HTTP POST method not allowed for the requested resource");
  }

  @Override
  public void onPut(final SentiloRequest request, final SentiloResponse response) throws PlatformException {
    logger.debug("Executing subscribe PUT request");
    debug(request);

    // En este caso, si la peticion no tiene informado el tipo se entiende que este es DATA.
    // Igualmente, el path siempre debe tener informado almenos un token (evento o resourceId).
    validateResourceNumberParts(request, 1, 3);
    final Subscription subscription = parser.parseRequest(request, SubscribeType.DATA);
    validator.validateRequestMessageOnPut(new SubscribeInputMessage(subscription));
    if (subscription.getType().equals(SubscribeType.ALARM)) {
      subscription.setOwnerEntityId(alarmService.getAlertOwner(((AlarmSubscription) subscription).getAlertId()));
    }
    // Validamos que quien hace la peticion de subscripcion tiene permiso de lectura sobre la
    // entidad propietaria del recurso (data, order o alarm)
    validateReadAccess(request.getEntitySource(), subscription.getOwnerEntityId());
    subscribeService.subscribe(subscription);
  }

  public void setSubscribeService(final SubscribeService subscribeService) {
    this.subscribeService = subscribeService;
  }

  public void setSubscribeParser(final SubscribeParser parser) {
    this.parser = parser;
  }
}
