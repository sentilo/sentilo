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
import org.sentilo.common.enums.SubscribeType;
import org.sentilo.platform.common.domain.AlarmSubscription;
import org.sentilo.platform.common.domain.SubscribeInputMessage;
import org.sentilo.platform.common.domain.Subscription;
import org.sentilo.platform.common.service.AlarmService;
import org.sentilo.platform.common.service.SubscribeService;
import org.sentilo.platform.server.converter.SubscribeConverter;
import org.sentilo.platform.server.exception.MethodNotAllowedException;
import org.sentilo.platform.server.handler.AbstractHandler;
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

  private static final Logger LOGGER = LoggerFactory.getLogger(SubscribeHandler.class);

  @Autowired
  private SubscribeService subscribeService;

  @Autowired
  private AlarmService alarmService;

  private SubscribeConverter parser = new SubscribeConverter();
  private final RequestMessageValidator<SubscribeInputMessage> validator = new SubscribeValidator();

  @Override
  public void onDelete(final SentiloRequest request, final SentiloResponse response) {
    LOGGER.debug("Executing subscribe DELETE request");
    debug(request);
    // The request follows the following pattern:
    // DEL /subscribe/{eventType}/{resourceId}
    // where all parameters are not mandatory. Furthermore, {resourceId} may be composed by
    // providerId and sensorId.

    validateResourceNumberParts(request, 0, 3);
    final Subscription subscription = parser.parseBasicRequest(request);
    validator.validateRequestMessageOnDelete(new SubscribeInputMessage(subscription));
    // Only own subscriptions could be deleted by this action, so it is not necessary to validate
    // the authorization

    subscribeService.remove(subscription);
  }

  @Override
  public void onGet(final SentiloRequest request, final SentiloResponse response) {
    LOGGER.debug("Executing subscribe GET request");
    debug(request);

    // The request follows the following pattern:
    // GET /subscribe/{eventType}
    // where {eventType} parameter is not mandatory.

    validateResourceNumberParts(request, 0, 1);
    final Subscription subscription = parser.parseBasicRequest(request);
    validator.validateRequestMessageOnGet(new SubscribeInputMessage(subscription));
    // Only own subscriptions could be returned by this action, so it is not necessary to validate
    // the authorization

    final List<Subscription> subscriptions = subscribeService.get(subscription);
    parser.writeResponse(response, subscriptions);
  }

  @Override
  public void onPost(final SentiloRequest request, final SentiloResponse response) {
    throw new MethodNotAllowedException(HttpMethod.POST);
  }

  @Override
  public void onPut(final SentiloRequest request, final SentiloResponse response) {
    LOGGER.debug("Executing subscribe PUT request");
    debug(request);

    // The request follows the following pattern:
    // PUT /subscribe/{eventType}/{resourceId}
    // where {resourceId} may be composite (e.g. <providerId>/<sensorId>)

    validateResourceNumberParts(request, 2, 3);
    final Subscription subscription = parser.parseRequest(request);
    validator.validateRequestMessageOnPut(new SubscribeInputMessage(subscription));
    if (subscription.getType().equals(SubscribeType.ALARM)) {
      subscription.setOwnerEntityId(alarmService.getAlertOwner(((AlarmSubscription) subscription).getAlertId()));
    }

    validateReadAccess(request.getEntitySource(), subscription.getOwnerEntityId());

    subscribeService.subscribe(subscription);
  }

}
