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
import org.sentilo.platform.common.domain.Alarm;
import org.sentilo.platform.common.domain.AlarmInputMessage;
import org.sentilo.platform.common.service.AlarmService;
import org.sentilo.platform.server.converter.AlarmConverter;
import org.sentilo.platform.server.exception.MethodNotAllowedException;
import org.sentilo.platform.server.handler.AbstractHandler;
import org.sentilo.platform.server.request.SentiloRequest;
import org.sentilo.platform.server.response.SentiloResponse;
import org.sentilo.platform.server.validation.AlarmValidator;
import org.sentilo.platform.server.validation.RequestMessageValidator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.util.StringUtils;

@Controller
public class AlarmHandler extends AbstractHandler {

  private static final Logger LOGGER = LoggerFactory.getLogger(AlarmHandler.class);

  @Autowired
  private AlarmService alarmService;

  private AlarmConverter parser = new AlarmConverter();
  private final RequestMessageValidator<AlarmInputMessage> validator = new AlarmValidator();

  @Override
  public void onDelete(final SentiloRequest request, final SentiloResponse response) {
    throw new MethodNotAllowedException(HttpMethod.DELETE);
  }

  @Override
  public void onGet(final SentiloRequest request, final SentiloResponse response) {
    LOGGER.debug("Executing alarm GET request");
    debug(request);

    // The request follows the following pattern:
    // GET /alarm/{alertId}
    // Furthermore, it could have parameters

    validateResourceNumberParts(request, 1, 1);
    final AlarmInputMessage inputMessage = parser.parseGetRequest(request);
    validator.validateRequestMessageOnGet(inputMessage);
    // An alert belongs to an entity (provider / application), so to validate that the caller is
    // allowed to do the requested action, i.e. has the permission READ-WRITE over the entity
    // owner, first we need to retrieve it.
    final String alertOwner = alarmService.getAlertOwner(inputMessage.getAlertId());
    validateReadAccess(request.getEntitySource(), alertOwner);

    final List<Alarm> lastAlarmsMessages = alarmService.getLastAlarms(inputMessage);

    parser.writeResponse(response, lastAlarmsMessages);
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
    LOGGER.debug("Executing alarm PUT request");
    debug(request);

    // The request follows the following pattern:
    // PUT /alarm/{alertId}
    // with a message on the body

    validateResourceNumberParts(request, 1, 1);
    final AlarmInputMessage inputMessage = parser.parseRequest(request);
    inputMessage.setEntityOwner(alarmService.getAlertOwner(inputMessage.getAlertId()));

    validator.validateRequestMessageOnPut(inputMessage);
    validateWriteAccess(request.getEntitySource(), inputMessage.getEntityOwner());

    alarmService.setAlarm(inputMessage);
  }

}
