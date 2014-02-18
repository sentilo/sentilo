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
import org.sentilo.platform.common.domain.Alarm;
import org.sentilo.platform.common.domain.AlarmInputMessage;
import org.sentilo.platform.common.exception.PlatformException;
import org.sentilo.platform.common.service.AlarmService;
import org.sentilo.platform.server.handler.AbstractHandler;
import org.sentilo.platform.server.parser.AlarmParser;
import org.sentilo.platform.server.request.SentiloRequest;
import org.sentilo.platform.server.response.SentiloResponse;
import org.sentilo.platform.server.validation.AlarmValidator;
import org.sentilo.platform.server.validation.RequestMessageValidator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;

@Controller
public class AlarmHandler extends AbstractHandler {

  private final Logger logger = LoggerFactory.getLogger(AlarmHandler.class);

  @Autowired
  private AlarmService alarmService;

  private AlarmParser parser = new AlarmParser();
  private final RequestMessageValidator<AlarmInputMessage> validator = new AlarmValidator();

  @Override
  public void onDelete(final SentiloRequest request, final SentiloResponse response) throws PlatformException {
    throw new PlatformException(HttpStatus.SC_METHOD_NOT_ALLOWED, "HTTP DELETE method not allowed for the requested resource");
  }

  @Override
  public void onGet(final SentiloRequest request, final SentiloResponse response) throws PlatformException {
    logger.debug("Executing alarm GET request");
    debug(request);

    // La peticion tiene el formato
    // GET /alarm/alertId
    // Ademas, puede haber parametros en la URL

    validateResourceNumberParts(request, 1, 1);
    final AlarmInputMessage inputMessage = parser.parseGetRequest(request);
    validator.validateRequestMessageOnGet(inputMessage);
    final String alertOwner = alarmService.getAlertOwner(inputMessage.getAlertId());
    validateReadAccess(request.getEntitySource(), alertOwner);
    final List<Alarm> lastAlarmsMessages = alarmService.getLastMessages(inputMessage);

    parser.writeResponse(request, response, lastAlarmsMessages);
  }

  @Override
  public void onPost(final SentiloRequest request, final SentiloResponse response) throws PlatformException {
    throw new PlatformException(HttpStatus.SC_METHOD_NOT_ALLOWED, "HTTP POST method not allowed for the requested resource");
  }

  @Override
  public void onPut(final SentiloRequest request, final SentiloResponse response) throws PlatformException {
    logger.debug("Executing alarm PUT request");
    debug(request);

    // La peticion tiene el siguiente formato
    // PUT /alarm/alertId con un mensaje en el body de manera opcional

    validateResourceNumberParts(request, 1, 1);
    final AlarmInputMessage inputMessage = parser.parseRequest(request);
    validator.validateRequestMessageOnPut(inputMessage);
    // La alarma pertenece a un proveedor o a una app cliente.
    // Recuperamos el propietario de la alarma y validamos que el solicitante de la accion tiene
    // autorizacion para escribir sobre los recursos del propietario
    final String alarmOwner = alarmService.getAlertOwner(inputMessage.getAlertId());
    validateWriteAccess(request.getEntitySource(), alarmOwner);

    alarmService.setAlarm(inputMessage);
  }

  public void setAlarmService(final AlarmService alarmService) {
    this.alarmService = alarmService;
  }

  public void setAlarmParser(final AlarmParser parser) {
    this.parser = parser;
  }
}
