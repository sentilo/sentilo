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
package org.sentilo.platform.server.parser;

import java.io.IOException;
import java.util.List;

import org.apache.http.HttpStatus;
import org.sentilo.platform.common.domain.Alarm;
import org.sentilo.platform.common.domain.AlarmInputMessage;
import org.sentilo.platform.common.exception.PlatformException;
import org.sentilo.platform.server.dto.AlarmMessage;
import org.sentilo.platform.server.dto.AlarmsMessage;
import org.sentilo.platform.server.request.SentiloRequest;
import org.sentilo.platform.server.request.SentiloResource;
import org.sentilo.platform.server.response.SentiloResponse;

public class AlarmParser extends PlatformJsonMessageConverter {

  public AlarmInputMessage parseRequest(final SentiloRequest request) throws PlatformException {

    final AlarmInputMessage inputMessage = (AlarmInputMessage) readInternal(AlarmInputMessage.class, request);

    final String alertId = request.getResourcePart(0);

    inputMessage.setAlertId(alertId);
    inputMessage.setSender(request.getEntitySource());

    return inputMessage;
  }

  public AlarmInputMessage parseGetRequest(final SentiloRequest request) throws PlatformException {
    final SentiloResource resource = request.getResource();
    final String alertId = resource.getResourcePart(0);
    final String from = request.getRequestParameter("from");
    final String to = request.getRequestParameter("to");
    final String limit = request.getRequestParameter("limit");

    return new AlarmInputMessage(alertId, parseDate(from), parseDate(to), parseInteger(limit));
  }

  public void writeResponse(final SentiloRequest request, final SentiloResponse response, final List<Alarm> alarmsList) throws PlatformException {
    // transformar a objeto de tipo AlarmsMessage
    final Object message = parseAlarmsListToAlarmssMessage(alarmsList);

    try {
      writeInternal(message, response);
    } catch (final IOException ex) {
      throw new PlatformException(HttpStatus.SC_INTERNAL_SERVER_ERROR, ex);
    }
  }

  private AlarmsMessage parseAlarmsListToAlarmssMessage(final List<Alarm> alarmsList) {
    final AlarmsMessage alarms = new AlarmsMessage();
    for (final Alarm alarm : alarmsList) {
      alarms.addAlarm(parseAlarmToAlarmMessage(alarm));
    }

    return alarms;
  }

  private AlarmMessage parseAlarmToAlarmMessage(final Alarm alarm) {
    final AlarmMessage alarmMessage = new AlarmMessage();
    alarmMessage.setMessage(alarm.getMessage());
    alarmMessage.setSender(alarm.getSender());
    alarmMessage.setTimestamp(timestampToString(alarm.getTimestamp()));
    return alarmMessage;
  }

}
