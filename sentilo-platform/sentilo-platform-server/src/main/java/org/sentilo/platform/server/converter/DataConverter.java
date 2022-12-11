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
package org.sentilo.platform.server.converter;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.sentilo.platform.common.domain.DataInputMessage;
import org.sentilo.platform.common.domain.Observation;
import org.sentilo.platform.server.dto.ObservationMessage;
import org.sentilo.platform.server.dto.ObservationsMessage;
import org.sentilo.platform.server.dto.SensorMessage;
import org.sentilo.platform.server.dto.SensorsMessage;
import org.sentilo.platform.server.request.SentiloRequest;
import org.sentilo.platform.server.request.SentiloResource;
import org.sentilo.platform.server.response.SentiloResponse;
import org.springframework.util.StringUtils;

public class DataConverter extends PlatformJsonMessageConverter {

  public DataInputMessage parsePutRequest(final SentiloRequest request) {
    final SentiloResource resource = request.getResource();
    List<Observation> observations = null;
    DataInputMessage message = null;
    if (resource.getParts().length == 3) {
      final String providerId = resource.getResourcePart(0);
      final String sensorId = resource.getResourcePart(1);
      final String value = resource.getResourcePart(2);
      final Observation observation = new Observation(providerId, sensorId, value);
      observations = new ArrayList<Observation>();
      observations.add(observation);
      message = new DataInputMessage(providerId, sensorId, observations);
    } else if (resource.getParts().length == 1) {
      final String providerId = resource.getResourcePart(0);
      final SensorsMessage inputMessage = (SensorsMessage) readInternal(SensorsMessage.class, request);
      observations = inputMessageToDomain(resource, inputMessage);
      message = new DataInputMessage(providerId, observations);
    } else {
      final String providerId = resource.getResourcePart(0);
      final String sensorId = resource.getResourcePart(1);
      final ObservationsMessage inputMessage = (ObservationsMessage) readInternal(ObservationsMessage.class, request);
      observations = inputMessageToDomain(resource, inputMessage);
      message = new DataInputMessage(providerId, sensorId, observations);
    }

    return message;
  }

  public DataInputMessage parseDeleteRequest(final SentiloRequest request) {
    final SentiloResource resource = request.getResource();
    final String providerId = resource.getResourcePart(0);
    final String sensorId = resource.getResourcePart(1);

    return new DataInputMessage(providerId, sensorId);
  }

  public DataInputMessage parseGetRequest(final SentiloRequest request) {
    final SentiloResource resource = request.getResource();
    final String providerId = resource.getResourcePart(0);
    final String sensorId = resource.getResourcePart(1);
    final String from = request.getRequestParameter("from");
    final String to = request.getRequestParameter("to");
    final String limit = request.getRequestParameter("limit");

    return new DataInputMessage(providerId, sensorId, parseDate(from), parseDate(to), parseInteger(limit));
  }

  public void writeResponse(final SentiloRequest request, final SentiloResponse response, final List<Observation> observations) {
    // transformar a objeto de tipo SensorsMessage o ObservationsMessage, depende del caso de la
    // petición
    final Object message = parseObservationsListToMessage(request, observations);
    writeInternal(message, response);
  }

  private Object parseObservationsListToMessage(final SentiloRequest request, final List<Observation> observations) {
    final SentiloResource resource = request.getResource();

    if (resource.getParts().length == 1) {
      return parseObservationsListToSensorsMessage(observations);
    } else {
      return parseObservationsListToObservationsMessage(observations);
    }
  }

  private SensorsMessage parseObservationsListToSensorsMessage(final List<Observation> observationsList) {
    final SensorsMessage sensorsMessage = new SensorsMessage();
    final Map<String, SensorMessage> sensors = new HashMap<String, SensorMessage>();

    for (final Observation observation : observationsList) {
      final ObservationMessage obsMessage = parseObservationToObservationMessage(observation);
      SensorMessage sensorMessage = sensors.get(observation.getSensor());
      if (sensorMessage == null) {
        sensorMessage = new SensorMessage();
        sensorMessage.setSensor(observation.getSensor());
        sensors.put(sensorMessage.getSensor(), sensorMessage);
        sensorsMessage.addSensor(sensorMessage);
      }
      sensorMessage.addObservationMessage(obsMessage);
    }

    return sensorsMessage;
  }

  private ObservationsMessage parseObservationsListToObservationsMessage(final List<Observation> observationsList) {
    final ObservationsMessage observations = new ObservationsMessage();
    for (final Observation observation : observationsList) {
      observations.addObservation(parseObservationToObservationMessage(observation));
    }

    return observations;
  }

  private ObservationMessage parseObservationToObservationMessage(final Observation observation) {
    final ObservationMessage message = new ObservationMessage();
    message.setValue(observation.getValue());
    message.setLocation(observation.getLocation());
    message.setTimestamp(timestampToString(observation.getTimestamp()));
    message.setTime(observation.getTimestamp());

    return message;
  }

  private List<Observation> inputMessageToDomain(final SentiloResource resource, final SensorsMessage inputMessage) {
    final String providerId = resource.getResourcePart(0);
    final List<Observation> observations = new ArrayList<Observation>();

    for (final SensorMessage sensorMessage : inputMessage.getSensors()) {
      final String globalLocation = sensorMessage.getLocation();
      final String sensorId = sensorMessage.getSensor();

      observations.addAll(parseObservationsMessageToDomain(sensorMessage.getObservations(), globalLocation, providerId, sensorId));
    }

    return observations;
  }

  private List<Observation> inputMessageToDomain(final SentiloResource resource, final ObservationsMessage inputMessage) {
    final String providerId = resource.getResourcePart(0);
    final String sensorId = resource.getResourcePart(1);
    final String globalLocation = inputMessage.getLocation();

    return parseObservationsMessageToDomain(inputMessage.getObservations(), globalLocation, providerId, sensorId);
  }

  private List<Observation> parseObservationsMessageToDomain(final List<ObservationMessage> inputMessage, final String globalLocation,
      final String providerId, final String sensorId) {
    final List<Observation> observations = new ArrayList<Observation>();

    for (final ObservationMessage message : inputMessage) {
      final String value = message.getValue();
      final String timestamp = message.getTimestamp();
      final String location = StringUtils.hasText(message.getLocation()) ? message.getLocation() : globalLocation;

      final Observation observation = new Observation(providerId, sensorId, value, parseTimestamp(timestamp), parseLocation(location));
      observations.add(observation);
    }

    return observations;
  }
}
