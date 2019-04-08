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
package org.sentilo.platform.client.test.converter;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import java.util.ArrayList;
import java.util.List;

import org.junit.Test;
import org.sentilo.common.converter.StringMessageConverter;
import org.sentilo.common.exception.MessageNotWritableException;
import org.sentilo.platform.client.core.domain.DataInputMessage;
import org.sentilo.platform.client.core.domain.Observation;
import org.sentilo.platform.client.core.domain.ProviderObservations;
import org.sentilo.platform.client.core.domain.SensorObservations;
import org.sentilo.platform.client.core.parser.DataMessageConverter;

public class DataMessageConverterTest {

  private static final String PROVIDER_ID = "provider1";
  private static final String SENSOR_ID = "sensor1";
  private static final String OBSERVATION = "23";
  private static final String OBSERVATION2 = "28.4";

  private StringMessageConverter converter = new DataMessageConverter();

  @Test
  public void marshallMessageWithoutBody() throws MessageNotWritableException {
    final DataInputMessage message = new DataInputMessage(PROVIDER_ID, SENSOR_ID, OBSERVATION);
    final String json = converter.marshal(message);
    assertNull(json);
  }

  @Test
  public void marshallMessageWithSensorObservations() throws MessageNotWritableException {
    final DataInputMessage message = new DataInputMessage(PROVIDER_ID, SENSOR_ID);
    message.setSensorObservations(buildSensorObservations());
    final String json = converter.marshal(message);
    final String expected =
        "{\"observations\":[{\"value\":\"23\",\"timestamp\":\"20/02/2013T16:57:17\",\"time\":1361379437000},{\"value\":\"28.4\",\"timestamp\":\"20/02/2013T16:55:37\",\"time\":1361379337000}],\"location\":\"35.7 56.4\"}";
    assertNotNull(json);
    assertEquals(expected, json);
  }

  @Test
  public void marshallMessageWithProviderObservations() throws MessageNotWritableException {
    final DataInputMessage message = new DataInputMessage(PROVIDER_ID, SENSOR_ID);
    message.setProviderObservations(buildProviderObservations());
    final String json = converter.marshal(message);
    final String expected =
        "{\"sensors\":[{\"observations\":[{\"value\":\"23\",\"timestamp\":\"20/02/2013T16:57:17\",\"time\":1361379437000},{\"value\":\"28.4\",\"timestamp\":\"20/02/2013T16:55:37\",\"time\":1361379337000}],\"location\":\"35.7 56.4\",\"sensor\":\"sensor1\"}]}";
    assertNotNull(json);
    System.out.println(json);
    assertEquals(expected, json);
  }

  private ProviderObservations buildProviderObservations() {
    final ProviderObservations provObservations = new ProviderObservations();

    final SensorObservations sensorObs = buildSensorObservations();
    sensorObs.setSensor(SENSOR_ID);
    final List<SensorObservations> sensorObservations = new ArrayList<SensorObservations>();
    sensorObservations.add(sensorObs);

    provObservations.setSensorsObservations(sensorObservations);
    return provObservations;
  }

  private SensorObservations buildSensorObservations() {
    final SensorObservations sensorObs = new SensorObservations();
    sensorObs.setLocation("35.7 56.4");
    sensorObs.setObservations(buildObservations());
    return sensorObs;
  }

  private List<Observation> buildObservations() {
    final List<Observation> observations = new ArrayList<Observation>();
    observations.add(new Observation(OBSERVATION, "20/02/2013T16:57:17"));
    observations.add(new Observation(OBSERVATION2, "20/02/2013T16:55:37"));
    return observations;
  }
}
