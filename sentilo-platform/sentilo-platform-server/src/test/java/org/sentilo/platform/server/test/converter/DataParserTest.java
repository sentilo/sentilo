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
package org.sentilo.platform.server.test.converter;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.when;

import java.io.ByteArrayOutputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.apache.http.HttpVersion;
import org.apache.http.entity.ByteArrayEntity;
import org.apache.http.message.BasicHttpResponse;
import org.apache.http.message.BasicStatusLine;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.utils.DateUtils;
import org.sentilo.platform.common.domain.DataInputMessage;
import org.sentilo.platform.common.domain.Observation;
import org.sentilo.platform.common.exception.PlatformException;
import org.sentilo.platform.server.converter.DataConverter;
import org.sentilo.platform.server.request.SentiloRequest;
import org.sentilo.platform.server.request.SentiloResource;
import org.sentilo.platform.server.response.SentiloResponse;

public class DataParserTest {

  private DataConverter parser;
  @Mock
  private SentiloRequest sentiloRequest;
  @Mock
  private SentiloResource resource;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    parser = new DataConverter();
    when(sentiloRequest.getResource()).thenReturn(resource);
  }

  @Test
  public void parseDeleteRequest() throws Exception {
    final String providerId = "prov1";
    final String sensorId = "sensor1";

    when(resource.getResourcePart(0)).thenReturn(providerId);
    when(resource.getResourcePart(1)).thenReturn(sensorId);

    final DataInputMessage message = parser.parseDeleteRequest(sentiloRequest);

    assertEquals(message.getProviderId(), providerId);
    assertEquals(message.getSensorId(), sensorId);
  }

  @Test
  public void parseGetRequestWithoutFilter() throws Exception {
    final String providerId = "prov1";
    final String sensorId = "sensor1";

    when(resource.getResourcePart(0)).thenReturn(providerId);
    when(resource.getResourcePart(1)).thenReturn(sensorId);

    final DataInputMessage message = parser.parseGetRequest(sentiloRequest);

    assertEquals(message.getProviderId(), providerId);
    assertEquals(message.getSensorId(), sensorId);
  }

  @Test
  public void parseGetRequestWithFilter() throws Exception {
    final String providerId = "prov1";
    final String sensorId = "sensor1";
    final String from = "17/09/2012T12:34:45";
    final String to = null;
    final String limit = "5";

    when(resource.getResourcePart(0)).thenReturn(providerId);
    when(resource.getResourcePart(1)).thenReturn(sensorId);
    when(sentiloRequest.getRequestParameter("from")).thenReturn(from);
    when(sentiloRequest.getRequestParameter("to")).thenReturn(to);
    when(sentiloRequest.getRequestParameter("limit")).thenReturn(limit);

    final DataInputMessage message = parser.parseGetRequest(sentiloRequest);

    assertEquals(message.getProviderId(), providerId);
    assertEquals(message.getSensorId(), sensorId);
    assertTrue(message.hasQueryFilters());
    assertEquals(message.getQueryFilters().getLimit(), Integer.valueOf(limit));
    assertEquals(message.getQueryFilters().getFrom(), DateUtils.stringToDate(from));
    assertEquals(message.getQueryFilters().getTo(), DateUtils.stringToDate(to));
  }

  @Test
  public void parsePutObservationsList() throws Exception {
    final String json =
        "{\"observations\":[{\"value\":\"10.1\"}, {\"value\":\"11.2\", \"timestamp\": \"17/09/2012T12:34:45\"}, {\"value\":\"12.3\"}]}";
    final String[] parts = {"prov1", "sensor1"};

    when(sentiloRequest.getBody()).thenReturn(json);
    when(resource.getParts()).thenReturn(parts);

    final DataInputMessage message = parser.parsePutRequest(sentiloRequest);
    assertEquals("Must parse 3 elements", 3, message.getObservations().size());
  }

  @Test
  public void parsePutSimpleObservation() throws Exception {
    final String json = "{\"observations\":[{\"value\":\"11.2\", \"timestamp\": \"17/09/2012T12:34:45\"}]}";
    final String[] parts = {"prov1", "sensor1"};

    when(sentiloRequest.getBody()).thenReturn(json);
    when(resource.getParts()).thenReturn(parts);

    final DataInputMessage message = parser.parsePutRequest(sentiloRequest);
    assertEquals("Must parse 1 element", 1, message.getObservations().size());
  }

  @Test(expected = PlatformException.class)
  public void parsePutBadRequestObservation() throws Exception {
    final String json = "{\"observations\":[{\"value\":\"11.2\", \"timestamp\": \"17/09/2012A12:34:45\"}]}";
    final String[] parts = {"prov1", "sensor1"};

    when(sentiloRequest.getBody()).thenReturn(json);
    when(resource.getParts()).thenReturn(parts);

    parser.parsePutRequest(sentiloRequest);
  }

  @Test(expected = PlatformException.class)
  public void parsePutObservationWithTrasposeTimestamp() throws Exception {
    final String json = "{\"observations\":[{\"value\":\"11.2\", \"timestamp\": \"11/23/2012T12:34:45\"}]}";
    final String[] parts = {"prov1", "sensor1"};

    when(sentiloRequest.getBody()).thenReturn(json);
    when(resource.getParts()).thenReturn(parts);

    parser.parsePutRequest(sentiloRequest);
  }

  @Test
  public void parsePutSensorsList() throws Exception {
    final String json =
        "{\"sensors\":[{\"sensor\":\"sensor1\",\"observations\":[{\"value\":\"10.1\"},{\"value\":\"11.2\",\"timestamp\":\"17/09/2012T12:34:45\"},{\"value\":\"12.3\"}]},{\"sensor\":\"sensor2\",\"observations\":[{\"value\":\"10.1\"},{\"value\":\"11.2\",\"timestamp\":\"17/09/2012T12:34:45\"},{\"value\":\"12.3\"}]}]}";
    final String[] parts = {"prov1"};

    when(sentiloRequest.getBody()).thenReturn(json);
    when(resource.getParts()).thenReturn(parts);

    final DataInputMessage message = parser.parsePutRequest(sentiloRequest);
    assertEquals("Must parse 6 element", 6, message.getObservations().size());
  }

  @Test
  public void parseSensorWriteResponse() throws Exception {
    final String[] parts = {"prov1", "sensor1"};
    when(resource.getParts()).thenReturn(parts);

    final SentiloResponse response = SentiloResponse.build(new BasicHttpResponse(new BasicStatusLine(HttpVersion.HTTP_1_0, 200, "")));
    parser.writeResponse(sentiloRequest, response, getObservationsFromSensor());

    final ByteArrayOutputStream baos = new ByteArrayOutputStream();
    ((ByteArrayEntity) response.getHttpResponse().getEntity()).writeTo(baos);
    final String expected =
        "{\"observations\":[{\"value\":\"1\",\"timestamp\":\"21/02/2013T17:49:24\",\"time\":1361468964000},{\"value\":\"10\",\"timestamp\":\"21/02/2013T17:49:30\",\"time\":1361468970000}]}";
    assertEquals(expected, baos.toString());
  }

  @Test
  public void parseEmptySensorWriteResponse() throws Exception {
    final String[] parts = {"prov1", "sensor1"};
    when(resource.getParts()).thenReturn(parts);

    final List<Observation> subscriptionList = Collections.emptyList();
    final SentiloResponse response = SentiloResponse.build(new BasicHttpResponse(new BasicStatusLine(HttpVersion.HTTP_1_0, 200, "")));
    parser.writeResponse(sentiloRequest, response, subscriptionList);

    final ByteArrayOutputStream baos = new ByteArrayOutputStream();
    ((ByteArrayEntity) response.getHttpResponse().getEntity()).writeTo(baos);
    final String expected = "{\"observations\":[]}";
    assertEquals(expected, baos.toString());
  }

  @Test
  public void parseProviderWriteResponse() throws Exception {
    final String[] parts = {"prov1"};
    when(resource.getParts()).thenReturn(parts);

    final SentiloResponse response = SentiloResponse.build(new BasicHttpResponse(new BasicStatusLine(HttpVersion.HTTP_1_0, 200, "")));
    parser.writeResponse(sentiloRequest, response, getObservationsFromProvider());

    final ByteArrayOutputStream baos = new ByteArrayOutputStream();
    ((ByteArrayEntity) response.getHttpResponse().getEntity()).writeTo(baos);
    final String expected =
        "{\"sensors\":[{\"sensor\":\"sensor1\",\"observations\":[{\"value\":\"1\",\"timestamp\":\"21/02/2013T17:49:24\",\"time\":1361468964000}]},{\"sensor\":\"sensor2\",\"observations\":[{\"value\":\"10\",\"timestamp\":\"21/02/2013T17:49:30\",\"time\":1361468970000},{\"value\":\"5\",\"timestamp\":\"20/02/2013T17:49:30\",\"time\":1361382570000}]}]}";

    assertEquals(expected, baos.toString());
  }

  @Test
  public void parseEmptyProviderWriteResponse() throws Exception {
    final String[] parts = {"prov1"};
    when(resource.getParts()).thenReturn(parts);

    final List<Observation> subscriptionList = Collections.emptyList();
    final SentiloResponse response = SentiloResponse.build(new BasicHttpResponse(new BasicStatusLine(HttpVersion.HTTP_1_0, 200, "")));
    parser.writeResponse(sentiloRequest, response, subscriptionList);

    final ByteArrayOutputStream baos = new ByteArrayOutputStream();
    ((ByteArrayEntity) response.getHttpResponse().getEntity()).writeTo(baos);
    final String expected = "{\"sensors\":[]}";
    assertEquals(expected, baos.toString());
  }

  private List<Observation> getObservationsFromSensor() {
    final List<Observation> observations = new ArrayList<Observation>();
    final Observation obs1 = new Observation("prov1", "sensor1", "1", DateUtils.toMillis("21/02/2013T17:49:24"));
    final Observation obs2 = new Observation("prov1", "sensor1", "10", DateUtils.toMillis("21/02/2013T17:49:30"));
    observations.add(obs1);
    observations.add(obs2);
    return observations;
  }

  private List<Observation> getObservationsFromProvider() {
    final List<Observation> observations = new ArrayList<Observation>();
    final Observation obs1 = new Observation("prov1", "sensor1", "1", DateUtils.toMillis("21/02/2013T17:49:24"));
    final Observation obs2 = new Observation("prov1", "sensor2", "10", DateUtils.toMillis("21/02/2013T17:49:30"));
    final Observation obs3 = new Observation("prov1", "sensor2", "5", DateUtils.toMillis("20/02/2013T17:49:30"));
    observations.add(obs1);
    observations.add(obs2);
    observations.add(obs3);
    return observations;
  }
}
