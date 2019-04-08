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

import org.apache.http.HttpStatus;
import org.apache.http.HttpVersion;
import org.apache.http.entity.ByteArrayEntity;
import org.apache.http.message.BasicHttpResponse;
import org.apache.http.message.BasicStatusLine;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.utils.DateUtils;
import org.sentilo.platform.common.domain.Order;
import org.sentilo.platform.common.domain.OrderInputMessage;
import org.sentilo.platform.common.exception.JsonConverterException;
import org.sentilo.platform.server.converter.OrderConverter;
import org.sentilo.platform.server.request.SentiloRequest;
import org.sentilo.platform.server.request.SentiloResource;
import org.sentilo.platform.server.response.SentiloResponse;

public class OrderParserTest {

  private OrderConverter parser;
  @Mock
  private SentiloRequest sentiloRequest;
  @Mock
  private SentiloResource resource;

  @Before
  public void setUp() throws Exception {
    final String identityKeyProvider = "prov1";
    MockitoAnnotations.initMocks(this);
    parser = new OrderConverter();

    when(sentiloRequest.getResource()).thenReturn(resource);
    when(sentiloRequest.getEntitySource()).thenReturn(identityKeyProvider);
  }

  @Test
  public void parsePutRequestTest() throws Exception {
    final String providerId = "prov1";
    final String sensorId = "sensor1";
    final String order = "stop restart";

    final String json = "{\"order\":\"stop restart\"}";

    when(sentiloRequest.getBody()).thenReturn(json);
    when(sentiloRequest.getResourcePart(0)).thenReturn(providerId);
    when(sentiloRequest.getResourcePart(1)).thenReturn(sensorId);

    final OrderInputMessage message = parser.parseRequest(sentiloRequest);

    assertEquals(message.getProviderId(), providerId);
    assertEquals(message.getSensorId(), sensorId);
    assertEquals(message.getOrder(), order);
    assertEquals(providerId, message.getSender());
  }

  @Test
  public void parsePutRequestWithoutOrderTest() throws Exception {
    final String providerId = "prov1";
    final String sensorId = "sensor1";

    when(sentiloRequest.getBody()).thenReturn("");
    when(sentiloRequest.getResourcePart(0)).thenReturn(providerId);
    when(sentiloRequest.getResourcePart(1)).thenReturn(sensorId);

    try {
      parser.parseRequest(sentiloRequest);
    } catch (final JsonConverterException jce) {
      assertEquals("Must return 400 - Bad request", HttpStatus.SC_BAD_REQUEST, jce.getHttpStatus());
    }
  }

  @Test
  public void parseGetRequestWithoutFilter() throws Exception {
    final String providerId = "prov1";
    final String sensorId = "sensor1";

    when(resource.getResourcePart(0)).thenReturn(providerId);
    when(resource.getResourcePart(1)).thenReturn(sensorId);

    final OrderInputMessage message = parser.parseGetRequest(sentiloRequest);

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

    final OrderInputMessage message = parser.parseGetRequest(sentiloRequest);

    assertEquals(message.getProviderId(), providerId);
    assertEquals(message.getSensorId(), sensorId);
    assertTrue(message.hasQueryFilters());
    assertEquals(message.getQueryFilters().getLimit(), Integer.valueOf(limit));
    assertEquals(message.getQueryFilters().getFrom(), DateUtils.stringToDate(from));
    assertEquals(message.getQueryFilters().getTo(), DateUtils.stringToDate(to));
  }

  @Test
  public void parseSensorOrdersWriteResponse() throws Exception {
    final String[] parts = {"prov1", "sensor1"};
    when(resource.getParts()).thenReturn(parts);

    final SentiloResponse response = SentiloResponse.build(new BasicHttpResponse(new BasicStatusLine(HttpVersion.HTTP_1_0, 200, "")));
    parser.writeResponse(sentiloRequest, response, getSensorOrders());

    final ByteArrayOutputStream baos = new ByteArrayOutputStream();
    ((ByteArrayEntity) response.getHttpResponse().getEntity()).writeTo(baos);
    final String expected =
        "{\"orders\":[{\"order\":\"stop\",\"timestamp\":\"21/02/2013T17:49:24\",\"sender\":\"sender1\",\"time\":1361468964000},{\"order\":\"start\",\"timestamp\":\"21/02/2013T17:49:30\",\"sender\":\"sender1\",\"time\":1361468970000}]}";
    assertEquals(expected, baos.toString());
  }

  @Test
  public void parseEmptySensorWriteResponse() throws Exception {
    final String[] parts = {"prov1", "sensor1"};
    when(resource.getParts()).thenReturn(parts);

    final List<Order> ordersList = Collections.emptyList();
    final SentiloResponse response = SentiloResponse.build(new BasicHttpResponse(new BasicStatusLine(HttpVersion.HTTP_1_0, 200, "")));
    parser.writeResponse(sentiloRequest, response, ordersList);

    final ByteArrayOutputStream baos = new ByteArrayOutputStream();
    ((ByteArrayEntity) response.getHttpResponse().getEntity()).writeTo(baos);
    final String expected = "{\"orders\":[]}";
    assertEquals(expected, baos.toString());
  }

  @Test
  public void parseProviderWriteResponse() throws Exception {
    final String[] parts = {"prov1"};
    when(resource.getParts()).thenReturn(parts);

    final SentiloResponse response = SentiloResponse.build(new BasicHttpResponse(new BasicStatusLine(HttpVersion.HTTP_1_0, 200, "")));
    parser.writeResponse(sentiloRequest, response, getProviderOrders());

    final ByteArrayOutputStream baos = new ByteArrayOutputStream();
    ((ByteArrayEntity) response.getHttpResponse().getEntity()).writeTo(baos);
    final String expected =
        "{\"sensors\":[{\"sensor\":\"sensor1\",\"orders\":[{\"order\":\"stop\",\"timestamp\":\"21/02/2013T17:49:24\",\"sender\":\"sender1\",\"time\":1361468964000}]},{\"sensor\":\"sensor2\",\"orders\":[{\"order\":\"stop\",\"timestamp\":\"21/02/2013T17:49:30\",\"sender\":\"sender1\",\"time\":1361468970000},{\"order\":\"start\",\"timestamp\":\"21/02/2013T17:49:30\",\"sender\":\"sender1\",\"time\":1361468970000}]}]}";
    assertEquals(expected, baos.toString());
  }

  @Test
  public void parseEmptyProviderWriteResponse() throws Exception {
    final String[] parts = {"prov1"};
    when(resource.getParts()).thenReturn(parts);

    final List<Order> ordersList = Collections.emptyList();
    final SentiloResponse response = SentiloResponse.build(new BasicHttpResponse(new BasicStatusLine(HttpVersion.HTTP_1_0, 200, "")));
    parser.writeResponse(sentiloRequest, response, ordersList);

    final ByteArrayOutputStream baos = new ByteArrayOutputStream();
    ((ByteArrayEntity) response.getHttpResponse().getEntity()).writeTo(baos);
    final String expected = "{\"sensors\":[]}";
    assertEquals(expected, baos.toString());
  }

  private List<Order> getSensorOrders() {
    final List<Order> orders = new ArrayList<Order>();
    final Order order1 = new Order("prov1", "sensor1", "stop", "sender1", DateUtils.toMillis("21/02/2013T17:49:24"));
    final Order order2 = new Order("prov1", "sensor1", "start", "sender1", DateUtils.toMillis("21/02/2013T17:49:30"));
    orders.add(order1);
    orders.add(order2);

    return orders;
  }

  private List<Order> getProviderOrders() {
    final List<Order> orders = new ArrayList<Order>();
    final Order order1 = new Order("prov1", "sensor1", "stop", "sender1", DateUtils.toMillis("21/02/2013T17:49:24"));
    final Order order2 = new Order("prov1", "sensor2", "stop", "sender1", DateUtils.toMillis("21/02/2013T17:49:30"));
    final Order order3 = new Order("prov1", "sensor2", "start", "sender1", DateUtils.toMillis("21/02/2013T17:49:30"));
    orders.add(order1);
    orders.add(order2);
    orders.add(order3);
    return orders;
  }

}
