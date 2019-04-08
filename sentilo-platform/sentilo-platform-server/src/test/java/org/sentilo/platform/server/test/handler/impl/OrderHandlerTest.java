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
package org.sentilo.platform.server.test.handler.impl;

import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.enums.HttpMethod;
import org.sentilo.platform.common.domain.EntityMetadataMessage;
import org.sentilo.platform.common.domain.Order;
import org.sentilo.platform.common.domain.OrderInputMessage;
import org.sentilo.platform.common.exception.EventRejectedException;
import org.sentilo.platform.common.exception.PlatformException;
import org.sentilo.platform.common.security.repository.EntityMetadataRepository;
import org.sentilo.platform.common.service.OrderService;
import org.sentilo.platform.server.auth.AuthorizationService;
import org.sentilo.platform.server.converter.OrderConverter;
import org.sentilo.platform.server.handler.HandlerPath;
import org.sentilo.platform.server.handler.impl.OrderHandler;
import org.sentilo.platform.server.request.SentiloRequest;
import org.sentilo.platform.server.request.SentiloResource;
import org.sentilo.platform.server.response.SentiloResponse;

public class OrderHandlerTest extends AbstractBaseHandlerTest {

  private static final String PROVIDER1 = "provider1";
  @InjectMocks
  private OrderHandler handler;
  @Mock
  private OrderService service;
  @Mock
  private SentiloRequest request;
  @Mock
  private SentiloResource resource;
  @Mock
  private SentiloResponse response;
  @Mock
  private OrderConverter parser;
  @Mock
  private OrderInputMessage message;
  @Mock
  private AuthorizationService authorizationService;
  @Mock
  private EntityMetadataRepository entityMetadataRepository;
  @Mock
  private EntityMetadataMessage entityMetadataMessage;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);

    when(request.getResource()).thenReturn(resource);
    when(authorizationService.hasAccessToRead(anyString(), anyString())).thenReturn(true);
    when(authorizationService.hasAccessToWrite(anyString(), anyString())).thenReturn(true);
    when(entityMetadataRepository.getEntityMetadataFromId(anyString())).thenReturn(entityMetadataMessage);

  }

  @Test
  public void putRequest() throws Exception {
    when(parser.parseRequest(request)).thenReturn(message);
    when(message.getOrder()).thenReturn("stop");

    simulateRequest(HttpMethod.PUT, PROVIDER1, "/order/prov1/sensor1");
    handler.manageRequest(request, response);

    verify(parser).parseRequest(request);
    verify(service).setOrder(message);
  }

  @Test
  public void invalidPutRequest() throws Exception {
    boolean errorThrown = false;
    when(parser.parseRequest(request)).thenReturn(message);
    when(message.getOrder()).thenReturn(null);

    simulateRequest(HttpMethod.PUT, PROVIDER1, "/order/prov1/sensor1");
    try {
      handler.manageRequest(request, response);
    } catch (final PlatformException e) {
      assertBadRequest(e);
      errorThrown = true;
    } finally {
      Assert.assertTrue(errorThrown);
    }
  }

  @Test(expected = EventRejectedException.class)
  public void unknownSensor() throws Exception {
    when(parser.parseRequest(request)).thenReturn(message);
    when(message.getOrder()).thenReturn("stop");
    doThrow(EventRejectedException.class).when(service).setOrder(message);

    simulateRequest(HttpMethod.PUT, PROVIDER1, "/order/prov1/sensor1");
    handler.manageRequest(request, response);

    verify(parser).parseRequest(request);
    verify(service).setOrder(message);
  }

  @Test
  public void deleteRequest() throws Exception {
    try {
      simulateRequest(HttpMethod.DELETE, PROVIDER1, "/order/prov1/sensor1");
      handler.manageRequest(request, response);
    } catch (final PlatformException e) {
      assertMethodNotAllowed(e);
    }
  }

  @Test
  public void getRequest() throws Exception {
    final List<Order> orders = getOrders();
    final OrderInputMessage message = new OrderInputMessage("provider2", "sensor1");
    when(parser.parseGetRequest(request)).thenReturn(message);
    when(service.getLastOrders(message)).thenReturn(orders);

    simulateRequest(HttpMethod.GET, PROVIDER1, "/order/provider2/sensor1");
    handler.manageRequest(request, response);

    verify(parser).parseGetRequest(request);
    verify(parser).writeResponse(request, response, orders);
  }

  @Test
  public void simulatePutRequest() throws Exception {
    when(parser.parseRequest(request)).thenReturn(message);
    when(message.getOrder()).thenReturn("stop");
    when(request.getRequestParameter("method")).thenReturn("put");

    simulateRequest(HttpMethod.POST, PROVIDER1, "/order/prov1/sensor1");
    handler.manageRequest(request, response);

    verify(parser).parseRequest(request);
    verify(service).setOrder(message);
  }

  @Test
  public void postRequest() throws Exception {
    try {
      simulateRequest(HttpMethod.POST, PROVIDER1, "/order/prov1/sensor1");
      handler.manageRequest(request, response);
    } catch (final PlatformException e) {
      assertMethodNotAllowed(e);
    }
  }

  private List<Order> getOrders() {
    final List<Order> orders = new ArrayList<Order>();
    final Order order1 = new Order("prov1", "sensor1", "stop", "sender1", System.currentTimeMillis());
    final Order order2 = new Order("prov1", "sensor1", "start", "sender1", System.currentTimeMillis());
    orders.add(order1);
    orders.add(order2);

    return orders;
  }

  @Override
  protected HandlerPath getHandlerPath() {
    return HandlerPath.ORDER;
  }

  @Override
  protected SentiloResource getSentiloResource() {
    return resource;
  }

  @Override
  protected SentiloRequest getSentiloRequest() {
    return request;
  }

}
