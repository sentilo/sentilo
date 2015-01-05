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
package org.sentilo.platform.server.test.handler.impl;

import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.domain.SubscribeType;
import org.sentilo.platform.common.domain.Subscription;
import org.sentilo.platform.common.exception.PlatformException;
import org.sentilo.platform.common.service.SubscribeService;
import org.sentilo.platform.server.auth.AuthorizationService;
import org.sentilo.platform.server.handler.HandlerPath;
import org.sentilo.platform.server.handler.impl.SubscribeHandler;
import org.sentilo.platform.server.http.HttpMethod;
import org.sentilo.platform.server.parser.SubscribeParser;
import org.sentilo.platform.server.request.SentiloRequest;
import org.sentilo.platform.server.request.SentiloResource;
import org.sentilo.platform.server.response.SentiloResponse;

public class SubscribeHandlerTest extends AbstractBaseHandlerTest {

  private static final String PROVIDER1 = "provider1";
  private static final String ENDPOINT = "http://dev.connecta.cat";
  private SubscribeHandler handler;
  @Mock
  private SubscribeService service;
  @Mock
  private SentiloRequest request;
  @Mock
  private SentiloResource resource;
  @Mock
  private SentiloResponse response;
  @Mock
  private SubscribeParser parser;
  @Mock
  private Subscription message;
  @Mock
  private AuthorizationService authorizationService;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    handler = new SubscribeHandler();
    handler.setSubscribeService(service);
    handler.setSubscribeParser(parser);
    handler.setAuthorizationService(authorizationService);
    when(request.getResource()).thenReturn(resource);
    when(authorizationService.hasAccessToRead(anyString(), anyString())).thenReturn(true);
    when(authorizationService.hasAccessToWrite(anyString(), anyString())).thenReturn(true);
  }

  @Test
  public void putRequest() throws Exception {
    when(parser.parseRequest(request, SubscribeType.DATA)).thenReturn(message);
    when(message.getType()).thenReturn(SubscribeType.DATA);
    when(message.getEndpoint()).thenReturn(ENDPOINT);

    simulateRequest(HttpMethod.PUT, PROVIDER1, "/subscribe/provider1/sensor1");
    handler.manageRequest(request, response);

    verify(parser).parseRequest(request, SubscribeType.DATA);
    verify(service).subscribe(message);
  }

  @Test
  public void deleteRequest() throws Exception {
    when(parser.parseBasicRequest(request)).thenReturn(message);

    simulateRequest(HttpMethod.DELETE, PROVIDER1, "/subscribe/data/provider2/sensor1");
    handler.manageRequest(request, response);

    verify(parser).parseBasicRequest(request);
    verify(service).remove(message);
  }

  @Test
  public void deleteRequestWithoutResource() throws Exception {
    when(parser.parseBasicRequest(request)).thenReturn(message);

    simulateRequest(HttpMethod.DELETE, PROVIDER1, "/subscribe");
    handler.manageRequest(request, response);

    verify(parser).parseBasicRequest(request);
    verify(service).remove(message);
  }

  @Test
  public void getRequest() throws Exception {
    final List<Subscription> subscriptions = getSubscriptions();
    when(parser.parseBasicRequest(request)).thenReturn(message);
    when(service.get(message)).thenReturn(subscriptions);

    simulateRequest(HttpMethod.GET, PROVIDER1, "/subscribe/alarm");
    handler.manageRequest(request, response);

    verify(parser).parseBasicRequest(request);
    verify(service).get(message);
    verify(parser).writeResponse(response, subscriptions);
  }

  @Test
  public void postRequest() throws Exception {
    try {
      simulateRequest(HttpMethod.POST, PROVIDER1, "/subscribe/provider1/sensor1");
      handler.manageRequest(request, response);
    } catch (final PlatformException e) {
      assertMethodNotAllowed(e);
    }
  }

  @Override
  protected HandlerPath getHandlerPath() {
    return HandlerPath.SUBSCRIBE;
  }

  @Override
  protected SentiloResource getSentiloResource() {
    return resource;
  }

  @Override
  protected SentiloRequest getSentiloRequest() {
    return request;
  }

  private List<Subscription> getSubscriptions() {
    final List<Subscription> list = new ArrayList<Subscription>();
    list.add(new Subscription(PROVIDER1, PROVIDER1, ENDPOINT));
    return list;
  }
}
