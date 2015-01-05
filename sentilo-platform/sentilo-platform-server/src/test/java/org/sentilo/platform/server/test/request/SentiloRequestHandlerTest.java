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
package org.sentilo.platform.server.test.request;

import static org.mockito.Matchers.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.apache.http.HttpRequest;
import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.RequestLine;
import org.apache.http.entity.ContentType;
import org.apache.http.protocol.HttpContext;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.platform.server.auth.AuthenticationService;
import org.sentilo.platform.server.handler.AbstractHandler;
import org.sentilo.platform.server.handler.HandlerLocator;
import org.sentilo.platform.server.http.HttpHeader;
import org.sentilo.platform.server.request.SentiloRequest;
import org.sentilo.platform.server.request.SentiloRequestHandler;
import org.slf4j.Logger;

public class SentiloRequestHandlerTest {

  private SentiloRequestHandler requestHandler;

  @Mock
  private HandlerLocator handlerLocator;
  @Mock
  private AuthenticationService authenticationService;
  @Mock
  private HttpRequest httpRequest;
  @Mock
  private HttpResponse httpResponse;
  @Mock
  private HttpContext httpContext;
  @Mock
  private RequestLine requestLine;
  @Mock
  private AbstractHandler handler;
  @Mock
  private Logger logger;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    requestHandler = new SentiloRequestHandler(handlerLocator, authenticationService);

  }

  @Test
  public void handler() throws Exception {
    when(httpRequest.getRequestLine()).thenReturn(requestLine);
    when(requestLine.getMethod()).thenReturn("GET");
    when(requestLine.getUri()).thenReturn("http://lab.sentilo.io/data/mock");
    when(handlerLocator.lookup(any(SentiloRequest.class))).thenReturn(handler);
    when(handler.getLogger()).thenReturn(logger);

    requestHandler.handle(httpRequest, httpResponse, httpContext);

    // verify(handler).manageRequest(any(SentiloRequest.class), any(SentiloResponse.class));
    verify(httpResponse).setStatusCode(HttpStatus.SC_OK);
    verify(httpResponse).setHeader(HttpHeader.CONTENT_TYPE.toString(), ContentType.APPLICATION_JSON.toString());

  }

  @Test
  public void handlerNotFound() {
    when(httpRequest.getRequestLine()).thenReturn(requestLine);
    when(requestLine.getMethod()).thenReturn("GET");
    when(requestLine.getUri()).thenReturn("http://lab.sentilo.io/data/mock");
    requestHandler.handle(httpRequest, httpResponse, httpContext);

    verify(httpResponse).setStatusCode(HttpStatus.SC_NOT_FOUND);
    verify(httpResponse).setHeader(HttpHeader.CONTENT_TYPE.toString(), ContentType.APPLICATION_JSON.toString());

  }
}
