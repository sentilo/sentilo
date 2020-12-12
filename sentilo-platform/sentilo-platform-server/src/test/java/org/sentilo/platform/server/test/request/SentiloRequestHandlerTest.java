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
package org.sentilo.platform.server.test.request;

import static org.mockito.Matchers.any;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.List;

import org.apache.http.HttpRequest;
import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.RequestLine;
import org.apache.http.entity.ContentType;
import org.apache.http.protocol.HttpContext;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.enums.HttpHeader;
import org.sentilo.platform.common.exception.PlatformException;
import org.sentilo.platform.server.exception.SSLRequiredException;
import org.sentilo.platform.server.exception.UnauthorizedException;
import org.sentilo.platform.server.handler.AbstractHandler;
import org.sentilo.platform.server.handler.HandlerLocator;
import org.sentilo.platform.server.request.SentiloRequest;
import org.sentilo.platform.server.request.SentiloRequestHandler;
import org.sentilo.platform.server.request.interceptor.CredentialInterceptor;
import org.sentilo.platform.server.request.interceptor.SSLAccessInterceptor;
import org.sentilo.platform.server.request.interceptor.SentiloRequestHandlerInterceptor;
import org.springframework.test.util.ReflectionTestUtils;

public class SentiloRequestHandlerTest {

  @Mock
  private HandlerLocator handlerLocator;

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
  private CredentialInterceptor credentialInterceptor;
  @Mock
  private SSLAccessInterceptor sslAccessInterceptor;

  @InjectMocks
  private SentiloRequestHandler requestHandler;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    final List<SentiloRequestHandlerInterceptor> requestInterceptors = Arrays.asList(credentialInterceptor, sslAccessInterceptor);
    ReflectionTestUtils.setField(requestHandler, "requestInterceptors", requestInterceptors);
  }

  @Test
  public void handler() throws Exception {
    when(httpRequest.getRequestLine()).thenReturn(requestLine);
    when(requestLine.getMethod()).thenReturn("GET");
    when(requestLine.getUri()).thenReturn("http://lab.sentilo.io/data/mock");
    when(handlerLocator.lookup(any(SentiloRequest.class))).thenReturn(handler);

    requestHandler.handle(httpRequest, httpResponse, httpContext);

    // verify(handler).manageRequest(any(SentiloRequest.class), any(SentiloResponse.class));
    verify(httpResponse).setStatusCode(HttpStatus.SC_OK);
    verify(httpResponse).setHeader(HttpHeader.CONTENT_TYPE.toString(), ContentType.APPLICATION_JSON.toString());
  }

  @Test
  public void platformAccessException() {
    when(httpRequest.getRequestLine()).thenReturn(requestLine);
    when(requestLine.getMethod()).thenReturn("GET");
    when(requestLine.getUri()).thenReturn("http://lab.sentilo.io/data/mock");
    doThrow(new UnauthorizedException("Invalid credential ***")).when(credentialInterceptor).invoke(any(SentiloRequest.class));

    requestHandler.handle(httpRequest, httpResponse, httpContext);

    verify(httpResponse).setStatusCode(HttpStatus.SC_UNAUTHORIZED);
    verify(httpResponse).setHeader(HttpHeader.CONTENT_TYPE.toString(), ContentType.APPLICATION_JSON.toString());
  }

  @Test
  public void exception() {
    when(httpRequest.getRequestLine()).thenReturn(requestLine);
    when(requestLine.getMethod()).thenReturn("GET");
    doThrow(NullPointerException.class).when(requestLine).getUri();

    requestHandler.handle(httpRequest, httpResponse, httpContext);

    verify(httpResponse).setStatusCode(HttpStatus.SC_INTERNAL_SERVER_ERROR);
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

  @Test
  public void sslRequired() {
    when(httpRequest.getRequestLine()).thenReturn(requestLine);
    when(requestLine.getMethod()).thenReturn("GET");
    when(requestLine.getUri()).thenReturn("http://lab.sentilo.io/data/mock");
    doThrow(new SSLRequiredException()).when(sslAccessInterceptor).invoke(any(SentiloRequest.class));

    requestHandler.handle(httpRequest, httpResponse, httpContext);

    verify(httpResponse).setStatusCode(HttpStatus.SC_FORBIDDEN);
    verify(httpResponse).setHeader(HttpHeader.CONTENT_TYPE.toString(), ContentType.APPLICATION_JSON.toString());
  }

  public class MockPlatformException extends PlatformException {

    private static final long serialVersionUID = 1L;

    public MockPlatformException() {
      super(HttpStatus.SC_UNAUTHORIZED, "mock error message");
    }
  }
}
