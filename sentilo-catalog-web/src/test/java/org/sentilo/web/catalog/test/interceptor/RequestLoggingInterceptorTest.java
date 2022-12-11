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
package org.sentilo.web.catalog.test.interceptor;

import static org.mockito.Matchers.argThat;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.powermock.api.mockito.PowerMockito.mock;
import static org.powermock.api.mockito.PowerMockito.mockStatic;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.junit.Assert;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.ArgumentMatcher;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import org.sentilo.common.enums.HttpHeader;
import org.sentilo.common.enums.HttpMethod;
import org.sentilo.web.catalog.interceptor.RequestLoggingInterceptor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@RunWith(PowerMockRunner.class)
@PrepareForTest({RequestLoggingInterceptor.class, LoggerFactory.class, Logger.class})
public class RequestLoggingInterceptorTest {

  private final static String CLIENT_ADDRESS = "127.0.0.1";

  @InjectMocks
  private RequestLoggingInterceptor interceptor;

  @Mock
  private HttpServletRequest request;

  @Mock
  private HttpServletResponse response;

  @Mock
  private Object handler;

  private static Logger logger;

  @BeforeClass
  public static void setUpStatic() throws Exception {
    mockStatic(LoggerFactory.class);
    logger = mock(Logger.class);
    when(LoggerFactory.getLogger(RequestLoggingInterceptor.class)).thenReturn(logger);
  }

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void preHandle() throws Exception {
    final String requestUri = "/sentilo-catalog-web/map/view";
    final String requestContextPath = "sentilo-catalog-web";
    when(request.getHeader(HttpHeader.X_FORWARDED_FOR.name())).thenReturn(CLIENT_ADDRESS).thenReturn(null);
    when(request.getRemoteAddr()).thenReturn(CLIENT_ADDRESS);
    when(request.getMethod()).thenReturn(HttpMethod.GET.name());
    when(request.getContextPath()).thenReturn(requestContextPath);
    when(request.getRequestURI()).thenReturn(requestUri);

    final boolean result1 = interceptor.preHandle(request, response, handler);
    final boolean result2 = interceptor.preHandle(request, response, handler);

    Assert.assertTrue(result1 && result2);
    verify(logger, times(2)).info(argThat(new RequestLoggingMessageMatcher(CLIENT_ADDRESS, HttpMethod.GET, requestUri)));
    verify(request, times(2)).getHeader(HttpHeader.X_FORWARDED_FOR.name());
    verify(request).getRemoteAddr();
  }

  class RequestLoggingMessageMatcher extends ArgumentMatcher<String> {

    private final String clientAddress;
    private final String clientUri;
    private final HttpMethod requestMethod;

    public RequestLoggingMessageMatcher(final String clientAddress, final HttpMethod requestMethod, final String clientUri) {
      this.clientAddress = clientAddress;
      this.clientUri = clientUri;
      this.requestMethod = requestMethod;
    }

    @Override
    public boolean matches(final Object argument) {
      final String message = (String) argument;

      final boolean containsClientAddress = message.contains("remote_addr=" + clientAddress);
      final boolean containsClientMethodAndUri = message.contains(";request=" + requestMethod.name() + " " + clientUri);

      return containsClientAddress && containsClientMethodAndUri;
    }

  }

}
