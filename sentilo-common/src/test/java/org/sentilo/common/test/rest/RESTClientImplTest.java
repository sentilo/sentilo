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
package org.sentilo.common.test.rest;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.notNull;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import javax.net.ssl.SSLHandshakeException;

import org.apache.http.HttpRequestInterceptor;
import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.HttpVersion;
import org.apache.http.StatusLine;
import org.apache.http.auth.Credentials;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpDelete;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.methods.HttpPut;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.message.BasicStatusLine;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.exception.RESTClientException;
import org.sentilo.common.rest.RequestContext;
import org.sentilo.common.rest.RequestParameters;
import org.sentilo.common.rest.impl.RESTClientImpl;
import org.sentilo.common.rest.interceptor.IdentityKeyInterceptor;
import org.springframework.test.util.ReflectionTestUtils;

public class RESTClientImplTest {

  private final String host = "http://l27.0.0.1";

  @InjectMocks
  private RESTClientImpl restClient;

  @Mock
  private HttpClient httpClient;
  @Mock
  private HttpResponse httpResponse;
  @Mock
  private RequestParameters requestParameters;
  @Mock
  private Credentials credentials;
  @Mock
  private IdentityKeyInterceptor identityKeyInterceptor;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    final HttpRequestInterceptor[] interceptors = {identityKeyInterceptor};
    restClient.setHost(host);
    restClient.setInterceptors(interceptors);
  }

  @Test
  public void afterPropertiesSet() throws Exception {
    ReflectionTestUtils.setField(restClient, "httpClient", null);

    restClient.afterPropertiesSet();

    final HttpClient defaultHttpClient = (HttpClient) ReflectionTestUtils.getField(restClient, "httpClient");
    Assert.assertNotNull(defaultHttpClient);
    Assert.assertTrue(defaultHttpClient instanceof CloseableHttpClient);
  }

  @Test
  public void invalidGet() throws Exception {
    final String path = "data";
    final RequestContext rc = new RequestContext(path);
    final StatusLine statusLine = new BasicStatusLine(HttpVersion.HTTP_1_0, HttpStatus.SC_FORBIDDEN, "Forbidden request");

    when(httpClient.execute(notNull(HttpGet.class))).thenReturn(httpResponse);
    when(httpResponse.getStatusLine()).thenReturn(statusLine);

    try {
      restClient.get(rc);
    } catch (final RESTClientException rce) {
      Assert.assertEquals(HttpStatus.SC_FORBIDDEN, rce.getStatus());
      Assert.assertEquals("Forbidden request", rce.getMessage());
    }
  }

  @Test
  public void get() throws Exception {
    final String path = "/data";
    final RequestContext rc = new RequestContext(path);
    final String responseContent = "Lorem ipsum";
    final StatusLine statusLine = new BasicStatusLine(HttpVersion.HTTP_1_0, HttpStatus.SC_OK, "");

    when(httpClient.execute(notNull(HttpGet.class))).thenReturn(httpResponse);
    when(httpResponse.getStatusLine()).thenReturn(statusLine);
    when(httpResponse.getEntity()).thenReturn(new StringEntity(responseContent));

    final String result = restClient.get(rc);
    Assert.assertEquals(responseContent, result);
  }

  @Test
  public void getWithParameters() throws Exception {
    final String path = "/data";
    final RequestContext rc = new RequestContext(path);
    rc.setParameters(requestParameters);
    final String responseContent = "Lorem ipsum";
    final StatusLine statusLine = new BasicStatusLine(HttpVersion.HTTP_1_0, HttpStatus.SC_OK, "");

    when(httpClient.execute(notNull(HttpGet.class))).thenReturn(httpResponse);
    when(httpResponse.getStatusLine()).thenReturn(statusLine);
    when(httpResponse.getEntity()).thenReturn(new StringEntity(responseContent));

    final String result = restClient.get(rc);

    verify(httpClient).execute(any(HttpGet.class));
    Assert.assertEquals(responseContent, result);

  }

  @Test
  public void delete() throws Exception {
    final String path = "/data";
    final RequestContext rc = new RequestContext(path);
    final String responseContent = "Lorem ipsum";
    final StatusLine statusLine = new BasicStatusLine(HttpVersion.HTTP_1_0, HttpStatus.SC_OK, "");

    when(httpClient.execute(notNull(HttpDelete.class))).thenReturn(httpResponse);
    when(httpResponse.getStatusLine()).thenReturn(statusLine);
    when(httpResponse.getEntity()).thenReturn(new StringEntity(responseContent));

    final String result = restClient.delete(rc);

    verify(httpClient).execute(any(HttpDelete.class));
    Assert.assertEquals(responseContent, result);
  }

  @Test
  public void deleteWithBody() throws Exception {
    final String path = "/data";
    final String body = "body";
    final RequestContext rc = new RequestContext(path, body);
    final String responseContent = "Lorem ipsum";
    final StatusLine statusLine = new BasicStatusLine(HttpVersion.HTTP_1_0, HttpStatus.SC_OK, "");

    when(httpClient.execute(notNull(HttpPut.class))).thenReturn(httpResponse);
    when(httpResponse.getStatusLine()).thenReturn(statusLine);
    when(httpResponse.getEntity()).thenReturn(new StringEntity(responseContent));

    final String result = restClient.delete(rc);

    verify(httpClient).execute(any(HttpPut.class));
    Assert.assertEquals(responseContent, result);
  }

  @Test
  public void post() throws Exception {
    final String path = "/data";
    final String body = "body";
    final RequestContext rc = new RequestContext(path, body);
    final String responseContent = "Lorem ipsum";
    final StatusLine statusLine = new BasicStatusLine(HttpVersion.HTTP_1_0, HttpStatus.SC_OK, "");

    when(httpClient.execute(notNull(HttpPost.class))).thenReturn(httpResponse);
    when(httpResponse.getStatusLine()).thenReturn(statusLine);
    when(httpResponse.getEntity()).thenReturn(new StringEntity(responseContent));

    final String result = restClient.post(rc);

    verify(httpClient).execute(any(HttpPost.class));
    Assert.assertEquals(responseContent, result);
  }

  @Test
  public void put() throws Exception {
    final String path = "/data";
    final String body = "body";
    final RequestContext rc = new RequestContext(path, body);
    final String responseContent = "Lorem ipsum";
    final StatusLine statusLine = new BasicStatusLine(HttpVersion.HTTP_1_0, HttpStatus.SC_OK, "");

    when(httpClient.execute(notNull(HttpPut.class))).thenReturn(httpResponse);
    when(httpResponse.getStatusLine()).thenReturn(statusLine);
    when(httpResponse.getEntity()).thenReturn(new StringEntity(responseContent));

    final String result = restClient.put(rc);

    verify(httpClient).execute(any(HttpPut.class));
    Assert.assertEquals(responseContent, result);
  }

  @Test
  public void whenHttpsUrlIsConsumedThenSSLException() throws Exception {
    final String HOST_WITH_SSL = "https://catalog.thingtia.cloud";
    ReflectionTestUtils.setField(restClient, "httpClient", null);

    restClient.afterPropertiesSet();
    restClient.setHost(HOST_WITH_SSL);
    try {
      restClient.get(new RequestContext(""));
    } catch (final RESTClientException rce) {
      Assert.assertTrue(rce.getCause() instanceof SSLHandshakeException);
    }

  }

  @Test
  public void whenHttpsUrlIsConsumedThenCorrect() throws Exception {
    final String HOST_WITH_SSL = "https://catalog.thingtia.cloud";
    ReflectionTestUtils.setField(restClient, "httpClient", null);
    ReflectionTestUtils.setField(restClient, "noValidateCertificates", true);

    restClient.afterPropertiesSet();
    restClient.setHost(HOST_WITH_SSL);
    restClient.get(new RequestContext(""));
  }

}
