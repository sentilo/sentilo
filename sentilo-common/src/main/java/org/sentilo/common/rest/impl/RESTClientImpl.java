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
package org.sentilo.common.rest.impl;

import org.apache.http.HttpRequestInterceptor;
import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.StatusLine;
import org.apache.http.auth.AuthScope;
import org.apache.http.auth.Credentials;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpDelete;
import org.apache.http.client.methods.HttpEntityEnclosingRequestBase;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.methods.HttpPut;
import org.apache.http.client.methods.HttpRequestBase;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.impl.conn.PoolingClientConnectionManager;
import org.apache.http.params.HttpConnectionParams;
import org.apache.http.util.EntityUtils;
import org.sentilo.common.exception.RESTClientException;
import org.sentilo.common.rest.RESTClient;
import org.sentilo.common.rest.RequestParameters;
import org.sentilo.common.utils.RESTUtils;
import org.sentilo.common.utils.URIUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.util.StringUtils;

public class RESTClientImpl implements RESTClient, InitializingBean {

  private final Logger logger = LoggerFactory.getLogger(RESTClientImpl.class);

  private static final int DEFAULT_CONNECTION_TIMEOUT_MILLISECONDS = (60 * 1000);
  private static final int DEFAULT_READ_TIMEOUT_MILLISECONDS = (60 * 1000);

  private HttpClient httpClient;
  private Credentials credentials;
  private final AuthScope authScope = AuthScope.ANY;
  private HttpRequestInterceptor[] interceptors;

  private String host;

  public RESTClientImpl() {
  }

  public String get(final String path) throws RESTClientException {
    return get(path, (String) null);
  }

  public String get(final String path, final String identityToken) throws RESTClientException {
    return get(path, (RequestParameters) null, identityToken);
  }

  public String get(final String path, final RequestParameters parameters) throws RESTClientException {
    return get(path, parameters, (String) null);
  }

  public String get(final String path, final RequestParameters parameters, final String identityToken) throws RESTClientException {

    final String url = URIUtils.getURI(host, path, parameters);
    final HttpGet get = new HttpGet(url);

    return executeHttpCall(get, identityToken);
  }

  public String post(final String path, final String body) throws RESTClientException {
    return post(path, body, null);
  }

  public String post(final String path, final String body, final String identityToken) throws RESTClientException {
    logger.debug("Send post message to host {} and path {}", host, path);
    final String url = URIUtils.getURI(host, path);
    final HttpPost post = new HttpPost(url);
    logger.debug("Token {}", identityToken);
    logger.debug("Body {}", body);
    return executeHttpCall(post, body, identityToken);
  }

  public String put(final String path, final String body) throws RESTClientException {
    return put(path, body, null);
  }

  public String put(final String path, final String body, final String identityToken) throws RESTClientException {
    final String url = URIUtils.getURI(host, path);
    final HttpPut put = new HttpPut(url);

    return executeHttpCall(put, body, identityToken);
  }

  public String delete(final String path) throws RESTClientException {
    return delete(path, null);
  }

  public String delete(final String path, final String identityToken) throws RESTClientException {
    return delete(path, identityToken, null);
  }

  public String delete(final String path, final String body, final String identityToken) throws RESTClientException {

    // Una peticion DELETE no puede tener body por lo que en caso de que venga informado el
    // parametro body,
    // simulamos la llamada al DELETE haciendo un PUT con el parametro method=delete
    final String url = (StringUtils.hasText(body) ? URIUtils.getURI(host, path, RequestParameters.buildDelete()) : URIUtils.getURI(host, path));
    final HttpRequestBase delete = (StringUtils.hasText(body) ? new HttpPut(url) : new HttpDelete(url));

    return executeHttpCall(delete, body, identityToken);
  }

  @Override
  public void afterPropertiesSet() throws Exception {
    if (httpClient == null) {
      httpClient = new DefaultHttpClient(new PoolingClientConnectionManager());
      // Fijamos los timeouts de establecimiento de conexion y de lectura de respuesta
      setConnectionTimeout(DEFAULT_CONNECTION_TIMEOUT_MILLISECONDS);
      setReadTimeout(DEFAULT_READ_TIMEOUT_MILLISECONDS);
    }

    if (interceptors != null && httpClient instanceof DefaultHttpClient) {
      for (final HttpRequestInterceptor interceptor : interceptors) {
        ((DefaultHttpClient) httpClient).addRequestInterceptor(interceptor);
      }
    }

    if (credentials != null && httpClient instanceof DefaultHttpClient) {
      ((DefaultHttpClient) httpClient).getCredentialsProvider().setCredentials(authScope, credentials);

    }
  }

  public void destroy() throws Exception {
    // Tal y como recomienda la API de HttpClient, al destruir la clase cliente cerramos el
    // connectionManager asociado a la
    // clase HttpClient.
    httpClient.getConnectionManager().shutdown();
  }

  private void validateResponse(final HttpResponse response) throws RESTClientException {
    if (response.getStatusLine().getStatusCode() != HttpStatus.SC_OK) {
      final StatusLine line = response.getStatusLine();
      final StringBuilder sb = new StringBuilder(line.getReasonPhrase());
      try {
        if (response.getEntity() != null) {
          sb.append(EntityUtils.toString(response.getEntity()) + ". ");
        }
      } catch (final Exception e) {
        // Ignored
      }
      throw new RESTClientException(line.getStatusCode(), sb.toString());
    }
  }

  private String executeHttpCall(final HttpRequestBase httpRequest, final String identityToken) throws RESTClientException {
    return executeHttpCall(httpRequest, null, identityToken);
  }

  private String executeHttpCall(final HttpRequestBase httpRequest, final String body, final String identityToken) throws RESTClientException {
    try {
      logger.debug("Executing http call {} ", httpRequest.toString());
      if (StringUtils.hasText(body)) {
        ((HttpEntityEnclosingRequestBase) httpRequest).setEntity(new StringEntity(body, ContentType.APPLICATION_JSON));
      }

      if (StringUtils.hasText(identityToken)) {
        httpRequest.addHeader(RESTUtils.buildIdentityHeader(identityToken));
      }
      final HttpResponse response = httpClient.execute(httpRequest);
      validateResponse(response);
      return EntityUtils.toString(response.getEntity());
    } catch (final RESTClientException e) {
      throw e;
    } catch (final Exception e) {
      final String msg = String.format("Error while executing http call: %s ", httpRequest.toString());
      throw new RESTClientException(msg, e);
    }
  }

  /**
   * Sets the timeout until a connection is established. A value of 0 means <em>never</em> timeout.
   * 
   * @param timeout the timeout value in milliseconds
   * @see org.apache.http.params.HttpConnectionParams#setConnectionTimeout(org.apache.http.params.HttpParams,
   *      int)
   */
  public void setConnectionTimeout(final int timeout) {
    if (timeout < 0) {
      throw new IllegalArgumentException("timeout must be a non-negative value");
    }
    HttpConnectionParams.setConnectionTimeout(httpClient.getParams(), timeout);
  }

  /**
   * Set the socket read timeout for the underlying HttpClient. A value of 0 means <em>never</em>
   * timeout.
   * 
   * @param timeout the timeout value in milliseconds
   * @see org.apache.http.params.HttpConnectionParams#setSoTimeout(org.apache.http.params.HttpParams,
   *      int)
   */
  public void setReadTimeout(final int timeout) {
    if (timeout < 0) {
      throw new IllegalArgumentException("timeout must be a non-negative value");
    }
    HttpConnectionParams.setSoTimeout(httpClient.getParams(), timeout);
  }

  public void setHost(final String host) {
    this.host = host;
  }

  public void setHttpClient(final HttpClient httpClient) {
    this.httpClient = httpClient;
  }

  public void setInterceptors(final HttpRequestInterceptor[] interceptors) {
    this.interceptors = interceptors;
  }

  public void setCredentials(final Credentials credentials) {
    this.credentials = credentials;
  }
}
