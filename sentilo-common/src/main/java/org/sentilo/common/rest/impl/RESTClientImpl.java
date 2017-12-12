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
package org.sentilo.common.rest.impl;

import java.net.URI;
import java.security.GeneralSecurityException;
import java.security.KeyManagementException;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;

import javax.net.ssl.SSLContext;

import org.apache.http.HttpRequestInterceptor;
import org.apache.http.HttpResponse;
import org.apache.http.StatusLine;
import org.apache.http.auth.AuthScope;
import org.apache.http.auth.Credentials;
import org.apache.http.client.HttpClient;
import org.apache.http.client.config.RequestConfig;
import org.apache.http.client.methods.HttpDelete;
import org.apache.http.client.methods.HttpEntityEnclosingRequestBase;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.methods.HttpPut;
import org.apache.http.client.methods.HttpRequestBase;
import org.apache.http.config.Registry;
import org.apache.http.config.RegistryBuilder;
import org.apache.http.conn.socket.ConnectionSocketFactory;
import org.apache.http.conn.socket.PlainConnectionSocketFactory;
import org.apache.http.conn.ssl.NoopHostnameVerifier;
import org.apache.http.conn.ssl.SSLConnectionSocketFactory;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.BasicCredentialsProvider;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.impl.conn.PoolingHttpClientConnectionManager;
import org.apache.http.ssl.SSLContextBuilder;
import org.apache.http.ssl.TrustStrategy;
import org.apache.http.util.EntityUtils;
import org.sentilo.common.exception.RESTClientException;
import org.sentilo.common.rest.RESTClient;
import org.sentilo.common.rest.RequestContext;
import org.sentilo.common.rest.RequestParameters;
import org.sentilo.common.rest.hmac.HMACBuilder;
import org.sentilo.common.utils.DateUtils;
import org.sentilo.common.utils.RESTUtils;
import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.common.utils.URIUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.util.StringUtils;

public class RESTClientImpl implements RESTClient, InitializingBean {

  private static final Logger LOGGER = LoggerFactory.getLogger(RESTClientImpl.class);

  /**
   * Returns the timeout in milliseconds used when requesting a connection from the connection
   * manager.
   */
  private static final int DEFAULT_CONNECTION_POOL_TO_MS = 15 * 1000;
  /** Determines the timeout in milliseconds until a connection is established. */
  private static final int DEFAULT_CONNECTION_TO_MS = 15 * 1000;
  /**
   * Defines the socket timeout (SO_TIMEOUT) in milliseconds, which is the timeout for waiting for
   * data or, put differently, a maximum period inactivity between two consecutive data packets).
   */
  private static final int DEFAULT_READ_TO_MS = 15 * 1000;

  private HttpClient httpClient;
  private Credentials credentials;
  private final AuthScope authScope = AuthScope.ANY;
  private HttpRequestInterceptor[] interceptors;

  private String host;
  private String secretKey;
  private boolean noValidateCertificates = false;

  public RESTClientImpl() {
  }

  public String get(final RequestContext rc) throws RESTClientException {
    final String targetHost = getRequestTargetHost(rc);
    final URI uri = URIUtils.getURI(targetHost, rc.getPath(), rc.getParameters());
    final HttpGet get = new HttpGet(uri);

    return executeHttpCall(get, rc.getIdentityToken());
  }

  public String post(final RequestContext rc) throws RESTClientException {
    final String targetHost = getRequestTargetHost(rc);

    LOGGER.debug("Send post message to host {} and path {}", targetHost, rc.getPath());
    LOGGER.debug("Token {}", rc.getIdentityToken());
    LOGGER.debug("Body {}", rc.getBody());

    final URI uri = URIUtils.getURI(targetHost, rc.getPath());
    final HttpPost post = new HttpPost(uri);

    return executeHttpCall(post, rc.getBody(), rc.getIdentityToken());
  }

  public String put(final RequestContext rc) throws RESTClientException {
    final String targetHost = getRequestTargetHost(rc);
    final URI uri = URIUtils.getURI(targetHost, rc.getPath());

    final HttpPut put = new HttpPut(uri);

    return executeHttpCall(put, rc.getBody(), rc.getIdentityToken());
  }

  public String delete(final RequestContext rc) throws RESTClientException {
    final String targetHost = getRequestTargetHost(rc);

    // As a request DELETE cannot have body, we simulate the call to DELETE doing a PUT request with
    // the parameter method==delete
    final URI uri = StringUtils.hasText(rc.getBody()) ? URIUtils.getURI(targetHost, rc.getPath(), RequestParameters.buildDelete())
        : URIUtils.getURI(targetHost, rc.getPath());

    final HttpRequestBase delete = StringUtils.hasText(rc.getBody()) ? new HttpPut(uri) : new HttpDelete(uri);

    return executeHttpCall(delete, rc.getBody(), rc.getIdentityToken());
  }


  @Override
  public void afterPropertiesSet() throws Exception {
    if (httpClient == null) {

      final PoolingHttpClientConnectionManager pccm = noValidateCertificates
          ? new PoolingHttpClientConnectionManager(buildTrustSSLConnectionSocketFactory()) : new PoolingHttpClientConnectionManager();
      // Increase max total connection to 400
      pccm.setMaxTotal(400);
      // Increase default max connection per route to 50
      pccm.setDefaultMaxPerRoute(50);

      // Define timeouts
      RequestConfig.Builder requestBuilder = RequestConfig.custom();
      requestBuilder = requestBuilder.setSocketTimeout(DEFAULT_READ_TO_MS);
      requestBuilder = requestBuilder.setConnectTimeout(DEFAULT_CONNECTION_TO_MS);
      requestBuilder = requestBuilder.setConnectionRequestTimeout(DEFAULT_CONNECTION_POOL_TO_MS);

      final HttpClientBuilder httpClientBuilder = HttpClients.custom();
      httpClientBuilder.setDefaultRequestConfig(requestBuilder.build());
      httpClientBuilder.setConnectionManager(pccm);

      if (credentials != null) {
        final BasicCredentialsProvider credentialsProvider = new BasicCredentialsProvider();
        credentialsProvider.setCredentials(authScope, credentials);
        httpClientBuilder.setDefaultCredentialsProvider(credentialsProvider);
      }

      if (interceptors != null) {
        for (final HttpRequestInterceptor interceptor : interceptors) {
          httpClientBuilder.addInterceptorFirst(interceptor);
        }
      }

      httpClient = httpClientBuilder.build();
    }

  }

  public void destroy() throws Exception {
    // As recommended by HttpClient API, when the client is destroyed the related connectionManager
    // must be closed
    ((CloseableHttpClient) httpClient).close();
  }

  private String getRequestTargetHost(final RequestContext rc) {
    return StringUtils.hasText(rc.getHost()) ? rc.getHost() : host;
  }

  private void validateResponse(final HttpResponse response) throws RESTClientException {
    LOGGER.info("Response code: {}", response.getStatusLine().getStatusCode());
    // A response status code between 200 and 299 is considered a success status
    final StatusLine statusLine = response.getStatusLine();
    if (statusLine.getStatusCode() < 200 || statusLine.getStatusCode() > 299) {
      final StringBuilder sb = new StringBuilder();
      try {
        if (response.getEntity() != null) {
          sb.append(EntityUtils.toString(response.getEntity()));
        } else {
          sb.append(statusLine.getReasonPhrase());
        }
      } catch (final Exception e) {
        // Ignored
      }
      throw new RESTClientException(statusLine.getStatusCode(), sb.toString());
    }
  }

  private String executeHttpCall(final HttpRequestBase httpRequest, final String identityToken) throws RESTClientException {
    return executeHttpCall(httpRequest, null, identityToken);
  }

  private String executeHttpCall(final HttpRequestBase httpRequest, final String body, final String identityToken) throws RESTClientException {
    try {
      LOGGER.info("Executing http call to:  {} ", httpRequest.toString());
      if (StringUtils.hasText(body)) {
        ((HttpEntityEnclosingRequestBase) httpRequest).setEntity(new StringEntity(body, ContentType.APPLICATION_JSON));
      }

      if (StringUtils.hasText(identityToken)) {
        httpRequest.addHeader(RESTUtils.buildIdentityHeader(identityToken));
      }

      if (StringUtils.hasText(secretKey)) {
        addSignedHeader(httpRequest, body);
      }

      final HttpResponse response = httpClient.execute(httpRequest);
      validateResponse(response);
      if (response.getEntity() != null) {
        return EntityUtils.toString(response.getEntity());
      } else {
        return "";
      }

    } catch (final RESTClientException e) {
      LOGGER.error("Error executing http call: {} ", httpRequest.toString(), e);
      throw e;
    } catch (final Exception e) {
      LOGGER.error("Error executing http call: {} ", httpRequest.toString(), e);
      final String msg = String.format("Error executing http call: %s ", httpRequest.toString());
      throw new RESTClientException(msg, e);
    }
  }

  private void addSignedHeader(final HttpRequestBase httpRequest, final String body) throws GeneralSecurityException {
    final String currentDate = DateUtils.timestampToString(System.currentTimeMillis());
    final String hmac = HMACBuilder.buildHeader(body, host, secretKey, currentDate);
    httpRequest.addHeader(SentiloConstants.HMAC_HEADER, hmac);
    httpRequest.addHeader(SentiloConstants.DATE_HEADER, currentDate);
    LOGGER.debug("Add header {} with value {}", SentiloConstants.HMAC_HEADER, hmac);
    LOGGER.debug("Add header {} with value {}", SentiloConstants.DATE_HEADER, currentDate);
  }

  private Registry<ConnectionSocketFactory> buildTrustSSLConnectionSocketFactory()
      throws NoSuchAlgorithmException, KeyManagementException, KeyStoreException {
    // SSLContextFactory to allow all hosts. Without this an SSLException is thrown with either self
    // signed certs or certs signed by untrusted CA
    final SSLContext sslContext = new SSLContextBuilder().loadTrustMaterial(null, new TrustStrategy() {

      @Override
      public boolean isTrusted(final X509Certificate[] chain, final String authType) throws CertificateException {
        return true;
      }
    }).build();

    final SSLConnectionSocketFactory socketFactory = new SSLConnectionSocketFactory(sslContext, new NoopHostnameVerifier());
    return RegistryBuilder.<ConnectionSocketFactory>create().register("http", PlainConnectionSocketFactory.getSocketFactory())
        .register("https", socketFactory).build();
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

  public void setSecretKey(final String secretKey) {
    this.secretKey = secretKey;
  }

  public void setNoValidateCertificates(final boolean noValidateCertificates) {
    this.noValidateCertificates = noValidateCertificates;
  }

}
