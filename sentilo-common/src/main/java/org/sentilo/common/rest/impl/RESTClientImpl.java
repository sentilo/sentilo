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
import java.util.concurrent.TimeUnit;

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
import org.apache.http.conn.ConnectionKeepAliveStrategy;
import org.apache.http.conn.socket.ConnectionSocketFactory;
import org.apache.http.conn.socket.PlainConnectionSocketFactory;
import org.apache.http.conn.ssl.NoopHostnameVerifier;
import org.apache.http.conn.ssl.SSLConnectionSocketFactory;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.BasicCredentialsProvider;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.DefaultConnectionKeepAliveStrategy;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.impl.client.IdleConnectionEvictor;
import org.apache.http.impl.conn.PoolingHttpClientConnectionManager;
import org.apache.http.protocol.HttpContext;
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
import org.springframework.util.CollectionUtils;
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
  private HttpRequestInterceptor[] interceptors;
  private IdleConnectionEvictor idleConnectionMonitor;

  private String host;
  private String secretKey;
  private boolean noValidateCertificates = false;

  // Connection pool parameters
  private int maxTotalConnections = 400;
  private int maxTotalConnectionsPerRoute = 50;
  private long connectionTimeToLiveMs = DEFAULT_READ_TO_MS;

  public RESTClientImpl() {
  }

  public String get(final RequestContext rc) {
    final String targetHost = getRequestTargetHost(rc);
    final URI uri = URIUtils.getURI(targetHost, rc.getPath(), rc.getParameters());
    final HttpGet get = new HttpGet(uri);

    return executeHttpCall(get, rc);
  }

  public String post(final RequestContext rc) {
    final String targetHost = getRequestTargetHost(rc);

    LOGGER.debug("Send post message to host {} and path {}", targetHost, rc.getPath());
    LOGGER.debug("Token {}", rc.getIdentityToken());
    LOGGER.debug("Body {}", rc.getBody());

    final URI uri = URIUtils.getURI(targetHost, rc.getPath());
    final HttpPost post = new HttpPost(uri);

    return executeHttpCall(post, rc.getBody(), rc);
  }

  public String put(final RequestContext rc) {
    final String targetHost = getRequestTargetHost(rc);
    final URI uri = URIUtils.getURI(targetHost, rc.getPath());

    final HttpPut put = new HttpPut(uri);

    return executeHttpCall(put, rc.getBody(), rc);
  }

  public String delete(final RequestContext rc) {
    final String targetHost = getRequestTargetHost(rc);

    // As a request DELETE cannot have body, we simulate the call to DELETE doing a PUT request with
    // the parameter method==delete
    final URI uri = StringUtils.hasText(rc.getBody()) ? URIUtils.getURI(targetHost, rc.getPath(), RequestParameters.buildDelete())
        : URIUtils.getURI(targetHost, rc.getPath());

    final HttpRequestBase delete = StringUtils.hasText(rc.getBody()) ? new HttpPut(uri) : new HttpDelete(uri);

    return executeHttpCall(delete, rc.getBody(), rc);
  }

  @Override
  public void afterPropertiesSet() throws Exception {
    if (httpClient == null) {

      final PoolingHttpClientConnectionManager pccm = noValidateCertificates
          ? new PoolingHttpClientConnectionManager(buildTrustSSLConnectionSocketFactory()) : new PoolingHttpClientConnectionManager();
      // Set max total connection
      pccm.setMaxTotal(maxTotalConnections);
      // Set default max connection per route
      pccm.setDefaultMaxPerRoute(maxTotalConnectionsPerRoute);

      // Keep alive header definition:
      // https://tools.ietf.org/id/draft-thomson-hybi-http-timeout-01.html#rfc.section.2.1
      final ConnectionKeepAliveStrategy keepAliveStrategy = new DefaultConnectionKeepAliveStrategy() {

        public long getKeepAliveDuration(final HttpResponse response, final HttpContext context) {
          long keepAliveDuration = super.getKeepAliveDuration(response, context);
          // By default, if server response no defines any keep-alive timeout it is set to equals
          // connectionTimeToLiveMs
          if (keepAliveDuration == -1) {
            keepAliveDuration = connectionTimeToLiveMs;
          }
          return keepAliveDuration;
        }
      };

      // idle connection monitor thread: runs every 30 seconds to remove from the pool both closed
      // connections and idle connections with an inactivity time greater than 10s
      idleConnectionMonitor = new IdleConnectionEvictor(pccm, 30, TimeUnit.SECONDS, 10, TimeUnit.SECONDS);
      idleConnectionMonitor.start();

      // Define timeouts
      RequestConfig.Builder requestBuilder = RequestConfig.custom();
      requestBuilder = requestBuilder.setSocketTimeout(DEFAULT_READ_TO_MS);
      requestBuilder = requestBuilder.setConnectTimeout(DEFAULT_CONNECTION_TO_MS);
      requestBuilder = requestBuilder.setConnectionRequestTimeout(DEFAULT_CONNECTION_POOL_TO_MS);

      final HttpClientBuilder httpClientBuilder = HttpClients.custom();
      httpClientBuilder.setDefaultRequestConfig(requestBuilder.build());
      httpClientBuilder.setKeepAliveStrategy(keepAliveStrategy);
      httpClientBuilder.setRetryHandler(new SentiloHttpRequestRetryHandler());
      httpClientBuilder.setConnectionManager(pccm);

      if (credentials != null) {
        final BasicCredentialsProvider credentialsProvider = new BasicCredentialsProvider();
        credentialsProvider.setCredentials(AuthScope.ANY, credentials);
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
    // must be closed and the idle connection monitor thread must be stopped
    if (idleConnectionMonitor != null) {
      idleConnectionMonitor.shutdown();
    }

    ((CloseableHttpClient) httpClient).close();
  }

  private String getRequestTargetHost(final RequestContext rc) {
    return StringUtils.hasText(rc.getHost()) ? rc.getHost() : host;
  }

  private void validateResponse(final HttpResponse response) {
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
      LOGGER.debug("Response error message: {}", sb.toString());
      throw new RESTClientException(statusLine.getStatusCode(), sb.toString());
    }
  }

  private String executeHttpCall(final HttpRequestBase httpRequest, final RequestContext rc) {
    return executeHttpCall(httpRequest, null, rc);
  }

  private String executeHttpCall(final HttpRequestBase httpRequest, final String body, final RequestContext rc) {
    try {
      LOGGER.info("Executing http call to:  {} ", httpRequest.toString());
      prepareRequest(httpRequest, body, rc);
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

  private void prepareRequest(final HttpRequestBase httpRequest, final String body, final RequestContext rc) throws GeneralSecurityException {
    if (StringUtils.hasText(body)) {
      ((HttpEntityEnclosingRequestBase) httpRequest).setEntity(new StringEntity(body, ContentType.APPLICATION_JSON));
    }

    // Add custom headers
    if (!CollectionUtils.isEmpty(rc.getHeaders())) {
      rc.getHeaders().forEach((key, value) -> httpRequest.addHeader(key, value));
    }

    if (StringUtils.hasText(rc.getIdentityToken())) {
      httpRequest.addHeader(RESTUtils.buildIdentityHeader(rc.getIdentityToken()));
    }

    if (StringUtils.hasText(secretKey) || StringUtils.hasText(rc.getSecretKey())) {
      addSignedHeader(httpRequest, body, rc);
    }
  }

  private void addSignedHeader(final HttpRequestBase httpRequest, final String body, final RequestContext rc) throws GeneralSecurityException {
    final String targetHost = getRequestTargetHost(rc);
    final String currentSecretKey = StringUtils.hasText(rc.getSecretKey()) ? rc.getSecretKey() : secretKey;
    final String currentDate = DateUtils.timestampToString(System.currentTimeMillis());
    final String hmac = HMACBuilder.buildHeader(body, targetHost, currentSecretKey, currentDate);
    httpRequest.addHeader(SentiloConstants.HMAC_HEADER, hmac);
    httpRequest.addHeader(SentiloConstants.DATE_HEADER, currentDate);

    LOGGER.trace("HMAC header build params -- body {}  -- host {} -- secretKey {} -- date {} ", body, targetHost, currentSecretKey, currentDate);

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
