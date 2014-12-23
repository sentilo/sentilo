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
package org.sentilo.platform.server.request;

import java.io.IOException;
import java.net.URI;
import java.nio.charset.Charset;
import java.util.List;
import java.util.Map;

import org.apache.http.Header;
import org.apache.http.HttpEntity;
import org.apache.http.HttpEntityEnclosingRequest;
import org.apache.http.HttpRequest;
import org.apache.http.NameValuePair;
import org.apache.http.ParseException;
import org.apache.http.client.utils.URLEncodedUtils;
import org.apache.http.entity.ContentType;
import org.apache.http.util.EntityUtils;
import org.sentilo.common.utils.SentiloUtils;
import org.sentilo.platform.server.auth.AuthenticationService;
import org.sentilo.platform.server.exception.UnauthorizedException;
import org.sentilo.platform.server.http.HttpHeader;
import org.sentilo.platform.server.http.HttpMethod;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

public class SentiloRequest {

  private final Logger logger = LoggerFactory.getLogger(SentiloRequestHandler.class);
  private static final String UTF8 = "UTF-8";

  private HttpRequest httpRequest;

  private RequestParameters parameters;
  private SentiloResource resource;
  /** Token del path de la peticion que identifica el servicio invocado. */
  private String handlerPath;
  private String uri;
  private String path;
  private String entitySource;
  private ContentType contentType;
  private HttpMethod method;

  public static SentiloRequest build(final HttpRequest httpRequest) {
    Assert.notNull(httpRequest);
    final SentiloRequest request = new SentiloRequest();
    request.parseHttpRequest(httpRequest);
    return request;
  }

  public void checkCredentialIntegrity(final AuthenticationService authenticationService) throws UnauthorizedException {
    final String credential = extractHeader(HttpHeader.IDENTITY_KEY);
    entitySource = authenticationService.getIdentity(credential);
  }

  public String getResourcePart(final int pos) {
    return resource.getResourcePart(pos);
  }

  public String getRequestParameter(final String paramName) {
    return parameters.get(paramName);
  }

  public Map<String, String> getParameters() {
    return parameters.getParameters();
  }

  /*
   * public void processResource(final String path) { resource = new
   * SentiloResource(RequestUtils.extractResource(path)); }
   */

  public void setPathParts(final String handlerPath, final String resourcePath) {
    this.handlerPath = handlerPath;
    resource = new SentiloResource(resourcePath);
  }

  private void debug(final HttpEntity entity) {
    if (logger.isDebugEnabled()) {
      logger.debug("Default charset: {}", Charset.defaultCharset());
      logger.debug("Entity Content-Type: {}", org.apache.http.entity.ContentType.getOrDefault(entity));
    }
  }

  private void parseHttpRequest(final HttpRequest request) {
    httpRequest = request;
    parseMethod();
    parseContentType();
    parseUri();
  }

  private void parseMethod() {
    method = HttpMethod.valueOf(httpRequest.getRequestLine().getMethod());
  }

  private void parseContentType() {
    final String contentTypeValue = extractHeader(HttpHeader.CONTENT_TYPE);
    try {
      contentType = (StringUtils.hasText(contentTypeValue) ? ContentType.parse(extractHeader(HttpHeader.CONTENT_TYPE)) : getDefaultContentType());
    } catch (final ParseException pe) {
      contentType = getDefaultContentType();
    }
    logger.debug("Parsed Content-type: {}", contentTypeValue);
  }

  private void parseUri() {
    uri = httpRequest.getRequestLine().getUri();
    final URI parsedUri = URI.create(uri);
    path = parsedUri.getPath();
    processUriParameters(parsedUri);
    // path has the format handler_path_tokens/resources_tokens
    // handlerPath = RequestUtils.extractHandlerPath(path);
    // processResource(path);
  }

  private void processUriParameters(final URI parsedUri) {
    final List<NameValuePair> pairs = URLEncodedUtils.parse(parsedUri, UTF8);
    parameters = new RequestParameters(pairs);
  }

  private String extractHeader(final HttpHeader header) {
    logger.debug("extractHeader: {}", header.toString());

    final Header[] requestHeaders = httpRequest.getHeaders(header.toString());
    return (SentiloUtils.arrayIsEmpty(requestHeaders) ? null : requestHeaders[0].getValue());
  }

  public String getHandlerPath() {
    return handlerPath;
  }

  public SentiloResource getResource() {
    return resource;
  }

  public String getUri() {
    return uri;
  }

  public String getBody() throws IOException {
    final HttpEntity entity = ((HttpEntityEnclosingRequest) httpRequest).getEntity();
    debug(entity);
    return EntityUtils.toString(entity, UTF8);
  }

  public ContentType getContentType() {
    return contentType;
  }

  public String getEntitySource() {
    return entitySource;
  }

  private ContentType getDefaultContentType() {
    return ContentType.APPLICATION_JSON;
  }

  public HttpMethod getMethod() {
    return method;
  }

  public String getPath() {
    return path;
  }

  public String toString() {
    final StringBuilder sb = new StringBuilder();
    sb.append("\n\t Entity source:  " + entitySource);
    sb.append("\n\t Service: " + handlerPath);
    if (resource != null) {
      sb.append("\n\t Resource: " + resource.toString());
    }

    return sb.toString();
  }
}
