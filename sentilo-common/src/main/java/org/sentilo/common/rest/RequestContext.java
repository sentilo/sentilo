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
package org.sentilo.common.rest;

import java.util.HashMap;
import java.util.Map;

import com.google.common.base.Objects;

public class RequestContext {

  private String host;
  private String path;
  private String identityToken;
  private String secretKey;

  private Map<String, String> headers;
  private RequestParameters parameters;
  private String body;

  public RequestContext() {
    super();
  }

  public RequestContext(final String path) {
    this();
    this.path = path;
  }

  public RequestContext(final String path, final String body) {
    this(path);
    this.body = body;
  }

  @Override
  public boolean equals(final Object obj) {
    // self check
    if (this == obj) {
      return true;
    }
    // null, type and cast check
    if (obj == null || getClass() != obj.getClass()) {
      return false;
    }

    final RequestContext rc = (RequestContext) obj;

    return Objects.equal(host, rc.getHost()) && Objects.equal(path, rc.getPath()) && Objects.equal(identityToken, rc.getIdentityToken())
        && Objects.equal(parameters, rc.getParameters()) && Objects.equal(secretKey, rc.getSecretKey());
  }

  @Override
  public int hashCode() {
    return Objects.hashCode(host, path, identityToken, parameters, secretKey);
  }

  public String getHost() {
    return host;
  }

  public void setHost(final String host) {
    this.host = host;
  }

  public String getPath() {
    return path;
  }

  public void setPath(final String path) {
    this.path = path;
  }

  public String getSecretKey() {
    return secretKey;
  }

  public void setSecretKey(final String secretKey) {
    this.secretKey = secretKey;
  }

  public RequestParameters getParameters() {
    return parameters;
  }

  public void setParameters(final RequestParameters parameters) {
    this.parameters = parameters;
  }

  public String getBody() {
    return body;
  }

  public void setBody(final String body) {
    this.body = body;
  }

  public String getIdentityToken() {
    return identityToken;
  }

  public void setIdentityToken(final String identityToken) {
    this.identityToken = identityToken;
  }

  public void addHeaders(final Map<String, String> headers) {
    this.headers = headers;
  }

  public void addHeader(final String name, final String value) {
    if (headers == null) {
      headers = new HashMap<String, String>();
    }

    headers.put(name, value);
  }

  public Map<String, String> getHeaders() {
    return headers;
  }
}
