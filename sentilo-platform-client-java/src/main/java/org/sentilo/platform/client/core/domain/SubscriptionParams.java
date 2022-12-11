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
package org.sentilo.platform.client.core.domain;

import org.springframework.util.StringUtils;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

public class SubscriptionParams {

  /**
   * URL of the remote service where to send the notification
   */
  @JsonInclude(value = Include.NON_NULL)
  private String endpoint;

  /**
   * Secret used to build the HMAC-SHA signature header for every notification.
   */
  @JsonInclude(value = Include.NON_NULL)
  private String secretCallbackKey;

  /**
   * Maximum number of allowed retries if the remote server doesn't responds with a 2xx HTTP code .
   */
  @JsonInclude(value = Include.NON_DEFAULT)
  private long maxRetries;

  /** Delay base factor between each retry in minutes. */
  @JsonInclude(value = Include.NON_DEFAULT)
  private long retryDelay;

  public SubscriptionParams() {
    super();
  }

  public SubscriptionParams(final String endpoint) {
    super();
    this.endpoint = endpoint;
  }

  public SubscriptionParams(final String endpoint, final String secretCallbackKey, final long maxRetries, final long retryDelay) {
    this(endpoint);
    this.secretCallbackKey = secretCallbackKey;
    this.maxRetries = maxRetries;
    this.retryDelay = retryDelay;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder();
    sb.append("endpoint: " + endpoint);
    sb.append(", maxRetries: " + maxRetries);
    sb.append(", retrtDelay (minutes): " + retryDelay);
    if (StringUtils.hasText(secretCallbackKey)) {
      sb.append(", secretCallbackKey: [protected]");
    }
    return sb.toString();
  }

  public String getEndpoint() {
    return endpoint;
  }

  public String getSecretCallbackKey() {
    return secretCallbackKey;
  }

  public long getMaxRetries() {
    return maxRetries;
  }

  public void setRetries(final long maxRetries) {
    this.maxRetries = maxRetries;
  }

  public long getRetryDelay() {
    return retryDelay;
  }

  public void setRetryDelay(final long retryDelay) {
    this.retryDelay = retryDelay;
  }

  public void setEndpoint(final String endpoint) {
    this.endpoint = endpoint;
  }

  public void setSecretCallbackKey(final String secretCallbackKey) {
    this.secretCallbackKey = secretCallbackKey;
  }

  public void setMaxRetries(final long maxRetries) {
    this.maxRetries = maxRetries;
  }

}
