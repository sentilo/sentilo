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
package org.sentilo.platform.common.domain;

import java.util.Date;

import org.sentilo.common.domain.PlatformInputMessage;
import org.sentilo.common.domain.PlatformSearchInputMessage;
import org.sentilo.common.domain.QueryFilterParams;

public class OrderInputMessage implements PlatformInputMessage, PlatformSearchInputMessage {

  private String sender;
  private String providerId;
  private String sensorId;
  private String order;

  private QueryFilterParams queryFilters;

  public OrderInputMessage() {
    super();
  }

  public OrderInputMessage(final String providerId, final String sensorId) {
    this();
    this.providerId = providerId;
    this.sensorId = sensorId;
  }

  public OrderInputMessage(final String providerId, final String sensorId, final Date from, final Date to, final Integer limit) {
    this(providerId, sensorId);
    queryFilters = new QueryFilterParams(from, to, limit);
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.platform.client.core.domain.PlatformSearchInputMessage#getQueryFilters()
   */
  public QueryFilterParams getQueryFilters() {
    return queryFilters;
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.platform.client.core.domain.PlatformSearchInputMessage#hasQueryFilters()
   */
  public boolean hasQueryFilters() {
    return queryFilters != null;
  }

  public String getProviderId() {
    return providerId;
  }

  public void setProviderId(final String providerId) {
    this.providerId = providerId;
  }

  public String getSensorId() {
    return sensorId;
  }

  public void setSensorId(final String sensorId) {
    this.sensorId = sensorId;
  }

  public String getOrder() {
    return order;
  }

  public void setOrder(final String order) {
    this.order = order;
  }

  public void setSender(final String sender) {
    this.sender = sender;
  }

  public String getSender() {
    return sender;
  }
}
