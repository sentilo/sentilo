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
package org.sentilo.platform.client.core.domain;

import java.util.ArrayList;
import java.util.List;

import org.codehaus.jackson.annotate.JsonIgnore;
import org.codehaus.jackson.map.annotate.JsonSerialize;
import org.sentilo.common.domain.PlatformSearchInputMessage;
import org.sentilo.common.domain.QueryFilterParams;
import org.sentilo.platform.client.core.utils.ResourcesUtils;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

public class AlarmInputMessage implements PlatformClientInputMessage, PlatformSearchInputMessage {

  @JsonSerialize(include = JsonSerialize.Inclusion.NON_NULL)
  private final String alertId;
  @JsonSerialize(include = JsonSerialize.Inclusion.NON_NULL)
  private String message;
  @JsonSerialize(include = JsonSerialize.Inclusion.NON_NULL)
  private String providerId;
  @JsonSerialize(include = JsonSerialize.Inclusion.NON_NULL)
  private String sensorId;

  @JsonIgnore
  private QueryFilterParams queryFilters;

  @JsonIgnore
  private String identityToken;

  /** Lista ordenada de los identificadores que forman el path del recurso. */
  @JsonIgnore
  private final List<String> resourcesValues = new ArrayList<String>();

  public AlarmInputMessage(final String alertId) {
    super();

    Assert.isTrue(StringUtils.hasText(alertId));

    this.alertId = alertId;

    ResourcesUtils.addToResources(alertId, resourcesValues);
  }

  public AlarmInputMessage(final String alarmId, final String message) {
    this(alarmId);

    Assert.isTrue(StringUtils.hasText(message));

    this.message = message;
  }

  public AlarmInputMessage(final String alarmId, final QueryFilterParams queryFilters) {
    this(alarmId);
    this.queryFilters = queryFilters;
  }

  @Override
  public String getIdentityToken() {
    return identityToken;
  }

  @Override
  public void setIdentityToken(final String identityToken) {
    this.identityToken = identityToken;
  }

  public String getAlertId() {
    return alertId;
  }

  public String getMessage() {
    return message;
  }

  @Override
  public List<String> getResourcesValues() {
    return resourcesValues;
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.sentilo.platform.client.core.domain.PlatformSearchInputMessage#getQueryFilters()
   */
  @Override
  public QueryFilterParams getQueryFilters() {
    return queryFilters;
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.sentilo.platform.client.core.domain.PlatformSearchInputMessage#hasQueryFilters()
   */
  @Override
  public boolean hasQueryFilters() {
    return queryFilters != null;
  }

  @Override
  public String toString() {
    final StringBuffer sb = new StringBuffer();
    sb.append("\n\t --- Alarm ---");
    sb.append("\n\t alert:").append(alertId);
    sb.append("\n\t message:").append(message);

    if (StringUtils.hasText(providerId)) {
      sb.append("\n\t provider:").append(providerId);
    }

    if (StringUtils.hasText(sensorId)) {
      sb.append("\n\t sensor:").append(sensorId);
    }

    if (hasQueryFilters()) {
      sb.append(getQueryFilters());
    }
    return sb.toString();
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

}
