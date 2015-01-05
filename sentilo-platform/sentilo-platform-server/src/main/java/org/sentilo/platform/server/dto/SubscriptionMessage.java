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
package org.sentilo.platform.server.dto;

import org.codehaus.jackson.map.annotate.JsonSerialize;

public class SubscriptionMessage {

  @JsonSerialize(include = JsonSerialize.Inclusion.NON_NULL)
  private String endpoint;
  @JsonSerialize(include = JsonSerialize.Inclusion.NON_NULL)
  private String type;

  @JsonSerialize(include = JsonSerialize.Inclusion.NON_NULL)
  private String provider;
  @JsonSerialize(include = JsonSerialize.Inclusion.NON_NULL)
  private String sensor;
  @JsonSerialize(include = JsonSerialize.Inclusion.NON_NULL)
  private String alert;

  @JsonSerialize(include = JsonSerialize.Inclusion.NON_NULL)
  private String secretCallbackKey;

  public String getEndpoint() {
    return endpoint;
  }

  public void setEndpoint(final String endpoint) {
    this.endpoint = endpoint;
  }

  public String getType() {
    return type;
  }

  public void setType(final String type) {
    this.type = type;
  }

  public String getProvider() {
    return provider;
  }

  public void setProvider(final String provider) {
    this.provider = provider;
  }

  public String getSensor() {
    return sensor;
  }

  public void setSensor(final String sensor) {
    this.sensor = sensor;
  }

  public String getAlert() {
    return alert;
  }

  public void setAlert(final String alert) {
    this.alert = alert;
  }

  public String getSecretCallbackKey() {
    return secretCallbackKey;
  }

  public void setSecretCallbackKey(final String secretCallbackKey) {
    this.secretCallbackKey = secretCallbackKey;
  }
}
