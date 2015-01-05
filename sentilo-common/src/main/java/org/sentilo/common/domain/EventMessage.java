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
package org.sentilo.common.domain;

import org.codehaus.jackson.map.annotate.JsonSerialize;

/**
 * Aquesta classe conté l'estructura del missatge que s'envia als clients de les subscripcions a la
 * plataforma.
 */
public class EventMessage {

  private String message;
  private String timestamp;
  @JsonSerialize(include = JsonSerialize.Inclusion.NON_NULL)
  private String topic;
  @JsonSerialize(include = JsonSerialize.Inclusion.NON_NULL)
  private String type;
  @JsonSerialize(include = JsonSerialize.Inclusion.NON_NULL)
  private String sensor;
  @JsonSerialize(include = JsonSerialize.Inclusion.NON_NULL)
  private String provider;
  @JsonSerialize(include = JsonSerialize.Inclusion.NON_NULL)
  private String location;
  @JsonSerialize(include = JsonSerialize.Inclusion.NON_NULL)
  private String sender;
  @JsonSerialize(include = JsonSerialize.Inclusion.NON_NULL)
  private String alert;

  public EventMessage() {

  }

  public String toString() {
    final StringBuilder sb = new StringBuilder("\n--- Notification --- ");
    sb.append("\n\t message:" + message);
    sb.append("\n\t timestamp:" + timestamp);
    sb.append("\n\t topic:" + topic);
    sb.append("\n\t type:" + type);
    sb.append("\n\t provider:" + provider);
    sb.append("\n\t sensor:" + sensor);
    sb.append("\n\t location:" + location);
    return sb.toString();
  }

  public String getMessage() {
    return message;
  }

  public void setMessage(final String message) {
    this.message = message;
  }

  public String getTopic() {
    return topic;
  }

  public void setTopic(final String topic) {
    this.topic = topic;
  }

  public String getTimestamp() {
    return timestamp;
  }

  public void setTimestamp(final String timestamp) {
    this.timestamp = timestamp;
  }

  public String getType() {
    return type;
  }

  public void setType(final String type) {
    this.type = type;
  }

  public String getSensor() {
    return sensor;
  }

  public void setSensor(final String sensor) {
    this.sensor = sensor;
  }

  public String getProvider() {
    return provider;
  }

  public void setProvider(final String provider) {
    this.provider = provider;
  }

  public String getLocation() {
    return location;
  }

  public void setLocation(final String location) {
    this.location = location;
  }

  public String getSender() {
    return sender;
  }

  public void setSender(final String sender) {
    this.sender = sender;
  }

  public void setAlert(final String alert) {
    this.alert = alert;
  }

  public String getAlert() {
    return alert;
  }

}
