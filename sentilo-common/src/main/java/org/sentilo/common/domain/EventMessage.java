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
package org.sentilo.common.domain;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

/**
 * Defines the message structure which is sent to the clients subscribed to any topic
 */
public class EventMessage {

  protected String message;
  protected String timestamp;
  @JsonInclude(value = Include.NON_NULL)
  protected String topic;
  @JsonInclude(value = Include.NON_NULL)
  protected String type;
  @JsonInclude(value = Include.NON_NULL)
  protected String component;
  @JsonInclude(value = Include.NON_NULL)
  protected String sensor;
  @JsonInclude(value = Include.NON_NULL)
  protected String provider;
  @JsonInclude(value = Include.NON_NULL)
  protected String location;
  @JsonInclude(value = Include.NON_NULL)
  protected String alert;
  @JsonInclude(value = Include.NON_NULL)
  protected String alertType;
  @JsonInclude(value = Include.NON_NULL)
  protected Long time;
  @JsonInclude(value = Include.NON_NULL)
  private String sensorType;
  @JsonInclude(value = Include.NON_NULL)
  private Integer retryAttempt;

  @JsonInclude(value = Include.NON_NULL)
  private String publisher;
  @JsonInclude(value = Include.NON_NULL)
  private Long publishedAt;
  @JsonInclude(value = Include.NON_NULL)
  private String publisherTenant;
  @JsonInclude(value = Include.NON_NULL)
  private String tenant;

  @JsonInclude(value = Include.NON_NULL)
  @Deprecated
  /** This field is deprecated. Rather than this field, the new field publisher should be used */
  private String sender;

  public EventMessage() {

  }

  public String toString() {
    final StringBuilder sb = new StringBuilder("\n--- Notification --- ");
    sb.append("\n\t message:" + message);
    sb.append("\n\t timestamp:" + timestamp);
    sb.append("\n\t time:" + (time != null ? time.longValue() : null));
    sb.append("\n\t topic:" + topic);
    sb.append("\n\t type:" + type);
    sb.append("\n\t provider:" + provider);
    sb.append("\n\t component:" + component);
    sb.append("\n\t sensor:" + sensor);
    sb.append("\n\t location:" + location);
    sb.append("\n\t alert:" + alert);
    sb.append("\n\t alertType:" + alertType);
    sb.append("\n\t publisher:" + publisher);
    sb.append("\n\t publishedTime:" + (publishedAt != null ? publishedAt.longValue() : null));
    sb.append("\n\t publisherTenant:" + publisherTenant);
    sb.append("\n\t tenant:" + tenant);
    sb.append("\n");
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

  public void setAlert(final String alert) {
    this.alert = alert;
  }

  public String getAlert() {
    return alert;
  }

  public Long getTime() {
    return time;
  }

  public void setTime(final Long time) {
    this.time = time;
  }

  public String getAlertType() {
    return alertType;
  }

  public void setAlertType(final String alertType) {
    this.alertType = alertType;
  }

  public String getSensorType() {
    return sensorType;
  }

  public void setSensorType(final String sensorType) {
    this.sensorType = sensorType;
  }

  public String getComponent() {
    return component;
  }

  public void setComponent(final String component) {
    this.component = component;
  }

  public String getPublisher() {
    return publisher;
  }

  public void setPublisher(final String publisher) {
    this.publisher = publisher;
  }

  public Long getPublishedAt() {
    return publishedAt;
  }

  public void setPublishedAt(final Long publishedAt) {
    this.publishedAt = publishedAt;
  }

  public String getPublisherTenant() {
    return publisherTenant;
  }

  public void setPublisherTenant(final String publisherTenant) {
    this.publisherTenant = publisherTenant;
  }

  public String getTenant() {
    return tenant;
  }

  public void setTenant(final String tenant) {
    this.tenant = tenant;
  }

  public String getSender() {
    return getPublisher();
  }

  public void setSender(final String sender) {
    setPublisher(sender);
  }

  public Integer getRetryAttempt() {
    return retryAttempt;
  }

  public void setRetryAttempt(final Integer retryAttempt) {
    this.retryAttempt = retryAttempt;
  }

}
