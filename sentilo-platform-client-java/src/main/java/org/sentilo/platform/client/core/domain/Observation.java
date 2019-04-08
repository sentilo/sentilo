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

import java.util.Date;

import org.sentilo.common.utils.DateUtils;
import org.springframework.util.StringUtils;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

public class Observation {

  @JsonInclude(value = Include.NON_NULL)
  private String value;

  @JsonInclude(value = Include.NON_NULL)
  private String sensor;

  @JsonInclude(value = Include.NON_NULL)
  private String provider;

  @JsonInclude(value = Include.NON_NULL)
  private String timestamp;

  @JsonInclude(value = Include.NON_NULL)
  private String location;

  @JsonInclude(value = Include.NON_NULL)
  private Long time;

  public Observation() {
    super();
  }

  public Observation(final String value) {
    this();
    this.value = value;
  }

  public Observation(final String value, final String timestamp) {
    this(value);
    this.timestamp = timestamp;
    if (time == null && StringUtils.hasLength(timestamp)) {
      time = DateUtils.toMillis(timestamp);
    }
  }

  public Observation(final String value, final String timestamp, final String location) {
    this(value, timestamp);
    this.location = location;
  }

  public Observation(final String value, final Date timestamp, final String location) {
    this(value, DateUtils.toStringTimestamp(timestamp), location);
  }

  public Observation(final String value, final Date timestamp) {
    this(value, timestamp, null);
  }

  public Observation(final String value, final long timestamp, final String location) {
    this(value, new Date(timestamp), location);
    time = timestamp;
  }

  public Observation(final String value, final long timestamp) {
    this(value, timestamp, null);
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder();
    sb.append("--- Observation ---");
    if (StringUtils.hasText(provider)) {
      sb.append("\n\t provider:").append(provider);
    }
    if (StringUtils.hasText(sensor)) {
      sb.append("\n\t sensor:").append(sensor);
    }
    sb.append("\n\t value:").append(value);
    sb.append("\n\t timestamp:").append(timestamp);
    sb.append("\n\t time:").append(time);
    sb.append("\n\t location:").append(location);

    return sb.toString();
  }

  public String getValue() {
    return value;
  }

  public void setValue(final String value) {
    this.value = value;
  }

  public String getSensor() {
    return sensor;
  }

  public void setSensor(final String sensor) {
    this.sensor = sensor;
  }

  public String getTimestamp() {
    return timestamp;
  }

  public void setTimestamp(final String timestamp) {
    this.timestamp = timestamp;
  }

  public String getLocation() {
    return location;
  }

  public void setLocation(final String location) {
    this.location = location;
  }

  public void setProvider(final String provider) {
    this.provider = provider;
  }

  public String getProvider() {
    return provider;
  }

  public Long getTime() {
    return time;
  }

  public void setTime(final Long time) {
    this.time = time;
  }
}
