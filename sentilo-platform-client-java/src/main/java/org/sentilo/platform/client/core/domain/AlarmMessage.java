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

import org.sentilo.common.utils.DateUtils;
import org.springframework.util.StringUtils;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

public class AlarmMessage {

  @JsonInclude(value = Include.NON_NULL)
  private String message;
  @JsonInclude(value = Include.NON_NULL)
  private String timestamp;
  @JsonInclude(value = Include.NON_NULL)
  private String sender;
  @JsonInclude(value = Include.NON_NULL)
  private Long time;

  public AlarmMessage() {
    super();
  }

  public AlarmMessage(final String message, final String sender, final String timestamp) {
    this();
    this.message = message;
    this.sender = sender;
    this.timestamp = timestamp;
    time = DateUtils.toMillis(timestamp);
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder();
    sb.append("--- Alarm ---");
    if (StringUtils.hasText(sender)) {
      sb.append("\n\t sender:").append(sender);
    }
    sb.append("\n\t message:").append(message);
    sb.append("\n\t timestamp:").append(timestamp);
    sb.append("\n\t time:").append(time);

    return sb.toString();
  }

  @JsonIgnore
  public Long getTimestampToMillis() {
    return time;
  }

  public String getTimestamp() {
    return timestamp;
  }

  public void setTimestamp(final String timestamp) {
    this.timestamp = timestamp;
  }

  public String getMessage() {
    return message;
  }

  public void setMessage(final String message) {
    this.message = message;
  }

  public String getSender() {
    return sender;
  }

  public void setSender(final String sender) {
    this.sender = sender;
  }

  public Long getTime() {
    return time;
  }

  public void setTime(final Long time) {
    this.time = time;
  }
}
