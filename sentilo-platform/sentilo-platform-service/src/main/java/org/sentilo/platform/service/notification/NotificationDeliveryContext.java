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
package org.sentilo.platform.service.notification;

import org.sentilo.common.enums.EventType;
import org.sentilo.platform.common.domain.NotificationParams;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

public class NotificationDeliveryContext {

  @JsonInclude(value = Include.ALWAYS)
  private NotificationParams notificationParams;
  @JsonInclude(value = Include.ALWAYS)
  private String entity;
  @JsonInclude(value = Include.NON_NULL)
  private String tenant;
  @JsonInclude(value = Include.ALWAYS)
  private EventType eventType;

  public NotificationDeliveryContext() {
    super();
  }

  public NotificationDeliveryContext(final NotificationParams notificationParams, final String entity, final String tenant,
      final EventType eventType) {
    this();
    this.notificationParams = notificationParams;
    this.entity = entity;
    this.tenant = tenant;
    this.eventType = eventType;
  }

  public NotificationParams getNotificationParams() {
    return notificationParams;
  }

  public void setNotificationParams(final NotificationParams notificationParams) {
    this.notificationParams = notificationParams;
  }

  public String getEntity() {
    return entity;
  }

  public void setEntity(final String entity) {
    this.entity = entity;
  }

  public String getTenant() {
    return tenant;
  }

  public void setTenant(final String tenant) {
    this.tenant = tenant;
  }

  public EventType getEventType() {
    return eventType;
  }

  public void setEventType(final EventType eventType) {
    this.eventType = eventType;
  }

}
