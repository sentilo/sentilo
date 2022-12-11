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

import org.sentilo.common.enums.SubscribeType;
import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.common.utils.SentiloUtils;
import org.springframework.data.redis.listener.Topic;

public class Subscription {

  /** Topic to which this subscription is subscribed */
  protected Topic topic;

  /** Identificador de quien realiza la subscripcion */
  private final String sourceEntityId;

  /** Identificador del propietario del recurso al cual se realiza la subscripcion. */
  private String ownerEntityId;

  /** Tipo de subscripcion. */
  private SubscribeType type;

  /** Params to configure notifications related to this subscription */
  private NotificationParams notificationParams;

  public Subscription(final String sourceEntityId) {
    this.sourceEntityId = sourceEntityId;
  }

  public Subscription(final String sourceEntityId, final String targetEntityId) {
    this(sourceEntityId);
    ownerEntityId = targetEntityId;
  }

  public Subscription(final String sourceEntityId, final String targetEntityId, final SubscribeType type) {
    this(sourceEntityId, targetEntityId);
    this.type = type;
  }

  public static Subscription build(final String entityId, final String channel, final NotificationParams notificationParams) {

    // channel follows the following format: /<type>/<resourceId1>/<resourceId2>
    // where <resourceId2> is optional
    // and <resouceId1> could represent a pattern (ends with *)

    final String[] tokens = channel.split(SentiloConstants.REDIS_CHANNEL_TOKEN);
    final SubscribeType type = SubscribeType.valueOf(tokens[1].toUpperCase());
    Subscription subscription = null;
    String providerId, sensorId;
    switch (type) {
      case DATA:
        providerId = SentiloUtils.getElementAt(tokens, 2);
        sensorId = SentiloUtils.getElementAt(tokens, 3);
        subscription = new DataSubscription(entityId, providerId, sensorId, notificationParams);
        break;
      case ORDER:
        providerId = SentiloUtils.getElementAt(tokens, 2);
        sensorId = SentiloUtils.getElementAt(tokens, 3);
        subscription = new OrderSubscription(entityId, providerId, sensorId, notificationParams);
        break;
      case ALARM:
        final String alarmId = SentiloUtils.getElementAt(tokens, 2);
        subscription = new AlarmSubscription(entityId, null, alarmId, notificationParams);
        break;
    }

    return subscription;
  }

  /**
   * Returns topic to which subscription is subscribed.
   *
   * @return
   */
  public Topic getTopic() {
    return topic == null ? buildTopic() : topic;
  }

  /**
   * Metodo a sobrescribir por las clases hijas. Indica si la subscripcion identifica el recurso
   * sobre la que aplica.
   *
   * @return
   */
  public boolean hasResourceIdentified() {
    return false;
  }

  public String getSourceEntityId() {
    return sourceEntityId;
  }

  public String getOwnerEntityId() {
    return ownerEntityId;
  }

  public void setOwnerEntityId(final String ownerEntityId) {
    this.ownerEntityId = ownerEntityId;
  }

  public SubscribeType getType() {
    return type;
  }

  public NotificationParams getNotificationParams() {
    return notificationParams;
  }

  public void setNotificationParams(final NotificationParams notificationParams) {
    this.notificationParams = notificationParams;
  }

  protected Topic buildTopic() {
    // To implement by subclasses
    return null;
  }

}
