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
package org.sentilo.platform.server.converter;

import java.util.List;

import org.apache.http.HttpStatus;
import org.sentilo.common.enums.SubscribeType;
import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.platform.common.domain.AlarmSubscription;
import org.sentilo.platform.common.domain.DataSubscription;
import org.sentilo.platform.common.domain.NotificationParams;
import org.sentilo.platform.common.domain.OrderSubscription;
import org.sentilo.platform.common.domain.Subscription;
import org.sentilo.platform.common.exception.PlatformException;
import org.sentilo.platform.server.dto.SubscriptionMessage;
import org.sentilo.platform.server.dto.SubscriptionsMessage;
import org.sentilo.platform.server.request.SentiloRequest;
import org.sentilo.platform.server.response.SentiloResponse;
import org.springframework.util.StringUtils;

public class SubscribeConverter extends PlatformJsonMessageConverter {

  public Subscription parseRequest(final SentiloRequest request) {
    return parseRequest(request, false);
  }

  public Subscription parseBasicRequest(final SentiloRequest request) {
    return parseRequest(request, true);
  }

  private Subscription parseRequest(final SentiloRequest request, final boolean isBasicRequest) {
    final boolean resourceHasEvent = resourceHasEvent(request);
    final SubscribeType subscribeType = resourceHasEvent ? getSubscribeType(request) : null;
    final NotificationParams notificationParams = isBasicRequest ? null : buildNotificationParams(request);

    Subscription subscription;
    if (subscribeType != null) {
      subscription = buildSubscriptionFromRequest(request, subscribeType, notificationParams);
    } else {
      // This case implies that the action must be applied to all subscriptions from an entity
      // (i.e., the entity that does the request and the entity on which the action lies are equals)
      final String entityId = request.getEntitySource();
      subscription = new Subscription(entityId, entityId);
      subscription.setNotificationParams(notificationParams);
    }

    return subscription;
  }

  private Subscription buildSubscriptionFromRequest(final SentiloRequest request, final SubscribeType subscribeType,
      final NotificationParams notificationParams) {
    Subscription subscription;
    switch (subscribeType) {
      case DATA:
        subscription = buildDataSubscription(request, notificationParams);
        break;
      case ORDER:
        subscription = buildOrderSubscription(request, notificationParams);
        break;
      case ALARM:
        subscription = buildAlarmSubscription(request, notificationParams);
        break;
      default:
        throw new PlatformException(HttpStatus.SC_BAD_REQUEST, "Illegal subscribe event type:" + subscribeType);
    }

    return subscription;
  }

  private Subscription buildDataSubscription(final SentiloRequest request, final NotificationParams notificationParams) {
    final boolean resourceHasEvent = resourceHasEvent(request);
    final String entityId = request.getEntitySource();
    final String providerId = resourceHasEvent ? request.getResourcePart(1) : request.getResourcePart(0);
    final String sensorId = resourceHasEvent ? request.getResourcePart(2) : request.getResourcePart(1);
    return new DataSubscription(entityId, providerId, sensorId, notificationParams);
  }

  private Subscription buildOrderSubscription(final SentiloRequest request, final NotificationParams notificationParams) {
    final String entityId = request.getEntitySource();
    final String providerId = request.getResourcePart(1);
    final String sensorId = request.getResourcePart(2);
    return new OrderSubscription(entityId, providerId, sensorId, notificationParams);
  }

  private Subscription buildAlarmSubscription(final SentiloRequest request, final NotificationParams notificationParams) {
    final String entityId = request.getEntitySource();
    final String alertId = request.getResourcePart(1);
    // The request doesn't identify who is the alert owner. It is filled in in a subsequent
    // step
    return new AlarmSubscription(entityId, null, alertId, notificationParams);
  }

  public void writeResponse(final SentiloResponse response, final List<Subscription> subscriptions) {
    // transformar a objeto de tipo SubscriptionsMessage
    final SubscriptionsMessage message = parseSubscriptionListToSubscriptionsMessage(subscriptions);
    writeInternal(message, response);
  }

  private NotificationParams buildNotificationParams(final SentiloRequest request) {
    final SubscriptionMessage inputMessage = (SubscriptionMessage) readInternal(SubscriptionMessage.class, request);

    final String endpoint = inputMessage != null ? inputMessage.getEndpoint() : null;
    final String secret = inputMessage != null ? inputMessage.getSecretCallbackKey() : null;
    final long maxRetries =
        inputMessage != null && inputMessage.getMaxRetries() > 0 ? inputMessage.getMaxRetries() : SentiloConstants.DEFAULT_MAX_RETRIES;
    final long retryDelay =
        inputMessage != null && inputMessage.getRetryDelay() > 0 ? inputMessage.getRetryDelay() : SentiloConstants.DEFAULT_RETRY_DELAY;

    return new NotificationParams(endpoint, secret, maxRetries, retryDelay);
  }

  private SubscriptionsMessage parseSubscriptionListToSubscriptionsMessage(final List<Subscription> subscriptionsList) {
    final SubscriptionsMessage subscriptions = new SubscriptionsMessage();
    for (final Subscription subscription : subscriptionsList) {
      subscriptions.addSubscription(parseSubscriptionToSubscribeMessage(subscription));
    }

    return subscriptions;
  }

  private SubscriptionMessage parseSubscriptionToSubscribeMessage(final Subscription subscription) {
    final SubscriptionMessage message = new SubscriptionMessage();

    message.setType(subscription.getType().toString());
    if (subscription.getNotificationParams() != null) {
      message.setEndpoint(subscription.getNotificationParams().getEndpoint());
      message.setMaxRetries(subscription.getNotificationParams().getMaxRetries());
      message.setRetryDelay(subscription.getNotificationParams().getRetryDelay());
    }

    switch (subscription.getType()) {
      case DATA:
        message.setProvider(((DataSubscription) subscription).getProviderId());
        message.setSensor(((DataSubscription) subscription).getSensorId());
        break;
      case ALARM:
        message.setAlert(((AlarmSubscription) subscription).getAlertId());
        break;
      case ORDER:
        message.setProvider(((OrderSubscription) subscription).getOwnerEntityId());
        message.setSensor(((OrderSubscription) subscription).getSensorId());
        break;
    }

    return message;
  }

  public boolean resourceHasEvent(final SentiloRequest request) {
    return getSubscribeType(request) != null ? true : false;
  }

  public SubscribeType getSubscribeType(final SentiloRequest request) {
    SubscribeType subscribeType = null;
    try {
      final String resourcePart = request.getResourcePart(0);
      if (StringUtils.hasText(resourcePart)) {
        subscribeType = SubscribeType.valueOf(resourcePart.toUpperCase());
      }
    } catch (final IllegalArgumentException e) {
    }

    return subscribeType;
  }

}
