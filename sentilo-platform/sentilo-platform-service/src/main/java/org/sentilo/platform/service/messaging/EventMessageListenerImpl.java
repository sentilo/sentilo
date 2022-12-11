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
package org.sentilo.platform.service.messaging;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.sentilo.common.domain.EventMessage;
import org.sentilo.common.enums.EventType;
import org.sentilo.common.utils.MessagingUtils;
import org.sentilo.platform.common.domain.NotificationParams;
import org.sentilo.platform.service.notification.NotificationDeliveryContext;
import org.sentilo.platform.service.notification.NotificationDeliveryService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.redis.listener.Topic;
import org.springframework.util.Assert;

public class EventMessageListenerImpl implements EventMessageListener {

  private static final Logger LOGGER = LoggerFactory.getLogger(EventMessageListenerImpl.class);

  private final String name;

  /** Map with all of subscriptions that this listener is subscribed. */
  private final Map<String, NotificationParams> subscriptions = new HashMap<String, NotificationParams>();
  private NotificationDeliveryService notificationDeliveryService;
  private String tenant;

  public EventMessageListenerImpl(final String name) {
    super();
    Assert.notNull(name, "name must not be NULL");
    this.name = name;
  }

  @Override
  public void onMessage(final EventMessage message) {
    final String sMessage = MessagingUtils.marshal(message);
    final String channel = message.getTopic();

    LOGGER.debug("{} -->  Get message on channel {}", name, channel);
    LOGGER.debug("{} -->  Message body {}", name, message);

    final NotificationParams params = getNotificationParams(channel);
    final EventType eventType = EventType.valueOf(message.getType());
    notificationDeliveryService.pushNotification(sMessage, new NotificationDeliveryContext(params, name, tenant, eventType));
  }

  @Override
  public void addSubscription(final Topic topic, final NotificationParams params) {
    // If listener is already subscribe to topic, override its notification params
    subscriptions.put(topic.getTopic(), params);
  }

  @Override
  public void removeSubscription(final Topic topic) {
    if (subscriptions.containsKey(topic.getTopic())) {
      subscriptions.remove(topic.getTopic());
    }
  }

  public String getName() {
    return name;
  }

  public void setTenant(final String tenant) {
    this.tenant = tenant;
  }

  /**
   * Returns parameters associated with the notification of an event published in topic
   * <code>topic</code> This parameters could be defined exactly for given topic or could be defined
   * for a topic more general.
   *
   * For example, if given topic is /data/provider1/sensor1, process starts searching parameters for
   * topic /data/provider1/sensor1. Then, if not found, search move on to find parameters for topic
   * /data/provider1, and finally, last search is for topic /data
   *
   * @param topic
   * @return
   */
  private NotificationParams getNotificationParams(final String topic) {
    LOGGER.debug("Search notification params for topic {}", topic);
    NotificationParams parameters = null;
    if (!subscriptions.containsKey(topic)) {
      final List<String> topicCandidates = MessagingUtils.buildCandidates(topic);
      for (final String candidate : topicCandidates) {
        if (subscriptions.containsKey(candidate)) {
          parameters = subscriptions.get(candidate);
          break;
        }
      }
    } else {
      parameters = subscriptions.get(topic);
    }

    return parameters;
  }

  @Override
  public boolean equals(final Object obj) {
    if (obj == null || getClass() != obj.getClass()) {
      return false;
    }
    // Name is what identifies a message listener
    final EventMessageListenerImpl other = (EventMessageListenerImpl) obj;
    if (name == null) {
      return other.name == null;
    } else {
      return name.equals(other.name);
    }
  }

  @Override
  public int hashCode() {
    final int prime = 37;
    int result = 1;
    result = prime * result + (name == null ? 0 : name.hashCode());
    return result * super.hashCode();
  }

  public void setNotificationDeliveryService(final NotificationDeliveryService notificationDeliveryService) {
    this.notificationDeliveryService = notificationDeliveryService;
  }

}
