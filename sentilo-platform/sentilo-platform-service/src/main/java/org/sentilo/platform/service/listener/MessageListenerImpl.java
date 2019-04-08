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
package org.sentilo.platform.service.listener;

import java.util.HashMap;
import java.util.Map;

import org.sentilo.common.enums.EventType;
import org.sentilo.platform.common.domain.NotificationParams;
import org.sentilo.platform.service.notification.NotificationDeliveryContext;
import org.sentilo.platform.service.notification.NotificationDeliveryService;
import org.sentilo.platform.service.utils.ChannelUtils;
import org.sentilo.platform.service.utils.ChannelUtils.PubSubChannelPrefix;
import org.sentilo.platform.service.utils.PubSubConstants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.redis.connection.Message;
import org.springframework.data.redis.connection.MessageListener;
import org.springframework.data.redis.listener.Topic;
import org.springframework.data.redis.serializer.RedisSerializer;
import org.springframework.data.redis.serializer.StringRedisSerializer;
import org.springframework.util.Assert;

public class MessageListenerImpl implements MessageListener {

  private static final Logger LOGGER = LoggerFactory.getLogger(MessageListenerImpl.class);

  private final String name;
  private final RedisSerializer<String> serializer = new StringRedisSerializer();
  /** Map with all of subscriptions that this listener is subscribed. */
  private final Map<String, NotificationParams> subscriptions = new HashMap<String, NotificationParams>();
  private NotificationDeliveryService notificationDeliveryService;
  private String tenant;

  public MessageListenerImpl(final String name) {
    super();
    Assert.notNull(name, "name must not be NULL");
    this.name = name;
  }

  public void onMessage(final Message message, final byte[] pattern) {
    final String info = getInfo(message);
    final String channel = getChannel(message);

    LOGGER.debug("{} -->  Get message on channel {}", name, channel);
    LOGGER.debug("{} -->  Message body {}", name, info);

    final NotificationParams params = getNotificationParams(channel);
    final EventType eventType = getEventType(channel);
    notificationDeliveryService.pushNotification(info, new NotificationDeliveryContext(params, name, tenant, eventType));
  }

  public void addSubscription(final Topic topic, final NotificationParams params) {
    // If listener is already subscribe to topic, override its notification params
    subscriptions.put(topic.getTopic(), params);
  }

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

  protected String getInfo(final Message message) {
    return serializer.deserialize(message.getBody());
  }

  protected String getChannel(final Message message) {
    return serializer.deserialize(message.getChannel());
  }

  private EventType getEventType(final String channel) {
    final String[] tokens = channel.split(PubSubConstants.REDIS_CHANNEL_TOKEN);
    final PubSubChannelPrefix type = PubSubChannelPrefix.valueOf(tokens[1]);
    switch (type) {
      case data:
        return EventType.DATA;
      case alarm:
        return EventType.ALARM;
      case order:
        return EventType.ORDER;
      default:
        return null;
    }
  }

  private NotificationParams getNotificationParams(final String channel) {
    // Para saber que parametros utilizar para enviar la notificacion simplemente se debe recuperar
    // el valor asociado a
    // channel en el map de subscriptions, ya que este valor contiene los parámetros.
    // Pero al recuperar se debe tener en cuenta que el listener puede estar subscrito a un Channel
    // o a un Pattern.
    // Es decir, se puede estar subscrito o bien a
    // /data/providerId/sensorId (Channel)
    // o
    // /data/providerId* (Pattern)

    LOGGER.debug("Search notification params for channel {}", channel);
    NotificationParams params = subscriptions.get(channel);
    LOGGER.debug("Found params {} ", params);

    if (params == null && !ChannelUtils.isPatternTopic(channel)) {
      final String channelToPattern = ChannelUtils.channelToPattern(channel);
      LOGGER.debug("Search notification params for pattern {} ", channelToPattern);
      params = subscriptions.get(channelToPattern);
      LOGGER.debug("Found params {} ", params);
    }

    return params;
  }

  @Override
  public boolean equals(final Object obj) {
    if (obj == null || getClass() != obj.getClass()) {
      return false;
    }
    // Name is what identifies a message listener
    final MessageListenerImpl other = (MessageListenerImpl) obj;
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
