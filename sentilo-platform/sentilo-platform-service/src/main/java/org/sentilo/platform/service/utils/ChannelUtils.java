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
package org.sentilo.platform.service.utils;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.sentilo.common.converter.DefaultStringMessageConverter;
import org.sentilo.common.converter.StringMessageConverter;
import org.sentilo.common.enums.SubscribeType;
import org.sentilo.platform.common.domain.AlarmSubscription;
import org.sentilo.platform.common.domain.DataSubscription;
import org.sentilo.platform.common.domain.NotificationParams;
import org.sentilo.platform.common.domain.Observation;
import org.sentilo.platform.common.domain.OrderSubscription;
import org.sentilo.platform.common.domain.Subscription;
import org.springframework.data.redis.listener.ChannelTopic;
import org.springframework.data.redis.listener.PatternTopic;
import org.springframework.data.redis.listener.Topic;
import org.springframework.util.Assert;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

public final class ChannelUtils {

  private static StringMessageConverter converter = new DefaultStringMessageConverter();

  public static enum PubSubChannelPrefix {
    data, order, alarm
  };

  private ChannelUtils() {
    throw new AssertionError();
  }

  public static PubSubChannelPrefix translateSubscriptionType(final SubscribeType subscribeType) {
    return PubSubChannelPrefix.valueOf(subscribeType.name().toLowerCase());
  }

  public static List<String> filterTopicsOfType(final Set<String> topics, final SubscribeType type) {
    final List<String> filteredTopics = new ArrayList<String>();
    if (!CollectionUtils.isEmpty(topics)) {
      final Iterator<String> it = topics.iterator();

      while (it.hasNext()) {
        final String topicName = it.next();
        if (isTopicOfType(topicName, type)) {
          filteredTopics.add(topicName);
        }
      }
    }

    return filteredTopics;
  }

  public static boolean isTopicOfType(final String topicName, final SubscribeType type) {
    final String prefixToCompare = PubSubConstants.REDIS_CHANNEL_TOKEN.concat(type.name().toLowerCase());
    return type != null && topicName.startsWith(prefixToCompare);
  }

  public static Topic getChannel(final Observation observation) {
    Assert.notNull(observation, "observation must not be null");
    Assert.notNull(observation.getProvider(), "An observation must have a provider");
    Assert.notNull(observation.getSensor(), "An observation must have a sensor");
    return buildTopic(PubSubChannelPrefix.data, observation.getProvider(), observation.getSensor());
  }

  public static Topic getChannel(final Subscription subscription) {
    Assert.notNull(subscription);
    switch (subscription.getType()) {
      case DATA:
        return buildTopic(PubSubChannelPrefix.data, ((DataSubscription) subscription).getProviderId(),
            ((DataSubscription) subscription).getSensorId());
      case ORDER:
        return buildTopic(PubSubChannelPrefix.order, ((OrderSubscription) subscription).getOwnerEntityId(),
            ((OrderSubscription) subscription).getSensorId());
      case ALARM:
        return buildTopic(PubSubChannelPrefix.alarm, ((AlarmSubscription) subscription).getAlertId());
      default:
        throw new IllegalArgumentException("Unknown subscription type:" + subscription.getType());
    }
  }

  public static Subscription getSubscription(final String entityId, final String channel, final String notificationParamsJson) {
    Assert.notNull(channel);

    // channel follow the following format: /<type>/<resourceId1>/<resourceId2>
    // where <resourceId2> is optional
    // and <resouceId1> could represent a pattern (ends with *)

    final String[] tokens = channel.split(PubSubConstants.REDIS_CHANNEL_TOKEN);
    final PubSubChannelPrefix type = PubSubChannelPrefix.valueOf(tokens[1]);
    final NotificationParams notificationParams = (NotificationParams) converter.unmarshal(notificationParamsJson, NotificationParams.class);
    Subscription subscription = null;
    String providerId, sensorId;
    switch (type) {
      case data:
        providerId = tokens[2];
        sensorId = !providerId.endsWith(PubSubConstants.REDIS_CHANNEL_PATTERN_SUFFIX) && tokens.length > 3 ? tokens[3] : null;
        subscription = new DataSubscription(entityId, providerId, sensorId, notificationParams);
        break;
      case order:
        providerId = tokens[2];
        sensorId = !providerId.endsWith(PubSubConstants.REDIS_CHANNEL_PATTERN_SUFFIX) && tokens.length > 3 ? tokens[3] : null;
        subscription = new OrderSubscription(entityId, providerId, sensorId, notificationParams);
        break;
      case alarm:
        final String alarmId = tokens[2];
        subscription = new AlarmSubscription(entityId, null, alarmId, notificationParams);
        break;
    }

    return subscription;
  }

  public static Topic buildTopic(final PubSubChannelPrefix prefix, final String... resources) {
    // El atributo resources puede tener 1 o 2 elementos. En caso de tener longitud 2 y que el
    // segundo sea null este metodo debe retornar un Topic de tipo Pattern y no Channel, ya que
    // significa que queremos subscribirnos a todos los datos, de cierto tipo, publicados para
    // un proveedor.

    final StringBuilder sb = new StringBuilder(PubSubConstants.REDIS_CHANNEL_TOKEN);
    sb.append(prefix.name());
    for (final String resource : resources) {
      if (StringUtils.hasText(resource)) {
        sb.append(PubSubConstants.REDIS_CHANNEL_TOKEN);
        sb.append(resource);
      } else if (resources.length == 2) {
        sb.append(PubSubConstants.REDIS_CHANNEL_PATTERN_SUFFIX);
      }
    }

    return buildTopic(sb.toString());
  }

  public static Topic buildTopic(final String topicName) {
    if (isPatternTopic(topicName)) {
      return new PatternTopic(topicName);
    } else {
      return new ChannelTopic(topicName);
    }
  }

  public static boolean isPatternTopic(final String topic) {
    return StringUtils.hasText(topic) && topic.endsWith(PubSubConstants.REDIS_CHANNEL_PATTERN_SUFFIX);
  }

  public static String channelToPattern(final String channel) {

    // This method transforms a channel with the format /<type>/<resourceId1>/<resourceId2>
    // which represents (in a business sense) a subscription to any data of type <type>
    // published by the resource identified by <resourceId1>/<resourceId2>, in a parent pattern
    // channel, i.e., a channel with the format /<type>/<resourceId1>*.

    String pattern = channel;

    if (!isPatternTopic(channel)) {
      final int lastPos = channel.lastIndexOf(PubSubConstants.REDIS_CHANNEL_TOKEN);
      pattern = channel.substring(0, lastPos).concat(PubSubConstants.REDIS_CHANNEL_PATTERN_SUFFIX);
    }

    return pattern;
  }
}
