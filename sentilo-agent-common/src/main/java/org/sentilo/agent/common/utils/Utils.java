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
package org.sentilo.agent.common.utils;

import org.sentilo.common.enums.SubscribeType;
import org.springframework.data.redis.listener.ChannelTopic;
import org.springframework.data.redis.listener.PatternTopic;
import org.springframework.data.redis.listener.Topic;
import org.springframework.util.StringUtils;

public abstract class Utils {

  private Utils() {
    throw new AssertionError();
  }

  public static Topic buildTopic(final String topicName) {
    if (isTopicPattern(topicName)) {
      return new PatternTopic(topicName);
    } else {
      return new ChannelTopic(topicName);
    }
  }

  public static boolean isTopicPattern(final String topic) {
    return StringUtils.hasText(topic) && topic.endsWith(Constants.REDIS_CHANNEL_PATTERN_SUFFIX);
  }

  public static boolean isTopicOfType(final String topicName, final SubscribeType type) {
    return type != null && topicName.startsWith(type.toString().toLowerCase());
  }

  /**
   * This method validates subscription uri, i.e, it validates that follows the pattern
   *
   * /{subscription_type}/{resourceid1}/{resourceid2}
   *
   * where:
   * <ul>
   * <li>subscription_type must be data, alarm or order</li>
   * <li>resourceid1 and resourceid2 are optional and allow to subscribe to events of a specific
   * resource</li>
   * </ul>
   *
   * @param subscription
   * @return
   */
  public static boolean isValidSubscription(final String subscription) {
    boolean valid = false;

    if (StringUtils.hasText(subscription)) {
      try {
        final String[] tokens = subscription.split(Constants.TOPIC_TOKEN);
        if (tokens.length > 2 && tokens.length < 5) {
          SubscribeType.valueOf(tokens[1].toUpperCase());
          valid = true;
        }
      } catch (final IllegalArgumentException e) {
        // unknown SubscribeType tokens[1]
        valid = false;
      }
    }

    return valid;
  }

  public static String getPendingEventQueueName() {
    final String agentName = System.getProperty(Constants.SENTILO_AGENT_NAME_ENV);
    return agentName.toLowerCase() + Constants.PENDING_QUEUE_SUFFIX;
  }

  public static String getAgentName() {
    final String agentName = System.getProperty(Constants.SENTILO_AGENT_NAME_ENV);
    return agentName.toLowerCase();
  }
}
