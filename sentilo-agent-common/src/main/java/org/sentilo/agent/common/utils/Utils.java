/*
 * Sentilo
 * 
 * Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de Barcelona.
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

import org.sentilo.common.domain.SubscribeType;
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
    return (type != null && topicName.startsWith(type.toString().toLowerCase()));
  }

  /**
   * Valida el format de la subscripcio. És a dir, valida que tingui un format vàlid per a Sentilo,
   * i.e., que el format sigui /subscription_type/resourceid1/resourceid2 on
   * <ul>
   * <li>subscription_type ha de ser data, alarm o order</li>
   * <li>resourceid1 i resourceid2 són opcionals i permeten afitar més a quin recurs és vol fer la
   * subscripció</li>
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
      }
    }

    return valid;
  }
}
