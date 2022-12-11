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

import org.sentilo.common.utils.MessagingUtils;
import org.sentilo.common.utils.SentiloConstants;
import org.springframework.data.redis.listener.ChannelTopic;
import org.springframework.data.redis.listener.PatternTopic;
import org.springframework.data.redis.listener.Topic;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

public abstract class AgentMessagingUtils extends MessagingUtils {

  private AgentMessagingUtils() {
    throw new AssertionError();
  }

  /**
   * This method is deprecated and only is used for agents which uses Redis Publish/Subscribe
   * mechanism. Will be deleted in next version when all agents will be migrated to us Redis Stream
   * mechanism
   *
   * @param topicName
   * @return
   * @deprecated as of 2.0.0
   */
  @Deprecated
  public static Topic buildTopic(final String topicName) {
    final String formattedTopicName = formatTopic(topicName);
    if (isTopicPattern(formattedTopicName)) {
      return new PatternTopic(formattedTopicName);
    } else {
      return new ChannelTopic(formattedTopicName);
    }
  }

  private static boolean isTopicPattern(final String topic) {
    return StringUtils.hasText(topic) && topic.endsWith(SentiloAgentConstants.REDIS_CHANNEL_PATTERN_SUFFIX);
  }

  private static String formatTopic(final String srcTopic) {
    // @formatter:off
    // A client could be subscribed to:
    // - All events of a given event type (e.g. /data , /order or /alarm)
    // - or all events of a given event type and a determined provider (/data/provider1, /order/provider1 or /alarm/provider1)
    // - or all events from a specified sensor (e.g. /data/provider1/sensor2)
    // So if source topic has 1 or 2 elements, then this method ensures that it ends with a slash
    // character followed by the * character to indicates that it represents a pattern in the
    // publish/subscribe Redis context. Otherwise it represents a channel.

    // @formatter:on
    final String aux = srcTopic.startsWith(SentiloConstants.SLASH) ? srcTopic : SentiloConstants.SLASH + srcTopic;
    final String aux2 = aux.endsWith(SentiloConstants.SLASH) ? aux.substring(0, srcTopic.length() - 1) : aux;
    return aux2.split(SentiloConstants.SLASH).length < 4 ? aux2 + SentiloConstants.SLASH + SentiloAgentConstants.REDIS_CHANNEL_PATTERN_SUFFIX : aux2;
  }

  public static String getPendingEventQueueName() {
    final String agentName = System.getProperty(SentiloConstants.SENTILO_AGENT_NAME_ENV);
    return agentName.toLowerCase() + SentiloAgentConstants.PENDING_QUEUE_SUFFIX;
  }

  public static String getAgentName() {
    final String agentName = System.getProperty(SentiloConstants.SENTILO_AGENT_NAME_ENV);
    Assert.hasText(agentName, "JVM variable sentilo.agent.name is mandatory!!!");
    return agentName.toLowerCase();
  }

  public static String getSimpleNodeAgentName() {
    final String nodeAgentName = System.getProperty(SentiloConstants.SENTILO_NODE_AGENT_NAME_ENV);
    // Assert.hasText(nodeAgentName, "JVM variable sentilo.node.agent.name is mandatory!!!");
    return nodeAgentName;
  }

  public static String getGroupName() {
    final String agentName = System.getProperty(SentiloConstants.SENTILO_AGENT_NAME_ENV);
    return agentName.toLowerCase() + SentiloAgentConstants.STREAM_GROUP_SUFFIX;
  }

  public static String getConsumerName() {
    final String envSuffix = getSimpleNodeAgentName();
    final String nodeAgentName = StringUtils.hasText(envSuffix) ? envSuffix.toLowerCase() : getHostName();
    final String agentName = getAgentName();
    return agentName + SentiloAgentConstants.STREAM_CONSUMER_SUFFIX + nodeAgentName;
  }

}
