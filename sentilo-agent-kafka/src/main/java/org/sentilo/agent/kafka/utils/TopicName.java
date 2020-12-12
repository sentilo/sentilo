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
package org.sentilo.agent.kafka.utils;

import org.sentilo.common.domain.EventMessage;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Helper class that creates Kafka topic name according to settings.
 *
 */
public class TopicName {

  private static final Logger LOGGER = LoggerFactory.getLogger(TopicName.class);

  public enum TopicNameMode {
    topicPerSensor, topicPerProvider, topicPerSensorType, topicPerMessageType, singleTopic;
  }

  private static TopicNameMode topicNameMode;
  private static String topicPrefix = "sentilo";
  private static String topicSeparator = ".";

  /**
   * Returns topic name either in form of &lt;event type&gt;.&lt;sensor type&gt; or &lt;event
   * type&gt;.&lt;component&gt;.&lt;sensor name&gt;, depending on property metrics.fromSensorType
   *
   *
   * Possible values of topicNameMode: topicPerSensor, topicPerProvider, topicPerSensorType,
   * topicPerMessageType, singleTopic Examples with value of kafka.topic.prefix set to "sentilo" and
   * kafka.topic.separator set to ".": topicPerSensor: sentilo.data.providerName.sensorName
   * topicPerProvider: sentilo.data.providerName topicPerSensorType: sentilo.data.temperature
   * topicPerMessageType: sentilo.data singleTopic: sentilo
   *
   * @param event
   */
  public static String createTopicName(final EventMessage event) {

    if (topicSeparator == null) {
      topicSeparator = "";
    }

    String topic;

    String prefix = "";
    if (topicPrefix != null && topicPrefix.length() > 0) {
      if (topicNameMode != TopicNameMode.singleTopic) {
        prefix = topicPrefix + topicSeparator;
      } else {
        prefix = topicPrefix;
      }

    }

    switch (topicNameMode) {

      case topicPerSensor:
        topic = prefix + event.getType().toLowerCase() + topicSeparator + event.getProvider() + topicSeparator + event.getSensor();
        break;
      case topicPerProvider:
        topic = prefix + event.getType().toLowerCase() + topicSeparator + event.getProvider();
        break;
      case topicPerSensorType:
        topic = prefix + event.getType().toLowerCase() + topicSeparator + event.getSensorType();
        break;
      case topicPerMessageType:
        topic = prefix + event.getType().toLowerCase();
        break;
      case singleTopic:
      default:
        if (prefix.length() < 1) {
          topic = "sentilo";
        } else {
          topic = prefix;
        }
    }

    LOGGER.debug("Using topic mode: {}", topicNameMode);
    return topic;
  }

  public static TopicNameMode getTopicNameMode() {
    return topicNameMode;
  }

  public static void setTopicNameMode(final TopicNameMode topicNameMode) {
    TopicName.topicNameMode = topicNameMode;
  }

  public static String getTopicPrefix() {
    return topicPrefix;
  }

  public static void setTopicPrefix(final String topicPrefix) {
    TopicName.topicPrefix = topicPrefix;
  }

  public static String getTopicSeparator() {
    return topicSeparator;
  }

  public static void setTopicSeparator(final String topicSeparator) {
    TopicName.topicSeparator = topicSeparator;
  }

}
