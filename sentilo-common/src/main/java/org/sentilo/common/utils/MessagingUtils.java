/*
 * Sentilo
 *  
 * Original version 1.4 Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de
 * Barcelona. Modified by Opentrends adding support for multitenant deployments and SaaS. 
 * Modifications on version 1.5 Copyright (C) 2015 Opentrends Solucions i Sistemes, S.L.
 * 
 *   
 * This program is licensed and may be used, modified and redistributed under the terms  of the 
 * European Public License (EUPL), either version 1.1 or (at your option) any later version as soon 
 * as they are approved by the European Commission.
 *   
 * Alternatively, you may redistribute and/or modify this program under the terms of the GNU Lesser 
 * General Public License as published by the Free Software Foundation; either  version 3 of the 
 * License, or (at your option) any later version. 
 *   
 * Unless required by applicable law or agreed to in writing, software distributed under the License 
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR  CONDITIONS OF ANY KIND, either express 
 * or implied. 
 *   
 * See the licenses for the specific language governing permissions, limitations and more details.
 *   
 * You should have received a copy of the EUPL1.1 and the LGPLv3 licenses along with this program; 
 * if not, you may find them at: 
 *   
 * https://joinup.ec.europa.eu/software/page/eupl/licence-eupl http://www.gnu.org/licenses/   and 
 * https://www.gnu.org/licenses/lgpl.txt
 */
package org.sentilo.common.utils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import org.sentilo.common.converter.DefaultStringMessageConverter;
import org.sentilo.common.converter.StringMessageConverter;
import org.sentilo.common.enums.EventType;
import org.sentilo.common.enums.SubscribeType;
import org.springframework.data.redis.listener.ChannelTopic;
import org.springframework.data.redis.listener.Topic;
import org.springframework.util.StringUtils;

public abstract class MessagingUtils extends SentiloUtils {

  private static final StringMessageConverter converter = new DefaultStringMessageConverter();

  protected MessagingUtils() {
    // this prevents even the native class from calling this ctor as well :
    throw new AssertionError();
  }

  /**
   * This method validates given subscription topic, i.e, it validates given topic that follows the
   * pattern:
   * <p>
   * <code>/{subscription_type}/{resourceid1}/{resourceid2} </code>
   * </p>
   * where:
   * <ul>
   * <li>subscription_type must be data, alarm or order</li>
   * <li>resourceid1 and resourceid2 are optional and allow to subscribe to events of a specific
   * resource</li>
   * </ul>
   *
   * @param topic
   * @return
   */
  public static boolean isValidSubscription(final String topic) {
    boolean valid = false;

    if (StringUtils.hasText(topic)) {
      try {
        final String[] tokens = topic.split(SentiloConstants.REDIS_CHANNEL_TOKEN);
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

  /**
   * Returns a list consisting of the given topics that match the given type.
   *
   * @param topics
   * @param type
   * @return
   */
  public static List<String> filterTopicsOfType(final Set<String> topics, final SubscribeType type) {
    final String prefix = SentiloConstants.REDIS_CHANNEL_TOKEN + type.name().toLowerCase();
    return topics.stream().filter(topic -> topic.startsWith(prefix)).collect(Collectors.toList());
  }

  /**
   * Returns a new ChannelTopic representing a channel given by resources parameters which are
   * joined using a / as delimiter.
   *
   * For example, if resources are <code>{"mockProvider","mockSensor"} </code> and type is
   * <code>DATA</code> then returns <code>/data/mockProvider/mockSensor</code>
   *
   * @param type
   * @param resources
   * @return
   */
  public static Topic buildTopic(final SubscribeType type, final String... resources) {
    final String delimiter = SentiloConstants.REDIS_CHANNEL_TOKEN;
    final String topicPrefix = type.name().toLowerCase();
    final List<String> aux = new ArrayList<String>(Arrays.asList(resources));
    aux.add(0, topicPrefix);

    // Filter either null or empty values
    final List<String> topicTokens = aux.stream().filter(e -> StringUtils.hasText(e)).collect(Collectors.toList());
    return new ChannelTopic(delimiter + String.join(delimiter, topicTokens));
  }

  /**
   * Given an event's topic, generates a list of topic candidates to match a subscription to this
   * event, from more exclusive candidate to less: i.e. returns a list with all tree-topic nodes
   * defined by given topic ordered from the leaf to the root.
   *
   * <p>
   * For example, if event's topic is /data/mockProvider/mockSensor then final candidates list is
   * [/data/mockProvider/mockSensor, /data/mockProvider, /data]
   *
   * @param topic
   * @return
   */
  public static List<String> buildCandidates(final String topic) {
    final List<String> candidates = new ArrayList<String>();
    final List<String> aux = new ArrayList<String>(Arrays.asList(topic.split(SentiloConstants.SLASH)));
    while (aux.size() > 0) {
      final String candidate = String.join(SentiloConstants.SLASH, aux);
      if (StringUtils.hasText(candidate)) {
        candidates.add(candidate);
      }
      aux.remove(aux.size() - 1);
    }

    return candidates;
  }

  /**
   * This method transform old-style topic definition to new one, i.e, transform Redis topic
   * expressions like /data/* or /data/mockProvider* to /data or /data/mockProvider
   *
   * @param topic
   * @return
   */
  public static String formatTopicExpression(final String topic) {
    String newTopic = topic;

    while (newTopic.endsWith(SentiloConstants.CHANNEL_PATTERN_SUFFIX) || newTopic.endsWith(SentiloConstants.SLASH)) {
      newTopic = newTopic.substring(0, newTopic.length() - 1);
    }

    return newTopic;
  }

  public static String marshal(final Object message) {
    return converter.marshal(message);
  }

  public static String getStreamName(final EventType eventType) {
    return getStreamName(eventType.name());
  }

  public static String getStreamName(final String streamKey) {
    return SentiloConstants.STREAM + SentiloConstants.REDIS_KEY_TOKEN + streamKey.toLowerCase();
  }

}
