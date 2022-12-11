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
package org.sentilo.agent.common.listener;

import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.sentilo.agent.common.metrics.AgentMetricsCounter;
import org.sentilo.common.converter.DefaultStringMessageConverter;
import org.sentilo.common.converter.StringMessageConverter;
import org.sentilo.common.domain.EventMessage;
import org.sentilo.common.exception.MessageNotWritableException;
import org.sentilo.common.utils.MessagingUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.connection.Message;
import org.springframework.data.redis.connection.stream.MapRecord;
import org.springframework.data.redis.serializer.RedisSerializer;
import org.springframework.data.redis.serializer.StringRedisSerializer;
import org.springframework.util.Assert;
import org.springframework.util.CollectionUtils;

public abstract class AbstractMessageListenerImpl implements SentiloAgentMessageListener {

  private static final Logger LOGGER = LoggerFactory.getLogger(AbstractMessageListenerImpl.class);

  @Autowired
  private AgentMetricsCounter metricsCounters;

  private final String name;
  private final RedisSerializer<String> serializer = new StringRedisSerializer();
  private final StringMessageConverter eventConverter = new DefaultStringMessageConverter();
  /**
   * Allows filter input messages by its topic's value
   */
  private final Set<String> topicsOfInterest = new HashSet<String>();

  public AbstractMessageListenerImpl(final String name) {
    super();
    Assert.notNull(name, "name must not be NULL");
    this.name = name;
  }

  @Override
  public void onMessage(final Message message, final byte[] pattern) {
    final String info = getInfo(message);
    final String channel = getChannel(message);

    LOGGER.debug("{} -->  Received message on channel {}", name, channel);
    LOGGER.debug("{} -->  Message content {}", name, info);

    try {
      process(info);
    } catch (final MessageNotWritableException mnwe) {
      LOGGER.error("Error unmarshalling message: {}. ", info, mnwe);
    }
  }

  @Override
  public void onMessage(final MapRecord<String, String, String> message) {
    // If an error is raised while processing message, it is propagated to parent to mark it
    // as pending (business exceptions are controlled internally by each agent and messages are
    // moved to Agent pending event list no to Stream PEL).

    // For example, if a message arrive to an agent and it still is not ready to process messages,
    // then alert throws a no-controlled-exception and message is moved to Stream PEL. By contrast,
    // if an error is thrown while sending message to final repository, then error isn't propagated
    // any exception to this point and message is moved to Agent PEL.
    message.forEach(entry -> process(entry.getValue(), true));
  }

  protected void process(final EventMessage eventMessage) {
    metricsCounters.incrementInputEvents(1);
    doWithMessage(eventMessage);
  }

  /**
   * Every implementation of this class must override this method to implements business logic
   *
   * @param eventMessage
   */
  @Override
  public abstract void doWithMessage(final EventMessage eventMessage);

  public String getName() {
    return name;
  }

  public void setMetricsCounters(final AgentMetricsCounter metricsCounters) {
    this.metricsCounters = metricsCounters;
  }

  @Override
  public void addTopicOfInterest(final String topic) {
    topicsOfInterest.add(MessagingUtils.formatTopicExpression(topic));
  }

  protected void process(final String message) {
    // Unlike stream subscriptions, where clients read all events from streams, the
    // publish/subscribe mechanism ensures that clients only get events to which they are
    // subscribed so events don't need to be filtered.
    process(message, false);
  }

  protected void process(final String message, final boolean filter) {
    LOGGER.debug("{} -->  Message content {}", name, message);
    // The received message corresponds to a JSON representation of an object of type
    // EventMessage.
    final EventMessage eventMessage = unmarshal(message);
    if (!filter || (filter && isEventOfInterest(eventMessage))) {
      process(eventMessage);
    }
  }

  protected boolean isEventOfInterest(final EventMessage eventMessage) {
    return CollectionUtils.isEmpty(topicsOfInterest) ? true : processEvent(eventMessage);
  }

  /**
   * Returns if given <code>EventMessage</code> match topics of interests for this listener.
   *
   * @param eventMessage
   * @return
   */
  protected boolean processEvent(final EventMessage eventMessage) {
    // Events should be processed only if topicsOfInterest has any match with the candidates list.
    final List<String> candidates = MessagingUtils.buildCandidates(eventMessage.getTopic());
    return !Collections.disjoint(topicsOfInterest, candidates);
  }

  protected String getInfo(final Message message) {
    return serializer.deserialize(message.getBody());
  }

  protected String getChannel(final Message message) {
    return serializer.deserialize(message.getChannel());
  }

  protected EventMessage unmarshal(final String message) {
    return eventConverter.unmarshal(message, EventMessage.class);
  }

}
