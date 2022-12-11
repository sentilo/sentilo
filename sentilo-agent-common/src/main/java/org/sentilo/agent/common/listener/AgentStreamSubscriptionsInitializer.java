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

import java.time.Duration;
import java.util.ArrayList;
import java.util.List;

import org.sentilo.agent.common.service.AsyncStreamPendingEventService;
import org.sentilo.agent.common.utils.AgentMessagingUtils;
import org.sentilo.agent.common.utils.SentiloAgentConstants;
import org.sentilo.common.utils.MessagingUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.dao.QueryTimeoutException;
import org.springframework.data.redis.RedisSystemException;
import org.springframework.data.redis.connection.stream.Consumer;
import org.springframework.data.redis.connection.stream.MapRecord;
import org.springframework.data.redis.connection.stream.ReadOffset;
import org.springframework.data.redis.connection.stream.StreamOffset;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.data.redis.stream.StreamMessageListenerContainer;
import org.springframework.data.redis.stream.StreamMessageListenerContainer.StreamMessageListenerContainerOptions;
import org.springframework.data.redis.stream.StreamMessageListenerContainer.StreamReadRequest;
import org.springframework.data.redis.stream.Subscription;

public class AgentStreamSubscriptionsInitializer extends AgentSubscriptionsInitializer {

  private static final Logger LOGGER = LoggerFactory.getLogger(AgentStreamSubscriptionsInitializer.class);

  private static final int STREAM_READ_COUNT = 100;

  @Autowired(required = false)
  @Qualifier("asyncStreamPendingEventService")
  private AsyncStreamPendingEventService streamPendingEventService;

  @Autowired
  private StringRedisTemplate redisTemplate;

  private StreamMessageListenerContainer<String, MapRecord<String, String, String>> streamContainer;
  private final List<String> activeStreamsSubs = new ArrayList<String>();

  @Override
  public void onApplicationEvent(final ContextRefreshedEvent event) {
    LOGGER.info("Begin process to register subscriptions ");

    // If pollTimeout > redisCommandTimeout then the agent never read+ack any message from stream
    // subscriptions, and message directly goes to consumer PEL.
    final String sRedisConnTimeout = sentiloConfigProperties.getProperty("sentilo.redis.connTimeout", "4000");
    final long pollTimeout = Long.parseLong(sRedisConnTimeout) - 1000;

    final StreamMessageListenerContainerOptions<String, MapRecord<String, String, String>> containerOptions =
        StreamMessageListenerContainerOptions.builder().batchSize(STREAM_READ_COUNT).pollTimeout(Duration.ofMillis(pollTimeout)).build();

    streamContainer = StreamMessageListenerContainer.create(redisTemplate.getRequiredConnectionFactory(), containerOptions);
    streamContainer.start();

    subscribe();

    LOGGER.info("End of process");
  }

  @Override
  protected void registerSubscription(final SentiloAgentMessageListener streamListener, final String sTopic) {
    final String streamName = getStreamName(sTopic);

    if (!activeStreamsSubs.contains(streamName)) {
      final String groupName = AgentMessagingUtils.getGroupName();
      createGroupIfNeedBe(streamName, groupName);

      // @formatter:off
      final StreamReadRequest<String> srr = StreamReadRequest.builder(StreamOffset.create(streamName, ReadOffset.lastConsumed()))
          .consumer(Consumer.from(groupName, AgentMessagingUtils.getConsumerName()))
          .cancelOnError(e -> false)
          .autoAcknowledge(false)
          .errorHandler(e -> processStreamReadException(e, streamName))
          .build();

      final Subscription subs = streamContainer.register(srr, msg -> {
          streamListener.onMessage(msg);
          redisTemplate.opsForStream().acknowledge(groupName, msg);
        });
      // @formatter:on

      LOGGER.debug("Defined subscription to stream {}. Is active? {}", streamName, subs.isActive());

      activeStreamsSubs.add(streamName);
      if (streamPendingEventService != null) {
        streamPendingEventService.addPendingEventStream(streamName);
      }
    }

    LOGGER.debug("Subscription to stream {} registered successfully", streamName);
  }

  /**
   * Custom error handler for exceptions generated while reading messages from Stream
   * <code>streamName</code>. This implementation ignores exceptions of type
   * {@link org.springframework.dao.QueryTimeoutException}, which are raised periodically every
   * {@value sentilo.redis.connTimeout} milliseconds if there isn't activity, and log any other
   *
   * @param t
   * @param streamName
   */
  private void processStreamReadException(final Throwable t, final String streamName) {
    if (!(t instanceof QueryTimeoutException)) {
      LOGGER.warn("Error while reading from stream {} : {}", streamName, t);
    }
  }

  private void createGroupIfNeedBe(final String streamName, final String groupName) {
    try {
      redisTemplate.opsForStream().createGroup(streamName, ReadOffset.from("0"), groupName);
      LOGGER.info("Created group {} of consumers on stream {}", groupName, streamName);
    } catch (final RedisSystemException e) {
      LOGGER.debug("Group {} already exist on stream {}", groupName, streamName);
    }
  }

  private String getStreamName(final String sTopic) {
    final String streamKey = sTopic.split(SentiloAgentConstants.TOPIC_TOKEN)[1];
    return MessagingUtils.getStreamName(streamKey);
  }

}
