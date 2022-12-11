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
package org.sentilo.agent.common.service.impl;

import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

import org.sentilo.agent.common.context.config.LoadGenericAsyncStreamPendingEventService;
import org.sentilo.agent.common.service.AsyncStreamPendingEventService;
import org.sentilo.agent.common.utils.AgentMessagingUtils;
import org.sentilo.common.domain.EventMessage;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Conditional;
import org.springframework.data.redis.connection.stream.Consumer;
import org.springframework.data.redis.connection.stream.MapRecord;
import org.springframework.data.redis.connection.stream.ReadOffset;
import org.springframework.data.redis.connection.stream.StreamOffset;
import org.springframework.data.redis.connection.stream.StreamReadOptions;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

/**
 * Read and process pending messages from each stream to which agent is subscribed.
 */
@Component
@Conditional(LoadGenericAsyncStreamPendingEventService.class)
public class AsyncStreamPendingEventServiceImpl extends AbstractAsyncPendingEventServiceImpl implements AsyncStreamPendingEventService {

  private static final Logger LOGGER = LoggerFactory.getLogger(AsyncStreamPendingEventServiceImpl.class);

  private final Set<String> streamsToRead = new HashSet<String>();

  /**
   * Allows register a stream from which read pending events.
   */
  @Override
  public void addPendingEventStream(final String streamName) {
    streamsToRead.add(streamName);
  }

  @Override
  public int readAndProcessPendingEvents(final int count) {
    final AtomicInteger pendingEventsProcessed = new AtomicInteger(0);
    // For each stream where agent is subscribed its PEL must be read to process pending messages
    // @formatter:off
    final String groupName = AgentMessagingUtils.getGroupName();
    if(!CollectionUtils.isEmpty(streamsToRead)) {
      streamsToRead.forEach(streamName ->{
          pendingEventsProcessed.addAndGet(readAndProcessStreamPendingEvents(streamName, groupName, count));
        });
    }
    // @formatter:on
    return pendingEventsProcessed.get();
  }

  @SuppressWarnings("unchecked")
  protected int readAndProcessStreamPendingEvents(final String streamName, final String groupName, final int count) {
    final AtomicInteger pendingEventsProcessed = new AtomicInteger(0);
    try {
      LOGGER.debug("Reading pending events from stream: {}", streamName);
      final StreamOffset<String> sos = StreamOffset.create(streamName, ReadOffset.from("0"));
      final List<MapRecord<String, Object, Object>> messages = redisTemplate.opsForStream()
          .read(Consumer.from(groupName, AgentMessagingUtils.getConsumerName()), StreamReadOptions.empty().count(count), sos);

      pendingEventsProcessed.addAndGet(messages.size());
      // @formatter:off
      messages.forEach(entry -> {
          final Map<Object, Object> value = entry.getValue();
          value.forEach((k, v) -> processStreamMessage((String)v));
          redisTemplate.opsForStream().acknowledge(groupName, entry);
        });
      // @formatter:on
    } catch (final Exception e) {
      LOGGER.warn("An error has been raised while reading PEL from stream {} ", streamName, e);
    }

    return pendingEventsProcessed.get();
  }

  protected void processStreamMessage(final String message) {
    process(unmarshalEvent(message));
  }

  protected void process(final EventMessage event) {
    messageListener.doWithMessage(event);
  }

}
