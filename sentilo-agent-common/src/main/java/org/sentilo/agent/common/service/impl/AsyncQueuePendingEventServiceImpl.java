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
package org.sentilo.agent.common.service.impl;

import java.util.List;

import org.sentilo.agent.common.context.config.LoadGenericAsyncQueuePendingEventService;
import org.sentilo.agent.common.service.AsyncQueuePendingEventService;
import org.sentilo.agent.common.utils.AgentMessagingUtils;
import org.sentilo.common.domain.EventMessage;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Conditional;
import org.springframework.stereotype.Component;

/**
 * Implementation of {@link #AsyncQueuePendingEventServiceImpl()} that reads pending messages from
 * the PEL queue that each agent holds in Redis. Pending messages are stored as JSON strings in this
 * queue.
 */
@Component
@Conditional(LoadGenericAsyncQueuePendingEventService.class)
public class AsyncQueuePendingEventServiceImpl extends AbstractAsyncPendingEventServiceImpl implements AsyncQueuePendingEventService {

  private static final Logger LOGGER = LoggerFactory.getLogger(AsyncQueuePendingEventServiceImpl.class);

  private String queueName;

  @Override
  public int readAndProcessPendingEvents(final int count) {
    LOGGER.info("Reading pending events from queue: {}", getQueueName());
    int pendingEventsProcessed = 0;
    try {
      final List<String> pendingEvents = redisTemplate.opsForSet().pop(getQueueName(), count);
      processPendingEvents(pendingEvents);
      pendingEventsProcessed = pendingEvents.size();
    } catch (final Exception e) {
      LOGGER.warn("An error has been raised while reading PEL from queue [{}]", getQueueName(), e);
    }

    return pendingEventsProcessed;
  }

  protected void processPendingEvents(final List<String> pendingEvents) {
    pendingEvents.stream().forEach(event -> process(unmarshalEvent(event)));
  }

  protected void process(final EventMessage event) {
    messageListener.doWithMessage(event);
  }

  private String getQueueName() {
    if (queueName == null) {
      queueName = AgentMessagingUtils.getPendingEventQueueName();
    }

    return queueName;
  }

}
