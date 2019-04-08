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
package org.sentilo.agent.common.scheduler;

import org.sentilo.agent.common.service.AsyncPendingEventService;
import org.sentilo.agent.common.utils.Utils;
import org.sentilo.common.converter.DefaultStringMessageConverter;
import org.sentilo.common.converter.StringMessageConverter;
import org.sentilo.common.domain.EventMessage;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

/**
 * Scheduled job that runs every 5 minutes and read pending events from Redis and send them to the
 * {@link AsyncPendingEventService} configured.
 *
 * In every execution reads a maximum of 200 events to no penalize the performance of the underlying
 * agent (the processing of pending events must be done in a second plane, so we fix a ratio of 20
 * events/minute)
 */
@Component
public class PendingEventsProcessJob {

  private static final int MAX_SIZE = 200;
  private static final String NIL = "nil";

  @Autowired
  private StringRedisTemplate redisTemplate;

  @Autowired(required = false)
  private AsyncPendingEventService pendingEventService;

  private String queueName;

  private final StringMessageConverter converter = new DefaultStringMessageConverter();

  @Scheduled(initialDelay = 120000, fixedDelay = 300000)
  public void run() {
    // pending events are read and processed one by one until a maximum of MAX_SIZE readings is
    // reached.
    int numReadings = 0;
    boolean emptyQueue = false;

    while (!emptyQueue && numReadings < MAX_SIZE) {
      final String jsonEvent = redisTemplate.opsForSet().pop(getQueueName());
      numReadings++;

      if (!StringUtils.hasText(jsonEvent) || jsonEvent.equals(NIL)) {
        emptyQueue = true;
      } else {
        final EventMessage eventMessage = (EventMessage) converter.unmarshal(jsonEvent, EventMessage.class);
        pendingEventService.process(eventMessage);
      }
    }
  }

  private String getQueueName() {
    if (queueName == null) {
      queueName = Utils.getPendingEventQueueName();
    }

    return queueName;
  }
}
