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
package org.sentilo.agent.common.repository.impl;

import java.util.List;

import org.sentilo.agent.common.repository.PendingEventsRepository;
import org.sentilo.agent.common.utils.Utils;
import org.sentilo.common.converter.DefaultStringMessageConverter;
import org.sentilo.common.converter.StringMessageConverter;
import org.sentilo.common.domain.EventMessage;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Repository;
import org.springframework.util.CollectionUtils;

import com.google.common.collect.Lists;

@Repository
public class PendingEventsRepositoryImpl implements PendingEventsRepository {

  private static final Logger LOGGER = LoggerFactory.getLogger(PendingEventsRepositoryImpl.class);

  private static final int SUBLIST_SIZE = 10;

  @Autowired
  private StringRedisTemplate redisTemplate;

  private final StringMessageConverter converter = new DefaultStringMessageConverter();

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.agent.common.repository.PendingEventsRepository#storePendingEvents(java.util.List)
   */
  public void storePendingEvents(final List<EventMessage> pendingEvents) {
    if (!CollectionUtils.isEmpty(pendingEvents)) {
      final String queueKey = Utils.getPendingEventQueueName();
      LOGGER.debug("Adding {} events to queue {}", pendingEvents.size(), queueKey);
      // Add events to Redis in groups of N elements to no generate overload and because the method
      // has a time complexity O(N)
      final List<List<EventMessage>> sublists = Lists.partition(pendingEvents, SUBLIST_SIZE);
      for (final List<EventMessage> sublist : sublists) {
        redisTemplate.opsForSet().add(queueKey, buildMembersValues(sublist));
      }
    }
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.sentilo.agent.common.repository.PendingEventsRepository#pendingQueueSize()
   */
  public long pendingQueueSize() {
    final String queueKey = Utils.getPendingEventQueueName();
    return redisTemplate.opsForSet().size(queueKey);
  }

  private String[] buildMembersValues(final List<? extends EventMessage> pendingEvents) {
    final String[] members = new String[pendingEvents.size()];
    int i = 0;

    for (final EventMessage eventMessage : pendingEvents) {
      members[i] = converter.marshal(eventMessage);
      i++;
    }

    return members;
  }
}
