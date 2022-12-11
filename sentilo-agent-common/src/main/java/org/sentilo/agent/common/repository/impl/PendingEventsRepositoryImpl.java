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

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import org.sentilo.agent.common.repository.PendingEventsRepository;
import org.sentilo.agent.common.utils.AgentMessagingUtils;
import org.sentilo.common.converter.DefaultStringMessageConverter;
import org.sentilo.common.converter.StringMessageConverter;
import org.sentilo.common.domain.EventMessage;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataAccessException;
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
  @Override
  public List<EventMessage> storePendingEvents(final List<EventMessage> pendingEvents) {
    final List<EventMessage> failedList = new ArrayList<EventMessage>();

    if (!CollectionUtils.isEmpty(pendingEvents)) {
      final String queueKey = AgentMessagingUtils.getPendingEventQueueName();
      LOGGER.debug("Adding {} events to queue {}", pendingEvents.size(), queueKey);
      // Add events to Redis in groups of N elements to no generate overload and because the
      // method has a time complexity O(N)
      final List<List<EventMessage>> sublists = Lists.partition(pendingEvents, SUBLIST_SIZE);
      for (final List<EventMessage> sublist : sublists) {
        save(queueKey, sublist, failedList);
      }
    }

    return failedList;
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.agent.common.repository.PendingEventsRepository#pendingQueueSize()
   */
  @Override
  public long pendingQueueSize() {
    final String queueKey = AgentMessagingUtils.getPendingEventQueueName();
    return redisTemplate.opsForSet().size(queueKey);
  }

  private void save(final String queueKey, final List<EventMessage> sublist, final List<EventMessage> failedList) {
    try {
      final long membersAdded = redisTemplate.opsForSet().add(queueKey, buildMembersValues(sublist));
      if (membersAdded < sublist.size()) {
        // It means that some elements from sublist has not been persisted in Redis.
        // Clients must decide what to do with these elements.
        // @formatter:off
        final List<EventMessage> aux = sublist.stream()
            .filter(entry -> !redisTemplate.opsForSet().isMember(queueKey, converter.marshal(entry)))
            .collect(Collectors.toList());

        failedList.addAll(aux);
      }
    } catch (final DataAccessException dae) {
      failedList.addAll(sublist);
    }
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
