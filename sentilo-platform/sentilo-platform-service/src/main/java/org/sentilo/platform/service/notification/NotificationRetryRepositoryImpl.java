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
package org.sentilo.platform.service.notification;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Repository;

@Repository
public class NotificationRetryRepositoryImpl implements NotificationRetryRepository {

  private static final Logger LOGGER = LoggerFactory.getLogger(NotificationRetryRepositoryImpl.class);
  private static final String SORTED_SET_KEY = "push:pending:events";
  private static final long MAX_DELAY_RETRY = 12 * 60 * 60 * 1000;

  @Autowired
  private StringRedisTemplate redisTemplate;

  private NotificationRetryEventConverter eventParser = new NotificationRetryEventConverter();

  @Override
  public void save(final NotificationRetryEvent notification) {
    // To save an event to further retry, first its delay time must be computed
    // This delay time depends on two factors: the retry count (N) and the delay configured between
    // retries.
    // Delay = 2^N * (delayRetry) or 12 hours
    // In this way, every retry increases exponentially the delay between retries, up to a maximum
    // of 12 hours between retries.
    // It prevents that the remote server might get so overwhelmed when it comes back up
    // that it might go down again.
    final long currentTimestamp = System.currentTimeMillis();
    final long retryDelayMinutes = notification.getNotificationDeliveryContext().getNotificationParams().getRetryDelay();
    final int retryCount = notification.getRetryCount();
    final double newDelayMillis = Math.pow(2, retryCount) * (retryDelayMinutes * 60 * 1000);
    final double score = newDelayMillis < MAX_DELAY_RETRY ? newDelayMillis + currentTimestamp : MAX_DELAY_RETRY + currentTimestamp;

    redisTemplate.opsForZSet().add(SORTED_SET_KEY, eventParser.marshall(notification), score);
    LOGGER.debug("Message {} saved to be processed after {} millisecods.", notification.getMessage(), score);
  }

  @Override
  public void save(final List<NotificationRetryEvent> notifications) {
    // The difference between this method and the previous is that it save notifications that don't
    // have been processed by the retry job (for example due to an error while running job). These
    // events are pulling again with a new score of 1 minute

    final double newScore = System.currentTimeMillis() + 60 * 1000;

    for (final NotificationRetryEvent notification : notifications) {
      redisTemplate.opsForZSet().add(SORTED_SET_KEY, eventParser.marshall(notification), newScore);
      LOGGER.debug("Message {} saved to be processed after {} millisecods.", notification.getMessage(), newScore);
    }
  }

  @Override
  public List<NotificationRetryEvent> getEventsToRetry(final long currentTimestamp, final long limit) {
    LOGGER.debug("Querying notification events to retry with a delay time between 0 and {} and limited to {} events", currentTimestamp, limit);
    final Set<String> events = redisTemplate.opsForZSet().rangeByScore(SORTED_SET_KEY, 0, currentTimestamp, 0, limit);
    LOGGER.debug("Found {} notification events which should be redelivered now ", events.size());

    final List<NotificationRetryEvent> notificationEvents = new ArrayList<NotificationRetryEvent>();
    for (final String notifEventJson : events) {
      notificationEvents.add(eventParser.unmarshall(notifEventJson));
    }

    return notificationEvents;
  }

  public void remove(final long limit) {
    // Method getEventsToRetry always returns the firsts N elements with score <= currentTimestamp
    // This delete method remove the same elements from the set, i.e, remove the first N elements
    // (elements always are sorted by timestamp).

    final long elementsRemoved = redisTemplate.opsForZSet().removeRange(SORTED_SET_KEY, 0, limit - 1);
    LOGGER.debug("Removed {} events from the sorted set", elementsRemoved);
  }

  @Override
  public Long getTotalEventsToRetry(final long currentTimestamp) {
    LOGGER.debug("Querying total notification events to retry with a delay time between 0 and {}", currentTimestamp);
    final Long total = redisTemplate.opsForZSet().count(SORTED_SET_KEY, 0, currentTimestamp);
    LOGGER.debug("Found {} notification events which should be redelivered now ", total);

    return total;
  }

}
