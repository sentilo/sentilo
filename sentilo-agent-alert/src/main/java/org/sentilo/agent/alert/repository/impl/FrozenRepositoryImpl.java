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
package org.sentilo.agent.alert.repository.impl;

import java.io.IOException;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Set;

import org.sentilo.agent.alert.domain.InternalAlert;
import org.sentilo.agent.alert.repository.FrozenRepository;
import org.sentilo.agent.alert.utils.AlertUtils;
import org.sentilo.agent.alert.utils.Constants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.core.Cursor;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.ScanOptions;
import org.springframework.data.redis.core.ZSetOperations.TypedTuple;
import org.springframework.stereotype.Repository;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

@Repository
public class FrozenRepositoryImpl implements FrozenRepository {

  private static final Logger LOGGER = LoggerFactory.getLogger(FrozenRepositoryImpl.class);
  private static final String SORTED_SET_KEY = "alerts:frozen:sorted";
  private static final String LAST_SYNC_KEY = "agent:alert:lastSync";

  @Autowired
  private RedisTemplate<String, String> redisTemplate;

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.agent.alert.repository.FrozenRepository#checkFrozenAlerts()
   */
  public List<InternalAlert> checkFrozenAlerts() {
    final long currentTimestamp = System.currentTimeMillis();
    LOGGER.debug("Querying frozen alerts with a timeout between 0 and {}", currentTimestamp);
    final Set<String> members = redisTemplate.opsForZSet().rangeByScore(SORTED_SET_KEY, 0, currentTimestamp);
    LOGGER.debug("Found {} alerts which must publish a frozen alarm", members.size());
    final List<InternalAlert> alerts = new ArrayList<InternalAlert>();
    for (final String member : members) {
      final String[] tokens = member.split(Constants.REDIS_MEMBER_TOKEN);
      final InternalAlert alert = new InternalAlert(tokens[2]);
      alert.setProviderId(tokens[0]);
      alert.setSensorId(tokens[1]);
      alerts.add(alert);
    }

    return alerts;
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.agent.alert.repository.FrozenRepository#updateFrozenTimeouts(java.util.Collection)
   */
  public void updateFrozenTimeouts(final Collection<InternalAlert> frozenAlerts) {
    for (final InternalAlert frozenAlert : frozenAlerts) {
      updateFrozenTimeout(frozenAlert);
    }
  }

  private void updateFrozenTimeout(final InternalAlert frozenAlert) {
    final long currentTimestamp = System.currentTimeMillis();
    try {
      final String value = AlertUtils.buildFrozenAlertMember(frozenAlert);
      LOGGER.debug("Updating frozen timeout for member {}", value);
      final double maxFrozenMinutes = AlertUtils.transformNumber(frozenAlert.getExpression()).doubleValue();
      final double score = currentTimestamp + maxFrozenMinutes * 60 * 1000;
      redisTemplate.opsForZSet().add(SORTED_SET_KEY, value, score);
      LOGGER.debug("Frozen timeout for member {} updated to {}", value, score);
    } catch (final ParseException e) {
      LOGGER.warn("Cannot update frozen timeout for alert {}", frozenAlert.getId(), e);
    }
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.agent.alert.repository.FrozenRepository#synchronizeAlerts(java.util.Collection)
   */
  public void synchronizeAlerts(final Collection<InternalAlert> frozenAlerts) {
    LOGGER.debug("Synchronizing  frozen alerts with the repository");

    // First step is to update the frozen alerts stored on the repository
    updateAlertsModified(frozenAlerts);

    // Second step is to remove possible deprecated frozenAlerts from the repository.
    removeDeprecatedAlerts(frozenAlerts);

    // Finally, the latsSyncKey is updated
    updateLastSyncValue();
  }

  private void updateAlertsModified(final Collection<InternalAlert> frozenAlerts) {
    final String lastSyncValue = redisTemplate.opsForValue().get(LAST_SYNC_KEY);
    final long lastSyncTimestamp = StringUtils.hasText(lastSyncValue) ? Long.parseLong(lastSyncValue) : 0L;
    for (final InternalAlert frozenAlert : frozenAlerts) {
      // Frozen alert must be updated into SORTED_SET_KEY if and only if has been updated/created
      // after the last synchronization
      final Date updatedAt = frozenAlert.getUpdatedAt();
      if (updatedAt != null && updatedAt.getTime() >= lastSyncTimestamp) {
        updateFrozenTimeout(frozenAlert);
      }
    }
  }

  private void removeDeprecatedAlerts(final Collection<InternalAlert> frozenAlerts) {
    final Cursor<TypedTuple<String>> scanCursor = redisTemplate.opsForZSet().scan(SORTED_SET_KEY, ScanOptions.NONE);
    final List<String> membersToRemove = new ArrayList<String>();

    while (scanCursor.hasNext()) {
      final TypedTuple<String> member = scanCursor.next();
      // member verifies pattern providerId#sensorId#alertId
      final String[] tokens = member.getValue().split(Constants.REDIS_MEMBER_TOKEN);
      if (!frozenAlerts.contains(new InternalAlert(tokens[2]))) {
        membersToRemove.add(member.getValue());
      }
    }

    // Close cursor to evict connections leak
    closeCursor(scanCursor);

    LOGGER.debug("Found {} members on Redis that need to be removed because them no longer exist on Catalog", membersToRemove.size());
    if (!CollectionUtils.isEmpty(membersToRemove)) {
      // Remove in blocks of 20 members maximum to not penalize Redis
      final int blockSize = 20;
      int fromIndex = 0;
      int toIndex = membersToRemove.size() < blockSize ? membersToRemove.size() : blockSize;
      do {
        final List<String> subListToRemove = membersToRemove.subList(fromIndex, toIndex);
        redisTemplate.opsForZSet().remove(SORTED_SET_KEY, subListToRemove.toArray(new Object[subListToRemove.size()]));

        fromIndex = toIndex;
        toIndex = toIndex + blockSize < membersToRemove.size() ? toIndex + blockSize : membersToRemove.size();
      } while (fromIndex < membersToRemove.size());

    }
  }

  private void updateLastSyncValue() {
    final long newLastSyncValue = System.currentTimeMillis();
    redisTemplate.opsForValue().set(LAST_SYNC_KEY, Long.toString(newLastSyncValue));
    LOGGER.debug("New lastSync value updated to {}", newLastSyncValue);
  }

  private <E> void closeCursor(final Cursor<E> cursor) {
    try {
      if (!cursor.isClosed()) {
        cursor.close();
      }
    } catch (final IOException ioe) {
      // nothing to do ...
    }
  }

}
