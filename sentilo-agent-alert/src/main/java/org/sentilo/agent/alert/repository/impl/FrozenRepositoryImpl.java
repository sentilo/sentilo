/*
 * Sentilo
 * 
 * Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de Barcelona.
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

  private final Logger logger = LoggerFactory.getLogger(FrozenRepositoryImpl.class);
  private final String sortedSetKey = "alerts:frozen:sorted";
  private final String lastSyncKey = "agent:alert:lastSync";

  @Autowired
  private RedisTemplate<String, String> redisTemplate;

  /*
   * (non-Javadoc)
   * 
   * @see org.sentilo.agent.alert.repository.FrozenRepository#checkFrozenAlerts()
   */
  public List<InternalAlert> checkFrozenAlerts() {
    final long currentTimestamp = System.currentTimeMillis();
    logger.debug("Quering frozen alerts with a timeout between 0 and {}", currentTimestamp);
    final Set<String> members = redisTemplate.opsForZSet().rangeByScore(sortedSetKey, 0, currentTimestamp);
    logger.debug("Found {} alerts which must publish a frozen alarm", members.size());
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
      logger.debug("Updating frozen timeout for member {}", value);
      final double maxFrozenMinutes = AlertUtils.transformNumber(frozenAlert.getExpression()).doubleValue();
      final double score = currentTimestamp + (maxFrozenMinutes * 60 * 1000);
      redisTemplate.opsForZSet().add(sortedSetKey, value, score);
      logger.debug("Frozen timeout for member {} updated to {}", value, score);
    } catch (final ParseException e) {
      logger.warn("Cannot update frozen timeout for alert {}", frozenAlert.getId(), e);
    }
  }

  /*
   * (non-Javadoc)
   * 
   * @see
   * org.sentilo.agent.alert.repository.FrozenRepository#synchronizeAlerts(java.util.Collection)
   */
  public void synchronizeAlerts(final Collection<InternalAlert> frozenAlerts) {
    logger.debug("Synchronizing  frozen alerts with the repository");

    // First step is to update the frozen alerts stored on the repository
    updateAlertsModified(frozenAlerts);

    // Second step is to remove possible deprecated frozenAlerts from the repository.
    removeDeprecatedAlerts(frozenAlerts);

    // Finally, the latsSyncKey is updated
    updateLastSyncValue();
  }

  private void updateAlertsModified(final Collection<InternalAlert> frozenAlerts) {
    final String lastSyncValue = redisTemplate.opsForValue().get(lastSyncKey);
    final long lastSyncTimestamp = (StringUtils.hasText(lastSyncValue) ? Long.parseLong(lastSyncValue) : 0l);
    for (final InternalAlert frozenAlert : frozenAlerts) {
      // Frozen alert must be updated into sortedSetKey if and only if has been updated/created
      // after the last synchronization
      final Date updateAt = frozenAlert.getUpdateAt();
      if (updateAt != null && updateAt.getTime() >= lastSyncTimestamp) {
        updateFrozenTimeout(frozenAlert);
      }
    }
  }

  private void removeDeprecatedAlerts(final Collection<InternalAlert> frozenAlerts) {
    final Cursor<TypedTuple<String>> scanCursor = redisTemplate.opsForZSet().scan(sortedSetKey, ScanOptions.NONE);
    final List<String> membersToRemove = new ArrayList<String>();
    while (scanCursor.hasNext()) {
      final TypedTuple<String> member = scanCursor.next();
      // member verifies pattern providerId:sensorId:alertId
      final String[] tokens = member.getValue().split(Constants.REDIS_MEMBER_TOKEN);
      if (!frozenAlerts.contains(new InternalAlert(tokens[2]))) {
        membersToRemove.add(member.getValue());
      }
    }

    logger.debug("Found {} members on Redis that need to be remove because them no longer exist on Catalog", membersToRemove.size());
    if (!CollectionUtils.isEmpty(membersToRemove)) {
      // Remove in blocks of 20 members maximum to not penalize Redis
      final int blockSize = 20;
      int fromIndex = 0;
      int toIndex = (membersToRemove.size() < blockSize ? membersToRemove.size() : blockSize);
      do {
        final List<String> subListToRemove = membersToRemove.subList(fromIndex, toIndex);
        redisTemplate.opsForZSet().remove(sortedSetKey, subListToRemove.toArray(new Object[subListToRemove.size()]));

        fromIndex = toIndex;
        toIndex = ((toIndex + blockSize) < membersToRemove.size() ? toIndex + blockSize : membersToRemove.size());
      } while (fromIndex < membersToRemove.size());

    }
  }

  private void updateLastSyncValue() {
    final long newLastSyncValue = System.currentTimeMillis();
    redisTemplate.opsForValue().set(lastSyncKey, Long.toString(newLastSyncValue));
    logger.debug("New lastSync value updated to {}", newLastSyncValue);
  }

}
