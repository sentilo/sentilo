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
package org.sentilo.platform.service.dao;

import java.util.Arrays;
import java.util.Optional;

import org.sentilo.common.cache.LRUCache;
import org.sentilo.common.cache.impl.LRUCacheImpl;
import org.sentilo.common.utils.SentiloConstants;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.util.Pair;
import org.springframework.stereotype.Component;

/**
 * Caches entities ids (such as providers, sensors, alerts, ...) to avoid having to read them
 * constantly from Redis
 */
@Component
public class SentiloSequenceUtils {

  private static final String PID_SEQ = "global:pid";
  private static final String SID_SEQ = "global:sid";
  private static final String SDID_SEQ = "global:sdid";
  private static final String SOID_SEQ = "global:soid";
  private static final String AID_SEQ = "global:aid";
  private static final String AMID_SEQ = "global:amid";

  public static final String NIL = "nil";

  @Autowired
  private SentiloRedisTemplate sentiloRedisTemplate;

  private final SentiloKeysBuilder keysBuilder = new SentiloKeysBuilder();

  private final LRUCache<String, Long> pids = new LRUCacheImpl<String, Long>(10000, 720);
  private final LRUCache<String, Long> sids = new LRUCacheImpl<String, Long>(20000, 720);
  private final LRUCache<String, Long> aids = new LRUCacheImpl<String, Long>(10000, 720);

  public void clearAll() {
    pids.clear();
    sids.clear();
    aids.clear();
  }

  public void clearSensors() {
    sids.clear();
  }

  public void clearAlerts() {
    aids.clear();
  }

  public Optional<Long> getLocalPid(final String providerId) {
    final String cacheKey = providerId;
    return Optional.ofNullable(pids.get(cacheKey));
  }

  public Optional<Long> getPid(final String providerId) {
    final String cacheKey = providerId;
    final String redisKey = keysBuilder.getReverseProviderKey(providerId);
    return getId(pids, cacheKey, redisKey);
  }

  /**
   * Generates a new pid if not exist in Redis an identifier associated with provider
   * <code>providerId</code>. Otherwise returns its current pid.
   *
   * @param providerId
   * @return Pair of provider pid sequence and a boolean that indicates if this pid is new .
   */
  public Pair<Long, Boolean> getNewPidIfAbsent(final String providerId) {
    final String cacheKey = providerId;
    final Optional<Long> currentPid = getPid(providerId);
    return currentPid.isPresent() ? Pair.of(currentPid.get(), false) : Pair.of(getNewId(pids, cacheKey, PID_SEQ), true);
  }

  public void removePid(final String providerId) {
    pids.remove(providerId);
  }

  public Optional<Long> getLocalSid(final String providerId, final String sensorId) {
    final String cacheKey = buildCacheKey(providerId, sensorId);
    return Optional.ofNullable(sids.get(cacheKey));
  }

  public Optional<Long> getSid(final String providerId, final String sensorId) {
    final String cacheKey = buildCacheKey(providerId, sensorId);
    final String redisKey = keysBuilder.getReverseSensorKey(providerId, sensorId);
    return getId(sids, cacheKey, redisKey);
  }

  /**
   * Generates a new sid if not exist in Redis an identifier associated with sensor
   * <code>(providerId,sensorId)</code>. Otherwise returns its current sid.
   *
   * @param providerId
   * @param sensorId
   * @return Pair of sensor sid sequence and a boolean that indicates if this sid is new .
   */
  public Pair<Long, Boolean> getNewSidIfAbsent(final String providerId, final String sensorId) {
    final String cacheKey = buildCacheKey(providerId, sensorId);
    final Optional<Long> currentSid = getSid(providerId, sensorId);
    return currentSid.isPresent() ? Pair.of(currentSid.get(), false) : Pair.of(getNewId(sids, cacheKey, SID_SEQ), true);
  }

  public void removeSid(final String providerId, final String sensorId) {
    final String cacheKey = buildCacheKey(providerId, sensorId);
    sids.remove(cacheKey);
  }

  public Optional<Long> getLocalAid(final String alertId) {
    final String cacheKey = alertId;
    return Optional.ofNullable(aids.get(cacheKey));
  }

  public Optional<Long> getAid(final String alertId) {
    final String cacheKey = alertId;
    final String redisKey = keysBuilder.getReverseAlertKey(alertId);
    return getId(aids, cacheKey, redisKey);
  }

  /**
   * Generates a new aid if not exist in Redis an identifier associated with alert
   * <code>alertId</code>. Otherwise returns its current aid.
   *
   * @param alertId
   * @return Pair of alert aid sequence and a boolean that indicates if this aid is new .
   */
  public Pair<Long, Boolean> getNewAidIfAbsent(final String alertId) {
    final String cacheKey = alertId;
    final Optional<Long> currentAid = getAid(alertId);
    return currentAid.isPresent() ? Pair.of(currentAid.get(), false) : Pair.of(getNewId(aids, cacheKey, AID_SEQ), true);
  }

  public void removeAid(final String alertId) {
    final String cacheKey = alertId;
    aids.remove(cacheKey);
  }

  public Long getNewSdid() {
    return getKeyNextValue(SDID_SEQ);
  }

  public Long getNewSoid() {
    return getKeyNextValue(SOID_SEQ);
  }

  public Long getNewAmid() {
    return getKeyNextValue(AMID_SEQ);
  }

  public Long getCurrentSdid() {
    return getCurrentValue(SDID_SEQ);
  }

  public Long getCurrentSoid() {
    return getCurrentValue(SOID_SEQ);
  }

  public Long getCurrentAmid() {
    return getCurrentValue(AMID_SEQ);
  }

//@formatter:off
  /**
   * To get id, the next steps must be followed:
   *
   * 1. If exists local cache entry with key==cacheKey,
   * then id==entry.value
   * 2. If not exist, then process get from Redis the value of the key
   * reverseKey (for example "provider":providerId:"pid").
   * 2.1 If this last key exists, then id==redisKey.value
   * 2.2 Else, Optional.empty is returned.
   *
   * @param cache Local cache when id is stored
   * @param cacheKey Key of the cache entry with value id
   * @param redisKey Reverse key of Redis which holds id value
   * @return Optional long with the value of id or empty
   */
//@formatter:on
  private Optional<Long> getId(final LRUCache<String, Long> cache, final String cacheKey, final String reverseKey) {
    if (!cache.contains(cacheKey)) {
      // final String reverseKey = buildRedisKey(tokens);
      final Long id = getReverseKeyValue(reverseKey);
      if (id != null) {
        cache.put(cacheKey, id);
      }
    }

    return Optional.ofNullable(cache.get(cacheKey));
  }

  private Long getNewId(final LRUCache<String, Long> cache, final String cacheKey, final String sequence) {
    if (!cache.contains(cacheKey)) {
      cache.put(cacheKey, getKeyNextValue(sequence));
    }

    return cache.get(cacheKey);
  }

  private String buildCacheKey(final String... members) {
    return String.join(SentiloConstants.SENTILO_INTERNAL_TOKEN, Arrays.asList(members));
  }

  private Long getReverseKeyValue(final String reverseKey) {
    final String value = sentiloRedisTemplate.get(reverseKey);
    return NIL.equals(value) || value == null ? null : Long.valueOf(value);
  }

  private Long getKeyNextValue(final String key) {
    return sentiloRedisTemplate.getKeyNextValue(key);
  }

  private Long getCurrentValue(final String key) {
    final String value = sentiloRedisTemplate.get(key);
    return NIL.equals(value) || value == null ? new Long(0) : Long.valueOf(value);
  }
}
