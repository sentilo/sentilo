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

import org.sentilo.common.cache.LRUCache;
import org.sentilo.common.cache.impl.LRUCacheImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Caches entities ids (such as providers, sensors, alerts, ...) to avoid having to read them
 * constantly from Redis
 */
@Component
public class SentiloSequenceUtils {

  private static final String PID_KEY = "global:pid";
  private static final String SID_KEY = "global:sid";
  private static final String SDID_KEY = "global:sdid";
  private static final String SOID_KEY = "global:soid";
  private static final String AID_KEY = "global:aid";
  private static final String AMID_KEY = "global:amid";
  public static final String NIL = "nil";

  @Autowired
  private SentiloRedisTemplate sentiloRedisTemplate;

  private final LRUCache<String, Long> pids = new LRUCacheImpl<String, Long>(10000, 720);
  private final LRUCache<String, Long> sids = new LRUCacheImpl<String, Long>(20000, 720);
  private final LRUCache<String, Long> aids = new LRUCacheImpl<String, Long>(10000, 720);

  public Long getPid(final String providerId) {

    // Al recuperar un valor de un secuencial, puede pasar:
    // 1. Exista en la cache, y por lo tanto no hace falta acceder a Redis
    // 2. No exista en la cache. Si no existe en la cache, se debe acceder a Redis para recuperar el
    // valor de la key "provider:"+providerId+":pid".
    // Puede ocurrir:
    // 2.1 Que exista, y por lo tanto se retorne el valor numerico.
    // 2.2 No exista, y por lo tanto se retorne NULL.
    final String internalKey = providerId;
    if (!pids.contains(internalKey)) {
      final String reverseKey = "provider:" + providerId + ":pid";
      final Long pid = getReverseKeyValue(reverseKey);
      if (pid != null) {
        pids.put(providerId, pid);
      }
    }

    return pids.get(internalKey);
  }

  public Long setPid(final String providerId) {
    final String internalKey = providerId;
    if (!pids.contains(internalKey)) {
      pids.put(providerId, getKeyNextValue(PID_KEY));
    }

    return pids.get(internalKey);
  }

  public void removePid(final String providerId) {
    pids.remove(providerId);
  }

  public Long getSid(final String providerId, final String sensorId) {
    final String internalKey = providerId + "#" + sensorId;
    if (!sids.contains(internalKey)) {
      final String reverseKey = "sensor:" + providerId + ":" + sensorId + ":sid";
      final Long sid = getReverseKeyValue(reverseKey);
      if (sid != null) {
        sids.put(internalKey, sid);
      }
    }

    return sids.get(internalKey);
  }

  public Long setSid(final String providerId, final String sensorId) {
    final String internalKey = providerId + "#" + sensorId;
    if (!sids.contains(internalKey)) {
      sids.put(internalKey, getKeyNextValue(SID_KEY));
    }

    return sids.get(internalKey);
  }

  public void removeSid(final String providerId, final String sensorId) {
    final String internalKey = providerId + "#" + sensorId;
    sids.remove(internalKey);
  }

  public Long getAid(final String alertId) {
    final String internalKey = alertId;
    if (!aids.contains(internalKey)) {
      final String reverseKey = "alert:" + alertId + ":aid";
      final Long aid = getReverseKeyValue(reverseKey);
      if (aid != null) {
        aids.put(alertId, aid);
      }
    }

    return aids.get(internalKey);
  }

  public Long setAid(final String alertId) {
    final String internalKey = alertId;
    if (!aids.contains(internalKey)) {
      aids.put(alertId, getKeyNextValue(AID_KEY));
    }

    return aids.get(internalKey);
  }

  public void removeAid(final String alertId) {
    final String internalKey = alertId;
    aids.remove(internalKey);
  }

  public Long getSdid() {
    return getKeyNextValue(SDID_KEY);
  }

  public Long getSoid() {
    return getKeyNextValue(SOID_KEY);
  }

  public Long getAmid() {
    return getKeyNextValue(AMID_KEY);
  }

  public Long getCurrentSdid() {
    return getCurrentValue(SDID_KEY);
  }

  public Long getCurrentSoid() {
    return getCurrentValue(SOID_KEY);
  }

  public Long getCurrentAmid() {
    return getCurrentValue(AMID_KEY);
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
