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

import java.util.Map;
import java.util.Set;
import java.util.concurrent.TimeUnit;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Component;

@Component
public class SentiloRedisTemplate {

  @Autowired
  private StringRedisTemplate redisTemplate;

  public Set<String> keys(final String pattern) {
    return redisTemplate.keys(pattern);
  }

  public String get(final String key) {
    return redisTemplate.opsForValue().get(key);
  }

  public Long getKeyNextValue(final String key) {
    redisTemplate.opsForValue().setIfAbsent(key, "0");
    return redisTemplate.opsForValue().increment(key, 1);
  }

  public void set(final String key, final String value) {
    redisTemplate.opsForValue().set(key, value);
  }

  public Boolean del(final String key) {
    return redisTemplate.delete(key);
  }

  public Set<String> sMembers(final String key) {
    return redisTemplate.opsForSet().members(key);
  }

  public Long sAdd(final String key, final String... members) {
    return redisTemplate.opsForSet().add(key, members);
  }

  public Long sRem(final String key, final String... members) {
    return redisTemplate.opsForSet().remove(key, members);
  }

  public Set<String> zRevRangeByScore(final String key, final double max, final double min, final int offset, final int count) {
    return redisTemplate.opsForZSet().reverseRangeByScore(key, min, max, offset, count);
  }

  public Set<String> zRange(final String key, final long start, final long end) {
    return redisTemplate.opsForZSet().range(key, start, end);
  }

  public Boolean zAdd(final String key, final double score, final String member) {
    return redisTemplate.opsForZSet().add(key, member, score);
  }

  public Long zRemRangeByRank(final String key, final long start, final long end) {
    return redisTemplate.opsForZSet().removeRange(key, start, end);
  }

  public void hmSet(final String key, final Map<String, String> hash) {
    redisTemplate.opsForHash().putAll(key, hash);
  }

  public Map<String, String> hGetAll(final String key) {
    return redisTemplate.<String, String>opsForHash().entries(key);
  }

  public String hGet(final String key, final String field) {
    return redisTemplate.<String, String>opsForHash().get(key, field);
  }

  public void hSet(final String key, final String field, final String value) {
    redisTemplate.<String, String>opsForHash().put(key, field, value);
  }

  public Long hDel(final String key, final String... fields) {
    return redisTemplate.<String, String>opsForHash().delete(key, fields);
  }

  public Set<String> hKeys(final String key) {
    return redisTemplate.<String, String>opsForHash().keys(key);
  }

  public void publish(final String channel, final String message) {
    redisTemplate.convertAndSend(channel, message);
  }

  public Boolean expire(final String key, final int seconds) {
    return redisTemplate.expire(key, seconds, TimeUnit.SECONDS);
  }

}
