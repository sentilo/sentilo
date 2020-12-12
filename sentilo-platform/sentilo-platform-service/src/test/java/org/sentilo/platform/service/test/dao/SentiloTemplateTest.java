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
package org.sentilo.platform.service.test.dao;

import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.platform.service.dao.SentiloRedisTemplate;
import org.springframework.data.redis.core.HashOperations;
import org.springframework.data.redis.core.SetOperations;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.data.redis.core.ValueOperations;
import org.springframework.data.redis.core.ZSetOperations;

public class SentiloTemplateTest {

  @Mock
  private StringRedisTemplate redisTemplate;

  @Mock
  private ValueOperations<String, String> vOps;

  @Mock
  private HashOperations<String, Object, Object> hOps;

  @Mock
  private ZSetOperations<String, String> zsOps;

  @Mock
  private SetOperations<String, String> sOps;

  @InjectMocks
  private SentiloRedisTemplate sRedisTemplate;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    when(redisTemplate.opsForValue()).thenReturn(vOps);
    when(redisTemplate.opsForHash()).thenReturn(hOps);
    when(redisTemplate.opsForSet()).thenReturn(sOps);
    when(redisTemplate.opsForZSet()).thenReturn(zsOps);
  }

  @Test
  public void keys() {
    final String pattern = "abc*";
    sRedisTemplate.keys(pattern);

    verify(redisTemplate).keys(pattern);
  }

  @Test
  public void get() {
    final String key = "abc";
    sRedisTemplate.get(key);

    verify(redisTemplate).opsForValue();
    verify(vOps).get(key);
  }

  @Test
  public void getKeyNextValue() {
    final String key = "abc";
    sRedisTemplate.getKeyNextValue(key);

    verify(redisTemplate, times(2)).opsForValue();
    verify(vOps).setIfAbsent(key, "0");
    verify(vOps).increment(key, 1);
  }

  @Test
  public void set() {
    final String key = "abc";
    final String value = "def";
    sRedisTemplate.set(key, value);

    verify(redisTemplate).opsForValue();
    verify(vOps).set(key, value);
  }

  @Test
  public void del() {
    final String key = "abc";
    sRedisTemplate.del(key);

    verify(redisTemplate).delete(key);
  }

  @Test
  public void sMembers() {
    final String key = "abc";
    sRedisTemplate.sMembers(key);

    verify(redisTemplate).opsForSet();
    verify(sOps).members(key);
  }

  @Test
  public void sAdd() {
    final String key = "abc";
    final String[] members = {"def", "ghi"};

    sRedisTemplate.sAdd(key, members);

    verify(redisTemplate).opsForSet();
    verify(sOps).add(key, members);
  }

  @Test
  public void sRem() {
    final String key = "abc";
    final String[] members = {"def", "ghi"};

    sRedisTemplate.sRem(key, members);

    verify(redisTemplate).opsForSet();
    verify(sOps).remove(key, members);
  }

  @Test
  public void zRevRangeByScore() {
    final String key = "abc";
    final double max = 10;
    final double min = 1;
    final int offset = 0;
    final int count = 100;

    sRedisTemplate.zRevRangeByScore(key, max, min, offset, count);

    verify(redisTemplate).opsForZSet();
    verify(zsOps).reverseRangeByScore(key, min, max, offset, count);
  }

  @Test
  public void zRange() {
    final String key = "abc";
    final long start = 1;
    final long end = 10;

    sRedisTemplate.zRange(key, start, end);

    verify(redisTemplate).opsForZSet();
    verify(zsOps).range(key, start, end);
  }

  @Test
  public void zAdd() {
    final String key = "abc";
    final double score = 1;
    final String member = "def";

    sRedisTemplate.zAdd(key, score, member);

    verify(redisTemplate).opsForZSet();
    verify(zsOps).add(key, member, score);
  }

  @Test
  public void zRemRangeByRank() {
    final String key = "abc";
    final long start = 1;
    final long end = 10;

    sRedisTemplate.zRemRangeByRank(key, start, end);

    verify(redisTemplate).opsForZSet();
    verify(zsOps).removeRange(key, start, end);
  }

  @Test
  public void hmSet() {
    final String key = "abc";
    final Map<String, String> hash = Collections.<String, String>emptyMap();

    sRedisTemplate.hmSet(key, hash);

    verify(redisTemplate).opsForHash();
    verify(hOps).putAll(key, hash);
  }

  @Test
  public void hGetAll() {
    final String key = "abc";

    sRedisTemplate.hGetAll(key);

    verify(redisTemplate).opsForHash();
    verify(hOps).entries(key);
  }

  @Test
  public void hGet() {
    final String key = "abc";
    final String field = "field";

    sRedisTemplate.hGet(key, field);

    verify(redisTemplate).opsForHash();
    verify(hOps).get(key, field);
  }

  @Test
  public void hSet() {
    final String key = "abc";
    final String field = "field";
    final String value = "value";

    sRedisTemplate.hSet(key, field, value);

    verify(redisTemplate).opsForHash();
    verify(hOps).put(key, field, value);
  }

  @Test
  public void hDel() {
    final String key = "abc";
    final String fields[] = {"field1", "field2"};

    sRedisTemplate.hDel(key, fields);

    verify(redisTemplate).opsForHash();
    verify(hOps).delete(key, fields);
  }

  @Test
  public void hKeys() {
    final String key = "abc";

    sRedisTemplate.hKeys(key);

    verify(redisTemplate).opsForHash();
    verify(hOps).keys(key);
  }

  @Test
  public void publish() {
    final String channel = "abc";
    final String message = "message";

    sRedisTemplate.publish(channel, message);

    verify(redisTemplate).convertAndSend(channel, message);
  }

  @Test
  public void expire() {
    final String key = "abc";
    final int seconds = 10;

    sRedisTemplate.expire(key, seconds);

    verify(redisTemplate).expire(key, seconds, TimeUnit.SECONDS);
  }

}
