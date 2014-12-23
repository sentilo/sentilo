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
package org.sentilo.platform.service.test.dao;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.Map;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.platform.service.dao.JedisCallback;
import org.sentilo.platform.service.dao.JedisPoolUtils;
import org.sentilo.platform.service.dao.JedisTemplate;

import redis.clients.jedis.Jedis;

public class JedisTemplateTest {

  @Mock
  private JedisPoolUtils jedisPoolUtils;

  @SuppressWarnings("rawtypes")
  @Mock
  private JedisCallback jedisCallback;

  @Mock
  private Jedis conn;

  @InjectMocks
  private JedisTemplate<String, String> jedisTemplate;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    when(jedisPoolUtils.getResource()).thenReturn(conn);
  }

  @SuppressWarnings("unchecked")
  @Test
  public void execute() {
    jedisTemplate.execute(jedisCallback);

    verify(jedisPoolUtils).getResource();
    verify(jedisCallback).doInRedis(conn);
    verify(jedisPoolUtils).releaseConnection(conn, false);
  }

  @Test
  public void keys() {
    final String pattern = "abc*";
    jedisTemplate.keys(pattern);

    verify(conn).keys(pattern);
  }

  @Test
  public void get() {
    final String key = "abc";
    jedisTemplate.get(key);

    verify(conn).get(key);
  }

  @Test
  public void getKeyNextValue() {
    final String key = "abc";
    jedisTemplate.getKeyNextValue(key);

    verify(conn).setnx(key, "0");
    verify(conn).incr(key);
  }

  @Test
  public void set() {
    final String key = "abc";
    final String value = "def";
    jedisTemplate.set(key, value);

    verify(conn).set(key, value);
  }

  @Test
  public void del() {
    final String key = "abc";
    jedisTemplate.del(key);

    verify(conn).del(key);
  }

  @Test
  public void sMembers() {
    final String key = "abc";
    jedisTemplate.sMembers(key);

    verify(conn).smembers(key);
  }

  @Test
  public void sAdd() {
    final String key = "abc";
    final String[] members = {"def", "ghi"};

    jedisTemplate.sAdd(key, members);

    verify(conn).sadd(key, members);
  }

  @Test
  public void sRem() {
    final String key = "abc";
    final String[] members = {"def", "ghi"};

    jedisTemplate.sRem(key, members);

    verify(conn).srem(key, members);
  }

  @Test
  public void zRevRangeByScore() {
    final String key = "abc";
    final double max = 10;
    final double min = 1;
    final int offset = 0;
    final int count = 100;

    jedisTemplate.zRevRangeByScore(key, max, min, offset, count);

    verify(conn).zrevrangeByScore(key, max, min, offset, count);
  }

  @Test
  public void zRange() {
    final String key = "abc";
    final long start = 1;
    final long end = 10;

    jedisTemplate.zRange(key, start, end);

    verify(conn).zrange(key, start, end);
  }

  @Test
  public void zAdd() {
    final String key = "abc";
    final double score = 1;
    final String member = "def";

    jedisTemplate.zAdd(key, score, member);

    verify(conn).zadd(key, score, member);
  }

  @Test
  public void zRemRangeByRank() {
    final String key = "abc";
    final long start = 1;
    final long end = 10;

    jedisTemplate.zRemRangeByRank(key, start, end);

    verify(conn).zremrangeByRank(key, start, end);
  }

  @Test
  public void hmSet() {
    final String key = "abc";
    final Map<String, String> hash = Collections.<String, String>emptyMap();

    jedisTemplate.hmSet(key, hash);

    verify(conn).hmset(key, hash);
  }

  @Test
  public void hGetAll() {
    final String key = "abc";

    jedisTemplate.hGetAll(key);

    verify(conn).hgetAll(key);
  }

  @Test
  public void hGet() {
    final String key = "abc";
    final String field = "field";

    jedisTemplate.hGet(key, field);

    verify(conn).hget(key, field);
  }

  @Test
  public void hSet() {
    final String key = "abc";
    final String field = "field";
    final String value = "value";

    jedisTemplate.hSet(key, field, value);

    verify(conn).hset(key, field, value);
  }

  @Test
  public void hDel() {
    final String key = "abc";
    final String fields[] = {"field1", "field2"};

    jedisTemplate.hDel(key, fields);

    verify(conn).hdel(key, fields);
  }

  @Test
  public void hKeys() {
    final String key = "abc";

    jedisTemplate.hKeys(key);

    verify(conn).hkeys(key);
  }

  @Test
  public void publish() {
    final String channel = "abc";
    final String message = "message";

    jedisTemplate.publish(channel, message);

    verify(conn).publish(channel, message);
  }

  @Test
  public void expire() {
    final String key = "abc";
    final int seconds = 10;

    jedisTemplate.expire(key, seconds);

    verify(conn).expire(key, seconds);
  }
}
