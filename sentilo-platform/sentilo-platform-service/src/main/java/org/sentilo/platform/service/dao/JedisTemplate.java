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
package org.sentilo.platform.service.dao;

import java.util.Map;
import java.util.Set;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.listener.RedisMessageListenerContainer;
import org.springframework.stereotype.Component;
import org.springframework.util.Assert;

import redis.clients.jedis.Jedis;
import redis.clients.jedis.exceptions.JedisConnectionException;

/**
 * Based on RedisTemplate from spring-data-redis.
 * 
 * As not spring-data-redis 1.0.3 implements all redis available commands (e.g. zremrangebyrank )
 * and RedisMessageListenerContainer no lets unsubscribe for a subscription (see
 * http://forum.springsource.org/showthread.php?133937-Spring-Data-Redis-pub-sub-unsubscribe), all
 * the functionality needed is replicated in this project.
 * 
 * Upgrade: new versions of spring-data-redis already implements the unsubscribe functionality (see
 * https://jira.springsource.org/browse/DATAREDIS-107)
 * 
 * @see RedisMessageListenerContainer
 * @see DefaultZSetOperations
 */
@Component
public class JedisTemplate<K, V> {

  @Autowired
  private JedisPoolUtils jedisPoolUtils;

  // Merece la pena utilizar este template? Internamente realiza diversos pasos (pipeline,
  // transaccionalidad, .... ) que para nosotros no son necesarios
  // y simplemente añaden una nueva capa de abstracción sobre la comunicacion con Redis.
  // private RedisTemplate<String, String> redisTemplate;

  /**
   * Executes the given action object within a connection that can be exposed or not. Additionally,
   * the connection can be pipelined. Note the results of the pipeline are discarded (making it
   * suitable for write-only scenarios).
   * 
   * @param <T> return type
   * @param action callback object to execute
   * @return object returned by the action
   */
  public <T> T execute(final JedisCallback<T> action) {
    Assert.notNull(action, "Callback object must not be null");

    Jedis conn = null;
    boolean broken = false;
    try {
      conn = jedisPoolUtils.getResource();
      return action.doInRedis(conn);
    } catch (final Exception e) {
      if (e instanceof JedisConnectionException) {
        broken = true;
      }
      throw jedisPoolUtils.convertJedisAccessException(e);
    } finally {
      jedisPoolUtils.releaseConnection(conn, broken);
    }
  }

  public Set<String> keys(final String pattern) {
    return execute(new JedisCallback<Set<String>>() {

      public Set<String> doInRedis(final Jedis connection) {
        return connection.keys(pattern);
      }
    });
  }

  public String get(final String key) {
    return execute(new JedisCallback<String>() {

      public String doInRedis(final Jedis connection) {
        return connection.get(key);
      }
    });
  }

  /** Esta es la unica operacion que implica dos llamadas a Redis. --> Usar pipeline? */
  public Long getKeyNextValue(final String key) {
    return execute(new JedisCallback<Long>() {

      public Long doInRedis(final Jedis connection) {
        connection.setnx(key, "0");
        return connection.incr(key);
      }
    });
  }

  public String set(final String key, final String value) {
    return execute(new JedisCallback<String>() {

      public String doInRedis(final Jedis connection) {
        return connection.set(key, value);
      }
    });
  }

  public Long del(final String key) {
    return execute(new JedisCallback<Long>() {

      public Long doInRedis(final Jedis connection) {
        return connection.del(key);
      }
    });
  }

  public Set<String> sMembers(final String key) {
    return execute(new JedisCallback<Set<String>>() {

      public Set<String> doInRedis(final Jedis connection) {
        return connection.smembers(key);
      }
    });
  }

  public Long sAdd(final String key, final String... members) {
    return execute(new JedisCallback<Long>() {

      public Long doInRedis(final Jedis connection) {
        return connection.sadd(key, members);
      }
    });
  }

  public Long sRem(final String key, final String... members) {
    return execute(new JedisCallback<Long>() {

      public Long doInRedis(final Jedis connection) {
        return connection.srem(key, members);
      }
    });
  }

  public Set<String> zRevRangeByScore(final String key, final double max, final double min, final int offset, final int count) {
    return execute(new JedisCallback<Set<String>>() {

      public Set<String> doInRedis(final Jedis connection) {
        return connection.zrevrangeByScore(key, max, min, offset, count);
      }
    });
  }

  public Set<String> zRange(final String key, final long start, final long end) {
    return execute(new JedisCallback<Set<String>>() {

      public Set<String> doInRedis(final Jedis connection) {
        return connection.zrange(key, start, end);
      }
    });
  }

  public Long zAdd(final String key, final double score, final String member) {
    return execute(new JedisCallback<Long>() {

      public Long doInRedis(final Jedis connection) {
        return connection.zadd(key, score, member);
      }
    });
  }

  public Long zRemRangeByRank(final String key, final long start, final long end) {
    return execute(new JedisCallback<Long>() {

      public Long doInRedis(final Jedis connection) {
        return connection.zremrangeByRank(key, start, end);
      }
    });
  }

  public String hmSet(final String key, final Map<String, String> hash) {
    return execute(new JedisCallback<String>() {

      public String doInRedis(final Jedis connection) {
        return connection.hmset(key, hash);
      }
    });
  }

  public Map<String, String> hGetAll(final String key) {
    return execute(new JedisCallback<Map<String, String>>() {

      public Map<String, String> doInRedis(final Jedis connection) {
        return connection.hgetAll(key);
      }
    });
  }

  public String hGet(final String key, final String field) {
    return execute(new JedisCallback<String>() {

      public String doInRedis(final Jedis connection) {
        return connection.hget(key, field);
      }
    });
  }

  public Long hSet(final String key, final String field, final String value) {
    return execute(new JedisCallback<Long>() {

      public Long doInRedis(final Jedis connection) {
        return connection.hset(key, field, value);
      }
    });
  }

  public Long hDel(final String key, final String... fields) {
    return execute(new JedisCallback<Long>() {

      public Long doInRedis(final Jedis connection) {
        return connection.hdel(key, fields);
      }
    });
  }

  public Set<String> hKeys(final String key) {
    return execute(new JedisCallback<Set<String>>() {

      public Set<String> doInRedis(final Jedis connection) {
        return connection.hkeys(key);
      }
    });
  }

  public Long publish(final String channel, final String message) {
    return execute(new JedisCallback<Long>() {

      public Long doInRedis(final Jedis connection) {
        return connection.publish(channel, message);
      }
    });
  }

  public Long expire(final String key, final int seconds) {
    return execute(new JedisCallback<Long>() {

      public Long doInRedis(final Jedis connection) {
        return connection.expire(key, seconds);
      }
    });
  }

  // Definir en esta clase cada uno de los métodos de Jedis que estamos invocando.
  // Definir package dao en el cual añadir todas las clases de Jedis
  // Agrupar las operaciones por tipo de comando

}
