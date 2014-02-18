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

import java.io.IOException;

import org.sentilo.platform.common.exception.SentiloDataAccessException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import redis.clients.jedis.Jedis;
import redis.clients.jedis.JedisPool;
import redis.clients.jedis.exceptions.JedisException;

@Component
public class JedisPoolUtils {

  @Autowired
  private JedisPool pool;

  public Jedis getResource() {
    return pool.getResource();
  }

  public void returnResource(final Jedis jedis) {
    if (jedis != null) {
      pool.returnResource(jedis);
    }
  }

  public void returnBrokenResource(final Jedis jedis) {
    if (jedis != null) {
      pool.returnBrokenResource(jedis);
    }
  }

  public void destroy() {
    if (pool != null) {
      pool.destroy();
    }
  }

  public void releaseConnection(final Jedis jedis, final boolean broken) throws SentiloDataAccessException {
    // return the connection to the pool
    // Segun el creador de Jedis, https://github.com/xetorthio/jedis/issues/91, este tipo de error
    // debe considerarse
    // un sinonimo de conexion rota y se debe eliminar del pool:
    // Is possible try to use JedisConnectionException as synonym to
    // "connection is broken and I should return is broken".
    try {
      if (broken) {
        returnBrokenResource(jedis);
      } else {
        returnResource(jedis);
      }
    } catch (final Exception ex) {
      returnBrokenResource(jedis);
    }
  }

  public SentiloDataAccessException convertJedisAccessException(final Exception ex) {
    if (ex instanceof JedisException) {
      return new SentiloDataAccessException(ex.getMessage(), ex);
    }
    if (ex instanceof IOException) {
      return new SentiloDataAccessException(ex.getMessage(), ex);
    }

    return new SentiloDataAccessException("Unknown jedis exception", ex);
  }

  public void setPool(final JedisPool pool) {
    this.pool = pool;
  }
}
