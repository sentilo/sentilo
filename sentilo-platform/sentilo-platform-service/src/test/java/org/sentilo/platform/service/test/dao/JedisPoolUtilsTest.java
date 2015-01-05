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

import static org.mockito.Matchers.any;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.io.IOException;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.platform.common.exception.SentiloDataAccessException;
import org.sentilo.platform.service.dao.JedisPoolUtils;

import redis.clients.jedis.Jedis;
import redis.clients.jedis.JedisPool;
import redis.clients.jedis.exceptions.JedisException;

public class JedisPoolUtilsTest {

  @InjectMocks
  private JedisPoolUtils jedisPoolUtils;

  @Mock
  private JedisPool pool;

  @Mock
  private Jedis jedis;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void getResource() {
    jedisPoolUtils.getResource();

    verify(pool).getResource();
  }

  @Test
  public void returnResource() {
    jedisPoolUtils.returnResource(jedis);
    verify(pool).returnResource(jedis);
  }

  @Test
  public void returnResourceWithNullJedis() {
    jedisPoolUtils.returnResource(null);
    verify(pool, times(0)).returnResource(any(Jedis.class));
  }

  @Test
  public void returnBrokenResource() {
    jedisPoolUtils.returnBrokenResource(jedis);
    verify(pool).returnBrokenResource(jedis);
  }

  @Test
  public void returnBrokenResourceWithNullJedis() {
    jedisPoolUtils.returnBrokenResource(null);
    verify(pool, times(0)).returnBrokenResource(any(Jedis.class));
  }

  @Test
  public void destroy() {
    jedisPoolUtils.destroy();
    verify(pool).destroy();
  }

  @Test
  public void releaseConnection() {
    jedisPoolUtils.releaseConnection(jedis, true);
    verify(pool).returnBrokenResource(jedis);

    jedisPoolUtils.releaseConnection(jedis, false);
    verify(pool).returnResource(jedis);
  }

  @Test
  public void releaseConnectionWithException() {
    doThrow(Exception.class).when(pool).returnResource(jedis);

    jedisPoolUtils.releaseConnection(jedis, false);
    verify(pool).returnResource(jedis);
    verify(pool).returnBrokenResource(jedis);
  }

  @Test
  public void convertJedisAccessException() {
    Assert.assertTrue(jedisPoolUtils.convertJedisAccessException(new JedisException("")) instanceof SentiloDataAccessException);
    Assert.assertTrue(jedisPoolUtils.convertJedisAccessException(new IOException("")) instanceof SentiloDataAccessException);
    Assert.assertTrue(jedisPoolUtils.convertJedisAccessException(new NullPointerException("")) instanceof SentiloDataAccessException);
  }

}
