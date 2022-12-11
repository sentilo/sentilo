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
package org.sentilo.common.test.context.config.redis;

import java.util.Properties;

import org.apache.commons.pool2.impl.BaseObjectPoolConfig;
import org.apache.commons.pool2.impl.GenericObjectPoolConfig;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.context.config.redis.RedisProperties;

public class RedisPropertiesTest {

  @Mock
  private Properties sentiloConfigProperties;

  @InjectMocks
  private RedisProperties redisProperties;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void init() {
    redisProperties.init();

    Assert.assertEquals("sentilo", redisProperties.getPassword());
    Assert.assertEquals(6379, redisProperties.getPort());
    Assert.assertEquals("127.0.0.1", redisProperties.getHost());
    Assert.assertEquals(0l, redisProperties.getExpire());
    Assert.assertEquals("sentilo", redisProperties.getPassword());
    Assert.assertEquals(-1l, redisProperties.getConnTimeout());
    Assert.assertEquals(GenericObjectPoolConfig.DEFAULT_MAX_TOTAL, redisProperties.getMaxTotal());
    Assert.assertEquals(GenericObjectPoolConfig.DEFAULT_MIN_IDLE, redisProperties.getMinIdle());
    Assert.assertEquals(GenericObjectPoolConfig.DEFAULT_MAX_IDLE, redisProperties.getMaxIdle());
    Assert.assertEquals(BaseObjectPoolConfig.DEFAULT_MAX_WAIT_MILLIS, redisProperties.getMaxWaitMillis());
    Assert.assertEquals(0, redisProperties.getMaxRedirects());
    Assert.assertNull(redisProperties.getNodes());
    Assert.assertEquals(BaseObjectPoolConfig.DEFAULT_TEST_ON_BORROW, redisProperties.isTestOnBorrow());
    Assert.assertEquals(BaseObjectPoolConfig.DEFAULT_TEST_ON_CREATE, redisProperties.isTestOnCreate());
    Assert.assertEquals(BaseObjectPoolConfig.DEFAULT_TEST_ON_RETURN, redisProperties.isTestOnReturn());
    Assert.assertEquals(BaseObjectPoolConfig.DEFAULT_TEST_WHILE_IDLE, redisProperties.isTestWhileIdle());
  }
}
