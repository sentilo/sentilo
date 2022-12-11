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

import static org.mockito.Mockito.when;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.context.config.redis.RedisProperties;
import org.sentilo.common.context.config.redis.RedisStandaloneConfig;
import org.sentilo.common.test.AbstractBaseTest;
import org.springframework.data.redis.connection.RedisConfiguration;
import org.springframework.data.redis.connection.RedisPassword;
import org.springframework.data.redis.connection.RedisStandaloneConfiguration;
import org.springframework.data.redis.connection.lettuce.LettuceConnectionFactory;

import io.lettuce.core.ClientOptions;

public class RedisStandaloneConfigTest extends AbstractBaseTest {

  private final String host = "redis.acme.io";
  private final int port = 6777;
  private final String redisPassword = "1234";

  @InjectMocks
  private RedisStandaloneConfig config;

  @Mock
  private RedisProperties redisProperties;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    when(redisProperties.getPassword()).thenReturn(redisPassword);
    when(redisProperties.getHost()).thenReturn(host);
    when(redisProperties.getPort()).thenReturn(port);
  }

  @Test
  public void redisPassword() {
    final RedisPassword rp = config.redisPassword();
    Assert.assertArrayEquals(redisPassword.toCharArray(), rp.get());
  }

  @Test
  public void redisConfiguration() {
    final RedisConfiguration rsc = config.redisConfiguration();
    Assert.assertTrue(rsc instanceof RedisStandaloneConfiguration);
    Assert.assertEquals(host, ((RedisStandaloneConfiguration) rsc).getHostName());
    Assert.assertEquals(port, ((RedisStandaloneConfiguration) rsc).getPort());

  }

  @Test
  public void redisConnectionFactory() {
    final LettuceConnectionFactory rcf = (LettuceConnectionFactory) config.redisConnectionFactory();
    Assert.assertNotNull(rcf.getStandaloneConfiguration());
    Assert.assertEquals(host, rcf.getStandaloneConfiguration().getHostName());
    Assert.assertEquals(port, rcf.getStandaloneConfiguration().getPort());
    Assert.assertTrue(rcf.getClientConfiguration().getClientOptions().isPresent());
    Assert.assertTrue(rcf.getClientConfiguration().getClientOptions().get() instanceof ClientOptions);
  }
}
