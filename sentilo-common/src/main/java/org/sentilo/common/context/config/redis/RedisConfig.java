/*
 * Sentilo
 *  
 * Original version 1.4 Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de
 * Barcelona. Modified by Opentrends adding support for multitenant deployments and SaaS. 
 * Modifications on version 1.5 Copyright (C) 2015 Opentrends Solucions i Sistemes, S.L.
 * 
 *   
 * This program is licensed and may be used, modified and redistributed under the terms  of the 
 * European Public License (EUPL), either version 1.1 or (at your option) any later version as soon 
 * as they are approved by the European Commission.
 *   
 * Alternatively, you may redistribute and/or modify this program under the terms of the GNU Lesser 
 * General Public License as published by the Free Software Foundation; either  version 3 of the 
 * License, or (at your option) any later version. 
 *   
 * Unless required by applicable law or agreed to in writing, software distributed under the License 
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR  CONDITIONS OF ANY KIND, either express 
 * or implied. 
 *   
 * See the licenses for the specific language governing permissions, limitations and more details.
 *   
 * You should have received a copy of the EUPL1.1 and the LGPLv3 licenses along with this program; 
 * if not, you may find them at: 
 *   
 * https://joinup.ec.europa.eu/software/page/eupl/licence-eupl http://www.gnu.org/licenses/   and 
 * https://www.gnu.org/licenses/lgpl.txt
 */
package org.sentilo.common.context.config.redis;

import java.time.Duration;

import org.apache.commons.pool2.impl.GenericObjectPoolConfig;
import org.sentilo.common.utils.ModuleUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.data.redis.connection.RedisPassword;
import org.springframework.data.redis.connection.lettuce.LettucePoolingClientConfiguration;

import io.lettuce.core.ClientOptions;
import io.lettuce.core.ReadFrom;

public abstract class RedisConfig {

  @Autowired
  protected RedisProperties redisProperties;

  @Bean(name = "redisPassword")
  public RedisPassword redisPassword() {
    return RedisPassword.of(redisProperties.getPassword());
  }

  @SuppressWarnings("rawtypes")
  @Bean(name = "redisPoolConfig")
  public LettucePoolingClientConfiguration redisPoolConfig() {

    final GenericObjectPoolConfig objectPoolConfig = new GenericObjectPoolConfig();
    objectPoolConfig.setMaxTotal(redisProperties.getMaxTotal());
    objectPoolConfig.setMaxIdle(redisProperties.getMaxIdle());
    objectPoolConfig.setMaxWaitMillis(redisProperties.getMaxWaitMillis());
    objectPoolConfig.setTestOnBorrow(redisProperties.isTestOnBorrow());
    objectPoolConfig.setTestOnCreate(redisProperties.isTestOnCreate());
    objectPoolConfig.setTestOnReturn(redisProperties.isTestOnReturn());
    // Properties extracted from JedisPoolConfig
    objectPoolConfig.setTestWhileIdle(redisProperties.isTestWhileIdle());
    objectPoolConfig.setMinEvictableIdleTimeMillis(60000);
    objectPoolConfig.setTimeBetweenEvictionRunsMillis(30000);
    objectPoolConfig.setNumTestsPerEvictionRun(-1);

    final LettucePoolingClientConfiguration poolingConfig =
        LettucePoolingClientConfiguration.builder().poolConfig(objectPoolConfig).commandTimeout(Duration.ofMillis(redisProperties.getConnTimeout()))
            .clientName(getClientName()).readFrom(ReadFrom.UPSTREAM_PREFERRED).clientOptions(getClientOptions()).build();

    return poolingConfig;
  }

  protected String getClientName() {
    // Redis clients are either agents or API Server.
    return ModuleUtils.getModuleName();
  }

  protected abstract ClientOptions getClientOptions();

}
