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
import java.util.Arrays;
import java.util.List;

import org.redisson.Redisson;
import org.redisson.api.RedissonClient;
import org.redisson.config.ClusterServersConfig;
import org.redisson.config.Config;
import org.redisson.config.ReadMode;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import org.springframework.data.redis.connection.RedisClusterConfiguration;
import org.springframework.data.redis.connection.RedisConnectionFactory;
import org.springframework.data.redis.connection.lettuce.LettuceConnectionFactory;
import org.springframework.util.Assert;

import io.lettuce.core.ClientOptions;
import io.lettuce.core.ClientOptions.DisconnectedBehavior;
import io.lettuce.core.cluster.ClusterClientOptions;
import io.lettuce.core.cluster.ClusterTopologyRefreshOptions;

@Configuration
@Profile("cluster")
public class RedisClusterConfig extends RedisConfig {

  @Bean(name = "redisConnectionFactory")
  public RedisConnectionFactory redisConnectionFactory() {
    return new LettuceConnectionFactory(redisConfiguration(), redisPoolConfig());
  }

  @Bean(name = "redisConfiguration")
  public RedisClusterConfiguration redisConfiguration() {
    final String clusterNodes = redisProperties.getNodes();
    Assert.hasLength(clusterNodes, "[Assertion failed] - cluster nodes must not be null or empty!!");

    final List<String> redisNodes = Arrays.asList(clusterNodes.split(","));
    final RedisClusterConfiguration redisConfiguration = new RedisClusterConfiguration(redisNodes);
    redisConfiguration.setMaxRedirects(redisProperties.getMaxRedirects());
    redisConfiguration.setPassword(redisPassword());

    return redisConfiguration;
  }

  @Bean(name = "redissonClient")
  public RedissonClient redissonClient() {
    final Config cfg = new Config();
    cfg.setLockWatchdogTimeout(Duration.ofSeconds(5).toMillis());
    cfg.setUseScriptCache(true);
    // cfg.

    final String[] rNodes = Arrays.asList(redisProperties.getNodes().split(",")).stream().map(entry -> "redis://" + entry).toArray(String[]::new);
    final ClusterServersConfig csc = cfg.useClusterServers();
    csc.addNodeAddress(rNodes);
    // To evict startup error "Not all slots are covered"
    // https://github.com/redisson/redisson/issues/2284
    csc.setCheckSlotsCoverage(false);
    csc.setPassword(redisProperties.getPassword());
    csc.setReadMode(ReadMode.MASTER);
    cfg.setLockWatchdogTimeout(Duration.ofSeconds(5).toMillis());
    cfg.setNettyThreads(0);
    cfg.setThreads(0);

    return Redisson.create(cfg);
  }

  @Override
  protected ClientOptions getClientOptions() {
    // @formatter:off
    final ClusterTopologyRefreshOptions topologyRefreshOptions = ClusterTopologyRefreshOptions.builder()
        .enablePeriodicRefresh(Duration.ofSeconds(60))
        .enableAllAdaptiveRefreshTriggers()
        .build();

    return ClusterClientOptions.builder()
        .topologyRefreshOptions(topologyRefreshOptions)
        .autoReconnect(true)
        .cancelCommandsOnReconnectFailure(true)
        .pingBeforeActivateConnection(true)
        .disconnectedBehavior(DisconnectedBehavior.DEFAULT).build();

    // @formatter:on
  }
}
