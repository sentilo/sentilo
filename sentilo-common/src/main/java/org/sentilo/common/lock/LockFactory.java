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
package org.sentilo.common.lock;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import javax.annotation.PostConstruct;

import org.redisson.RedissonLock;
import org.redisson.api.RedissonClient;
import org.sentilo.common.utils.SentiloConstants;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.stereotype.Component;

/**
 * Provides a central point for build a Lock based on instance context (simple or cluster deploy).
 * If instance is simple, then returns a ReentrantLock instance, otherwise returns a distributed
 * lock based on Redis an implemented by Redisson library.
 *
 * Returned locks are no-fairness to improve throughput.
 *
 * @see ReentrantLock
 * @see RedissonLock
 */
@Component
public class LockFactory {

  @Autowired(required = false)
  private RedissonClient redisson;

  @Autowired
  private Environment environment;

  private final Map<String, Lock> lockRegistry = new HashMap<String, Lock>();

  private boolean isClusterInstance;

  @PostConstruct
  public void init() {
    final String[] profiles = environment.getActiveProfiles();
    isClusterInstance = Arrays.asList(profiles).contains(SentiloConstants.CLUSTER_PROFILE);
  }

  /**
   * Returns a Redis distributed lock by name, if it is a cluster instance. Otherwise returns a
   * ReentrantLock instance by name
   *
   * @param name
   * @return
   */
  public Lock getLock(final String name) {
    return getLock(name, true);
  }

  /**
   * Returns a Redis distributed lock by name, if it is a cluster instance and given distributed
   * parameter is true. Otherwise returns a ReentrantLock instance by name
   *
   * @param name
   * @param distributed
   * @return
   */
  public Lock getLock(final String name, final boolean distributed) {
    if (!lockRegistry.containsKey(name)) {
      lockRegistry.put(name, buildLock(name, distributed));
    }

    return lockRegistry.get(name);
  }

  private Lock buildLock(final String name, final boolean distributed) {
    // In case of a cluster with many nodes, change Redisson lock to RedissonSpinLock
    // https://github.com/redisson/redisson/issues/3349
    return isClusterInstance && distributed ? redisson.getLock(name) : new ReentrantLock();
  }

}
