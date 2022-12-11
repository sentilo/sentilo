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
package org.sentilo.common.test.lock;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.redisson.Redisson;
import org.redisson.RedissonLock;
import org.sentilo.common.lock.LockFactory;
import org.springframework.core.env.Environment;

public class LockFactoryTest {

  @Mock
  private Environment environment;

  @Mock
  private Redisson redisson;

  @Mock
  private RedissonLock rLock;

  @InjectMocks
  private LockFactory lockFactory;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void getLock_when_no_cluster() {
    final String lockName = "simpleLock";
    when(environment.getActiveProfiles()).thenReturn(new String[] {"dev"});
    lockFactory.init();
    final Lock lock = lockFactory.getLock(lockName);

    Assert.assertTrue(lock instanceof ReentrantLock);
  }

  @Test
  public void getLock_when_cluster() {
    final String lockName = "distributedLock";
    when(environment.getActiveProfiles()).thenReturn(new String[] {"dev", "cluster"});
    when(redisson.getLock(lockName)).thenReturn(rLock);
    lockFactory.init();
    final Lock lock = lockFactory.getLock(lockName);

    verify(redisson).getLock(lockName);
    Assert.assertTrue(lock instanceof RedissonLock);
  }

}
