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
package org.sentilo.platform.server.test.pool;

import static org.junit.Assert.assertEquals;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyLong;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import org.sentilo.platform.server.SentiloHttpRequestTask;
import org.sentilo.platform.server.pool.ThreadPool;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.test.util.ReflectionTestUtils;

@RunWith(PowerMockRunner.class)
@PrepareForTest({ThreadPool.class, LoggerFactory.class, ReflectionTestUtils.class})
public class ThreadPoolTest {

  @InjectMocks
  private ThreadPool pool;

  @Mock
  private ThreadPoolExecutor poolExecutor;

  @Mock
  private BlockingQueue<Runnable> poolQueue;

  @Mock
  private SentiloHttpRequestTask task;

  private static Logger logger;

  @BeforeClass
  public static void setUpStatic() throws Exception {
    PowerMockito.mockStatic(LoggerFactory.class);
    logger = PowerMockito.mock(Logger.class);
    when(LoggerFactory.getLogger(anyString())).thenReturn(logger);
    when(LoggerFactory.getLogger(any(Class.class))).thenReturn(logger);
  }

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    ReflectionTestUtils.setField(pool, "queueSize", new Integer(10));
    ReflectionTestUtils.setField(pool, "initialCapacity", new Integer(1));
    ReflectionTestUtils.setField(pool, "maxCapacity", new Integer(5));
    ReflectionTestUtils.setField(pool, "shutdownSecondsTimeout", new Integer(2));
  }

  @Test
  public void initialize() {
    pool.initialize();
    verify(logger, times(2)).info(anyString());
  }

  @Test
  public void submit() {
    ReflectionTestUtils.setField(pool, "threadPool", poolExecutor);

    pool.submit(task);

    verify(poolExecutor).submit(task);
  }

  @Test
  public void shutdown() {
    pool.shutdown();

    verify(poolExecutor).shutdown();
  }

  @Test
  public void shutdownControlled() {
    pool.shutdownControlled();

    try {
      verify(poolExecutor).awaitTermination(anyLong(), eq(TimeUnit.SECONDS));
    } catch (final InterruptedException e) {
      verify(poolExecutor).shutdownNow();
    }
  }

  @Test
  public void shutdownControlled_withInterruptedException() throws InterruptedException {
    doThrow(new InterruptedException()).when(poolExecutor).awaitTermination(anyLong(), eq(TimeUnit.SECONDS));

    pool.shutdownControlled();

    verify(poolExecutor).shutdownNow();
  }

  @Test
  public void shutdownNow() throws Exception {
    when(poolExecutor.awaitTermination(new Integer(2), TimeUnit.SECONDS)).thenReturn(Boolean.FALSE);
    pool.shutdown();

    verify(poolExecutor).shutdown();
    verify(poolExecutor).shutdownNow();
  }

  @Test
  public void getCurrentTasks() {
    final int activeCount = 4;
    final int queueSize = 5;

    when(poolExecutor.getActiveCount()).thenReturn(activeCount);
    when(poolExecutor.getQueue()).thenReturn(poolQueue);
    when(poolQueue.size()).thenReturn(queueSize);

    final long currentTasks = pool.getCurrentTasks();

    assertEquals(activeCount + queueSize, currentTasks);

  }

}
