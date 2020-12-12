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
package org.sentilo.platform.common.test.ratelimiter;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.IntStream;

import org.sentilo.platform.common.domain.EntityMetadataMessage;
import org.sentilo.platform.common.ratelimiter.QuotaContext;
import org.sentilo.platform.common.ratelimiter.QuotaContextHolder;
import org.sentilo.platform.common.ratelimiter.service.InMemorySlidingWindowServiceImpl;
import org.sentilo.platform.common.ratelimiter.service.RateLimiterService;
import org.sentilo.platform.common.security.RequesterContext;
import org.sentilo.platform.common.security.RequesterContextHolder;

public class InMemorySlidingWindowServiceTest {

  private RateLimiterService rateLimiterService = new InMemorySlidingWindowServiceImpl();

  public static void main(final String[] args) {
    final InMemorySlidingWindowServiceTest boot = new InMemorySlidingWindowServiceTest();
    boot.run();
  }

  public static RequesterContext buildRequesterContext(final String account, final long quota) {
    final EntityMetadataMessage emm = new EntityMetadataMessage();
    emm.setEntity(account);
    emm.setApiInputQuota(quota);
    return new RequesterContext(emm);
  }

  public static void printResult(final String threadNum, final QuotaContext quotaContext) {
    System.out.println("---- thread: " + threadNum + " -- account: " + quotaContext.getAccount() + " -- total: " + quotaContext.getCurrent()
        + " -- quota: " + quotaContext.getLimit());
  }

  private void run() {
    final RequesterContext[] accountsContexts = {buildRequesterContext("account_1", 250), buildRequesterContext("account_2", 300)};
    sendRequest(rateLimiterService, accountsContexts, 510);
  }

  private void sendRequest(final RateLimiterService rateLimiter, final RequesterContext[] accountsContexts, final int totalCnt) {
    final long startTime = System.currentTimeMillis();
    final ExecutorService executor = Executors.newFixedThreadPool(20);
    final CountDownLatch doneSignal = new CountDownLatch(totalCnt);
    final AtomicInteger allowedCount = new AtomicInteger(0);
    final AtomicInteger blockedCount = new AtomicInteger(0);
    final Runnable requestTask = () -> {
      final RequesterContext rc = accountsContexts[(int) Thread.currentThread().getId() % 2];
      RequesterContextHolder.setContext(rc);
      if (rateLimiter.allow(rc.getEntityId())) {
        allowedCount.incrementAndGet();
        printResult(Thread.currentThread().getName(), QuotaContextHolder.getContext(QuotaContext.Type.ENTITY));
      } else {
        printResult("KO--" + Thread.currentThread().getName(), QuotaContextHolder.getContext(QuotaContext.Type.ENTITY));
        blockedCount.incrementAndGet();
      }
      try {
        TimeUnit.MILLISECONDS.sleep(10);
      } catch (final InterruptedException e) {
        e.printStackTrace();
      } finally {
        QuotaContextHolder.clearContext();
        RequesterContextHolder.clearContext();
      }

      doneSignal.countDown();
    };

    IntStream.range(0, totalCnt).forEach(i -> executor.submit(requestTask));

    try {
      doneSignal.await();
    } catch (final InterruptedException e) {
      e.printStackTrace();
    }

    executor.shutdown();

    System.out.println("allowedCount:" + allowedCount.get());
    System.out.println("blockedCount:" + blockedCount.get());

    final double duration = (System.currentTimeMillis() - startTime) / 1000.0;
    System.out.println(totalCnt + " requests processed in " + duration + " seconds. " + "Rate: " + totalCnt / duration + " per second");
  }

}
