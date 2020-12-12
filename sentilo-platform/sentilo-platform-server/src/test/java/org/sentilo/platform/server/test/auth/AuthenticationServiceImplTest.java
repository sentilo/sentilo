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
package org.sentilo.platform.server.test.auth;

import static org.mockito.Mockito.when;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.IntStream;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.test.AbstractBaseTest;
import org.sentilo.platform.common.domain.EntityMetadataMessage;
import org.sentilo.platform.common.security.AnonymousIdentityContext;
import org.sentilo.platform.common.security.RequesterContextHolder;
import org.sentilo.platform.common.security.repository.EntityMetadataRepository;
import org.sentilo.platform.server.auth.impl.AuthenticationServiceImpl;
import org.sentilo.platform.server.exception.UnauthorizedException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.test.util.ReflectionTestUtils;

public class AuthenticationServiceImplTest extends AbstractBaseTest {

  private static final Logger LOGGER = LoggerFactory.getLogger(AuthenticationServiceImplTest.class);
  private final String token = "646967a9f99ae76cfb836026d0015c4b80f8c0e1efbd3d261250156efd8fb96f";
  private final String mockEntity = "mockEntityId";

  @InjectMocks
  private AuthenticationServiceImpl authenticationService;
  @Mock
  private EntityMetadataRepository credentialsRepository;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);

    final EntityMetadataMessage entityMetadata = new EntityMetadataMessage();
    entityMetadata.setToken(token);
    entityMetadata.setEntity(mockEntity);

    when(credentialsRepository.containsEntityCredential(token)).thenReturn(Boolean.TRUE);
    when(credentialsRepository.getEntityMetadataFromToken(token)).thenReturn(entityMetadata);
  }

  @Test
  public void testConcurrence() {
    final int totalReq = 15;
    final int poolSize = 10;
    final ExecutorService service = Executors.newFixedThreadPool(poolSize);
    final CountDownLatch doneSignal = new CountDownLatch(totalReq);
    final AtomicInteger allowedCount = new AtomicInteger(0);
    final AtomicInteger blockedCount = new AtomicInteger(0);
    final Runnable requestTask = () -> {

      LOGGER.debug("Running thread {}", Thread.currentThread().getName());
      try {
        authenticationService.checkCredential(token);
        Assert.assertEquals(token, RequesterContextHolder.getContext().getToken());
        allowedCount.incrementAndGet();
      } catch (final UnauthorizedException e) {
        LOGGER.debug("Error validating credential in thread: {}", Thread.currentThread().getName());
        blockedCount.incrementAndGet();
      } finally {
        LOGGER.debug("Finished thread {} ", Thread.currentThread().getName());
      }

      try {
        TimeUnit.MILLISECONDS.sleep(10);
      } catch (final InterruptedException e) {
        e.printStackTrace();
      }

      doneSignal.countDown();
    };

    IntStream.range(0, totalReq).forEach(i -> service.submit(requestTask));

    try {
      doneSignal.await();
    } catch (final InterruptedException e) {
      e.printStackTrace();
    }

    service.shutdown();

    Assert.assertTrue(allowedCount.get() == totalReq);
    Assert.assertTrue(blockedCount.get() == 0);

  }

  @Test(expected = UnauthorizedException.class)
  public void testNoValidAnonymousAccess() throws Exception {
    authenticationService.checkCredential(null);
  }

  @Test
  public void testCheckCredential() throws Exception {
    authenticationService.checkCredential(token);
    Assert.assertEquals(token, RequesterContextHolder.getContext().getToken());
  }

  @Test
  public void testValidAnonymousAccess() throws Exception {
    ReflectionTestUtils.setField(authenticationService, "enableAnonymousAccess", Boolean.TRUE);
    ReflectionTestUtils.setField(authenticationService, "anonymousAppClientId", "ANONYMOUS");
    final EntityMetadataMessage anonymousMetadata = new EntityMetadataMessage();
    anonymousMetadata.setToken(token);
    anonymousMetadata.setEntity("ANONYMOUS");

    when(credentialsRepository.getEntityMetadataFromId("ANONYMOUS")).thenReturn(anonymousMetadata);

    authenticationService.checkCredential(null);
    Assert.assertTrue(RequesterContextHolder.getContext() instanceof AnonymousIdentityContext);
  }
}
