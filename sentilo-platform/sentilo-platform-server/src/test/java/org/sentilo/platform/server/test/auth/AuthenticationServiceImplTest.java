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

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

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
  private Runnable runnable;

  @InjectMocks
  private AuthenticationServiceImpl authenticationService;
  @Mock
  private EntityMetadataRepository credentialsRepository;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    runnable = new AuthenticationRunnable(authenticationService);

    final EntityMetadataMessage entityMetadata = new EntityMetadataMessage();
    entityMetadata.setToken(token);
    entityMetadata.setEntity(mockEntity);

    when(credentialsRepository.containsEntityCredential(token)).thenReturn(Boolean.TRUE);
    when(credentialsRepository.getEntityMetadataFromToken(token)).thenReturn(entityMetadata);
  }

  @SuppressWarnings({"unchecked", "rawtypes"})
  @Test
  public void testConcurrence() throws Exception {

    final int poolSize = 10;
    final ExecutorService service = Executors.newFixedThreadPool(poolSize);
    final List<Future<Runnable>> futures = new ArrayList<Future<Runnable>>();

    for (int n = 0; n < poolSize; n++) {
      final Future f = service.submit(runnable);
      futures.add(f);
    }

    // wait for all tasks to complete before continuing
    for (final Future<Runnable> f : futures) {
      f.get();
    }

    // shut down the executor service so that this thread can exit
    service.shutdownNow();
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

  public class AuthenticationRunnable implements Runnable {

    private final AuthenticationServiceImpl authenticationService;

    public AuthenticationRunnable(final AuthenticationServiceImpl authenticationService) {
      this.authenticationService = authenticationService;
    }

    @Override
    public void run() {
      LOGGER.debug("Running thread {}", Thread.currentThread().getName());
      try {
        authenticationService.checkCredential(token);
        Assert.assertEquals(token, RequesterContextHolder.getContext().getToken());

      } catch (final UnauthorizedException e) {
        LOGGER.debug("Error validating credential in thread: {}", Thread.currentThread().getName());
      } finally {
        LOGGER.debug("Finished thread {} ", Thread.currentThread().getName());
      }
    }
  }
}
