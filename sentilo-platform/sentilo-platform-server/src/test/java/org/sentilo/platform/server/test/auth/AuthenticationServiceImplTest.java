/*
 * Sentilo
 * 
 * Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de Barcelona.
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

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.platform.common.domain.CredentialMessage;
import org.sentilo.platform.common.domain.CredentialsMessage;
import org.sentilo.platform.common.service.CatalogService;
import org.sentilo.platform.server.auth.impl.AuthenticationServiceImpl;
import org.sentilo.platform.server.exception.UnauthorizedException;
import org.sentilo.platform.server.test.AbstractBaseTest;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class AuthenticationServiceImplTest extends AbstractBaseTest {

  private final Logger logger = LoggerFactory.getLogger(AuthenticationServiceImplTest.class);
  private final String token = "646967a9f99ae76cfb836026d0015c4b80f8c0e1efbd3d261250156efd8fb96f";

  private final AuthenticationServiceImpl authenticationService = new AuthenticationServiceImpl();
  private Runnable runnable;

  @Mock
  private CatalogService catalogService;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    runnable = new AuthenticationRunnable(authenticationService);
    authenticationService.setCatalogService(catalogService);

    final CredentialMessage credential = new CredentialMessage();
    credential.setToken(token);
    credential.setEntity("periko");

    final List<CredentialMessage> credentials = new ArrayList<CredentialMessage>();
    credentials.add(credential);

    final CredentialsMessage message = new CredentialsMessage();
    message.setCredentials(credentials);

    when(catalogService.getCredentials()).thenReturn(message);
  }

  @Test
  public void testConcurrence() throws Exception {
    for (int i = 0; i < 25; i++) {
      final Thread thread = new Thread(runnable);
      thread.setName(Integer.toString(i));
      thread.setDaemon(false);
      thread.start();
    }

    // Wait 4 seconds until all threads are finish
    Thread.sleep(4000);

  }

  public class AuthenticationRunnable implements Runnable {

    private final AuthenticationServiceImpl authenticationService;

    public AuthenticationRunnable(final AuthenticationServiceImpl authenticationService) {
      this.authenticationService = authenticationService;
    }

    @Override
    public void run() {
      logger.debug("Running thread {}", Thread.currentThread().getName());
      try {
        if (Integer.valueOf(Thread.currentThread().getName()) % 10 == 0) {
          authenticationService.loadActiveCredentials();
        } else {
          authenticationService.getIdentity(token);
        }
      } catch (final UnauthorizedException e) {
        logger.debug("Error validating credential in thread: {}", Thread.currentThread().getName());
      } finally {
        logger.debug("Finished thread {} ", Thread.currentThread().getName());
      }
    }
  }
}
