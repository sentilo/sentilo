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
package org.sentilo.web.catalog.test.security.service;

import static org.mockito.Matchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.cache.LRUCache;
import org.sentilo.web.catalog.domain.User;
import org.sentilo.web.catalog.repository.UserRepository;
import org.sentilo.web.catalog.security.service.impl.LoginUserAttemptServiceImpl;
import org.sentilo.web.catalog.service.impl.UserServiceImpl;
import org.springframework.test.util.ReflectionTestUtils;

public class LoginUserAttemptServiceImplTest {

  private static Integer MAX_ATTEMPTS = 3;

  @Mock
  private UserServiceImpl userService;

  @Mock
  private UserRepository repository;

  @Mock
  private User user;

  @InjectMocks
  private LoginUserAttemptServiceImpl loginService;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    ReflectionTestUtils.setField(loginService, "maxAttempts", MAX_ATTEMPTS);
  }

  @SuppressWarnings({"unchecked", "rawtypes"})
  @Test
  public void loginSucceeded() {
    final String userName = "mockUserName";
    final LRUCache<String, Integer> attemptsCache = (LRUCache) ReflectionTestUtils.getField(loginService, "attemptsCache");

    loginService.loginSucceeded(userName);

    Assert.assertFalse(attemptsCache.contains(userName));
  }

  @SuppressWarnings({"unchecked", "rawtypes"})
  @Test
  public void loginFailed() {
    final String userName = "mockWrongUserName";
    final LRUCache<String, Integer> attemptsCache = (LRUCache) ReflectionTestUtils.getField(loginService, "attemptsCache");

    when(userService.exists(userName)).thenReturn(true);
    when(userService.find(any(User.class))).thenReturn(user);
    when(userService.getRepository()).thenReturn(repository);

    for (int i = 0; i < MAX_ATTEMPTS; i++) {
      loginService.loginFailed(userName);
    }

    Assert.assertTrue(attemptsCache.contains(userName));
    Assert.assertEquals(MAX_ATTEMPTS, attemptsCache.get(userName));
    verify(repository, times(1)).save(user);
  }

}
