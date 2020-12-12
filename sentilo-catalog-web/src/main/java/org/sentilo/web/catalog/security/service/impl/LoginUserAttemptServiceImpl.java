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
package org.sentilo.web.catalog.security.service.impl;

import org.sentilo.common.cache.LRUCache;
import org.sentilo.common.cache.impl.LRUCacheImpl;
import org.sentilo.web.catalog.domain.User;
import org.sentilo.web.catalog.security.service.LoginAttemptService;
import org.sentilo.web.catalog.service.impl.UserServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

/**
 * Simple control to evict DoS attack. It maintains an in-memory cache to control user failed
 * attempts. If a user exceeds the maximum predefined limit of errors then he is blocked.
 */
@Service
public class LoginUserAttemptServiceImpl implements LoginAttemptService {

  @Autowired
  private UserServiceImpl userService;

  @Value("${login.attempts:3}")
  private int maxAttempts;

  private LRUCache<String, Integer> attemptsCache;

  public LoginUserAttemptServiceImpl() {
    super();
    attemptsCache = new LRUCacheImpl<String, Integer>(10000, 24 * 60);
  }

  @Override
  public void loginFailed(final String userName) {
    Integer currentAttempts = attemptsCache.get(userName, 0);
    currentAttempts++;
    attemptsCache.put(userName, currentAttempts);

    if (userService.exists(userName) && currentAttempts == maxAttempts) {
      final User user = userService.find(new User(userName));
      user.setActive(false);
      userService.getRepository().save(user);
    }
  }

  @Override
  public void loginSucceeded(final String userName) {
    attemptsCache.remove(userName);
  }
}
