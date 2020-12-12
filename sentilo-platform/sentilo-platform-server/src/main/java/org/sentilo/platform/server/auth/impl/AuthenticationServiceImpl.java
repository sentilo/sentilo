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
package org.sentilo.platform.server.auth.impl;

import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import org.sentilo.platform.common.domain.EntityMetadataMessage;
import org.sentilo.platform.common.security.AnonymousIdentityContext;
import org.sentilo.platform.common.security.RequesterContext;
import org.sentilo.platform.common.security.RequesterContextHolder;
import org.sentilo.platform.common.security.repository.EntityMetadataRepository;
import org.sentilo.platform.server.auth.AuthenticationService;
import org.sentilo.platform.server.exception.UnauthorizedException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

@Service
public class AuthenticationServiceImpl implements AuthenticationService {

  private static final Logger LOGGER = LoggerFactory.getLogger(AuthenticationServiceImpl.class);

  @Value("${enableAnonymousAccess:false}")
  private boolean enableAnonymousAccess;
  @Value("${anonymousAppClientId}")
  private String anonymousAppClientId;

  @Autowired
  private EntityMetadataRepository entityMetadataRepository;

  private final Lock lock = new ReentrantLock();

  public AuthenticationServiceImpl() {
    super();
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.platform.server.auth.AuthenticationService#getIdentity(java.lang.String)
   */
  public void checkCredential(final String credential) {
    lock.lock();
    LOGGER.debug("Init checkCredential process {}", Thread.currentThread().getName());
    try {
      if (!StringUtils.hasText(credential) && enableAnonymousAccess && StringUtils.hasText(anonymousAppClientId)) {
        validateAnonymousAccess();
        final EntityMetadataMessage anonymousMessage = entityMetadataRepository.getEntityMetadataFromId(anonymousAppClientId);
        RequesterContextHolder.setContext(new AnonymousIdentityContext(anonymousMessage));
        return;
      }

      validateCredential(credential);
      RequesterContextHolder.setContext(new RequesterContext(entityMetadataRepository.getEntityMetadataFromToken(credential)));

    } finally {
      LOGGER.debug("Finished checkCredential process {}", Thread.currentThread().getName());
      lock.unlock();
    }
  }

  private void validateAnonymousAccess() {
    if (entityMetadataRepository.getEntityMetadataFromId(anonymousAppClientId) == null) {
      throw new UnauthorizedException("Anonymous access is forbidden.");
    }
  }

  private void validateCredential(final String credential) {
    if (!entityMetadataRepository.containsEntityCredential(credential)) {
      throw new UnauthorizedException("Invalid credential " + credential);
    }
  }
}
