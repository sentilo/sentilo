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
package org.sentilo.platform.server.auth.impl;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import org.sentilo.platform.common.domain.CredentialMessage;
import org.sentilo.platform.common.domain.CredentialsMessage;
import org.sentilo.platform.common.exception.CatalogAccessException;
import org.sentilo.platform.common.service.CatalogService;
import org.sentilo.platform.server.auth.AuthenticationService;
import org.sentilo.platform.server.exception.UnauthorizedException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

@Service
public class AuthenticationServiceImpl implements AuthenticationService {

  private final Logger logger = LoggerFactory.getLogger(AuthorizationServiceImpl.class);

  private final Map<String, String> activeCredentials = new HashMap<String, String>();

  @Autowired
  private CatalogService catalogService;

  private final Lock lock = new ReentrantLock();

  public AuthenticationServiceImpl() {
    super();
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.sentilo.platform.server.auth.AuthenticationService#getIdentity(java.lang.String)
   */
  public String getIdentity(final String credential) throws UnauthorizedException {
    lock.lock();
    logger.debug("Init getIdentity thread {}", Thread.currentThread().getName());
    try {
      validateCredential(credential);
      return activeCredentials.get(credential);
    } finally {
      logger.debug("Finish getIdentity thread {}", Thread.currentThread().getName());
      lock.unlock();
    }
  }

  @Scheduled(initialDelay = 1000, fixedRate = 300000)
  public void loadActiveCredentials() {
    try {
      logger.debug("Actualizando cache de credenciales");
      final CredentialsMessage credentials = catalogService.getCredentials();
      final Map<String, String> auxCredentials = new HashMap<String, String>();
      if (credentials != null && !CollectionUtils.isEmpty(credentials.getCredentials())) {
        for (final CredentialMessage credential : credentials.getCredentials()) {
          auxCredentials.put(credential.getToken(), credential.getEntity());
        }
      }

      replaceActiveCredentials(auxCredentials);

    } catch (final CatalogAccessException e) {
      logger.warn("Error al llamar al catalogo para recuperar la lista de autorizaciones", e);
    }
  }

  private void validateCredential(final String credential) throws UnauthorizedException {
    if (!activeCredentials.containsKey(credential)) {
      throw new UnauthorizedException("Invalid credential " + credential);
    }
  }

  private void replaceActiveCredentials(final Map<String, String> updateCredentials) {
    lock.lock();
    try {
      logger.debug("Replace credenciales thread {}", Thread.currentThread().getName());
      activeCredentials.clear();
      activeCredentials.putAll(updateCredentials);
      logger.debug("Replaced credenciales thread {}", Thread.currentThread().getName());
    } finally {
      lock.unlock();
    }
  }

  public void setCatalogService(final CatalogService catalogService) {
    this.catalogService = catalogService;
  }
}
