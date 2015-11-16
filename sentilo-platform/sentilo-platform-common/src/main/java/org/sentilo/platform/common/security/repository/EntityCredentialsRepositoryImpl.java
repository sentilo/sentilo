/*
 * Sentilo
 *  
 * Original version 1.4 Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de Barcelona.
 * Modified by Opentrends adding support for multitenant deployments and SaaS. Modifications on version 1.5 Copyright (C) 2015 Opentrends Solucions i Sistemes, S.L.
 * 
 *   
 * This program is licensed and may be used, modified and redistributed under the
 * terms  of the European Public License (EUPL), either version 1.1 or (at your 
 * option) any later version as soon as they are approved by the European 
 * Commission.
 *   
 * Alternatively, you may redistribute and/or modify this program under the terms
 * of the GNU Lesser General Public License as published by the Free Software 
 * Foundation; either  version 3 of the License, or (at your option) any later 
 * version. 
 *   
 * Unless required by applicable law or agreed to in writing, software distributed
 * under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR 
 * CONDITIONS OF ANY KIND, either express or implied. 
 *   
 * See the licenses for the specific language governing permissions, limitations 
 * and more details.
 *   
 * You should have received a copy of the EUPL1.1 and the LGPLv3 licenses along 
 * with this program; if not, you may find them at: 
 *   
 *   https://joinup.ec.europa.eu/software/page/eupl/licence-eupl
 *   http://www.gnu.org/licenses/ 
 *   and 
 *   https://www.gnu.org/licenses/lgpl.txt
 */
package org.sentilo.platform.common.security.repository;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import org.sentilo.platform.common.domain.CredentialMessage;
import org.sentilo.platform.common.domain.CredentialsMessage;
import org.sentilo.platform.common.exception.CatalogAccessException;
import org.sentilo.platform.common.service.CatalogService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Repository;
import org.springframework.util.CollectionUtils;

@Repository
public class EntityCredentialsRepositoryImpl implements EntityCredentialsRepository {

  private static final Logger LOGGER = LoggerFactory.getLogger(EntityCredentialsRepositoryImpl.class);

  /**
   * Credentials map where the lookup is done with token, i.e. each entry follows the pattern
   * <credential token, credential>
   */
  private final Map<String, CredentialMessage> currentCredentials = new HashMap<String, CredentialMessage>();
  /**
   * Credentials map where the lookup is done with entityId, , i.e. each entry follows the pattern
   * <credential entityId, credential>
   */
  private final Map<String, CredentialMessage> entitiesCredentials = new HashMap<String, CredentialMessage>();

  @Autowired
  private CatalogService catalogService;

  private final Lock lock = new ReentrantLock();

  @Override
  public CredentialMessage getCredentials(final String token) {
    return currentCredentials.get(token);
  }

  @Override
  public String getTenant(final String entity) {
    return (entitiesCredentials.containsKey(entity) ? entitiesCredentials.get(entity).getTenantId() : null);
  }

  @Scheduled(initialDelay = 1000, fixedRate = 300000)
  public void loadActiveCredentials() {
    try {
      LOGGER.debug("Upgrading credentials cache");
      final CredentialsMessage credentials = catalogService.getCredentials();
      final Map<String, CredentialMessage> auxCredentials = new HashMap<String, CredentialMessage>();
      final Map<String, CredentialMessage> auxEntitiesCredentials = new HashMap<String, CredentialMessage>();
      if (credentials != null && !CollectionUtils.isEmpty(credentials.getCredentials())) {
        for (final CredentialMessage credential : credentials.getCredentials()) {
          auxCredentials.put(credential.getToken(), credential);
          auxEntitiesCredentials.put(credential.getEntity(), credential);
        }
      }

      replaceActiveCredentials(auxCredentials, auxEntitiesCredentials);

    } catch (final CatalogAccessException e) {
      LOGGER.warn("Error while processing sync request with Catalog to get the credentials list", e);
    }
  }

  public boolean containsCredential(final String token) {
    return currentCredentials.containsKey(token);
  }

  private void replaceActiveCredentials(final Map<String, CredentialMessage> updatedCurrentCredentials,
      final Map<String, CredentialMessage> updatedEntitiesCredentials) {
    lock.lock();
    try {
      LOGGER.debug("Replace current credentials. Thread {}", Thread.currentThread().getName());
      currentCredentials.clear();
      entitiesCredentials.clear();
      currentCredentials.putAll(updatedCurrentCredentials);
      entitiesCredentials.putAll(updatedEntitiesCredentials);
      LOGGER.debug("Replaced current credentials. Thread {}", Thread.currentThread().getName());
    } finally {
      lock.unlock();
    }
  }

}
