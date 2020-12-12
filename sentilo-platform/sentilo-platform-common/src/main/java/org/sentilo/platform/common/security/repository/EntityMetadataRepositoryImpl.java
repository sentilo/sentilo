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
package org.sentilo.platform.common.security.repository;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import org.sentilo.platform.common.domain.EntitiesMetadataMessage;
import org.sentilo.platform.common.domain.EntityMetadataMessage;
import org.sentilo.platform.common.exception.CatalogAccessException;
import org.sentilo.platform.common.service.CatalogService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Repository;
import org.springframework.util.CollectionUtils;

@Repository
public class EntityMetadataRepositoryImpl implements EntityMetadataRepository {

  private static final Logger LOGGER = LoggerFactory.getLogger(EntityMetadataRepositoryImpl.class);

  /**
   * Map where the lookup is done with token, i.e. each entry follows the pattern <entity token,
   * entity metadata>
   */
  private final Map<String, EntityMetadataMessage> tokensEntityMetadataMap = new HashMap<String, EntityMetadataMessage>();
  /**
   * Map where the lookup is done with entityId, , i.e. each entry follows the pattern <entity Id,
   * entity metadata>
   */
  private final Map<String, EntityMetadataMessage> idEntitiesMetadataMap = new HashMap<String, EntityMetadataMessage>();

  @Autowired
  private CatalogService catalogService;

  private final Lock lock = new ReentrantLock();

  @Override
  public EntityMetadataMessage getEntityMetadataFromToken(final String token) {
    return tokensEntityMetadataMap.get(token);
  }

  @Override
  public EntityMetadataMessage getEntityMetadataFromId(final String entityId) {
    return idEntitiesMetadataMap.get(entityId);
  }

  @Override
  public String getTenantOwner(final String entity) {
    return idEntitiesMetadataMap.containsKey(entity) ? idEntitiesMetadataMap.get(entity).getTenantId() : null;
  }

  @Scheduled(initialDelay = 1000, fixedRate = 60000)
  public void loadActiveEntitiesMetadata() {
    try {
      LOGGER.info("Upgrading entity metadata cache");
      final EntitiesMetadataMessage entitiesMetadata = catalogService.getEntitiesMetadata();
      final Map<String, EntityMetadataMessage> auxTokensEntityMetadataMap = new HashMap<String, EntityMetadataMessage>();
      final Map<String, EntityMetadataMessage> auxIdEntitiesMetadataMap = new HashMap<String, EntityMetadataMessage>();
      if (entitiesMetadata != null && !CollectionUtils.isEmpty(entitiesMetadata.getEntitiesMetadata())) {
        for (final EntityMetadataMessage entityMetadata : entitiesMetadata.getEntitiesMetadata()) {
          auxTokensEntityMetadataMap.put(entityMetadata.getToken(), entityMetadata);
          auxIdEntitiesMetadataMap.put(entityMetadata.getEntity(), entityMetadata);
        }
      }

      replaceActiveEntitiesMetadata(auxTokensEntityMetadataMap, auxIdEntitiesMetadataMap);
      LOGGER.info("Process finished successfully");
    } catch (final CatalogAccessException e) {
      LOGGER.warn("Error while processing sync request with Catalog to get the entity metadata list", e);
    }
  }

  public boolean containsEntityCredential(final String token) {
    return tokensEntityMetadataMap.containsKey(token);
  }

  private void replaceActiveEntitiesMetadata(final Map<String, EntityMetadataMessage> updatedTokensEntityMetadataMap,
      final Map<String, EntityMetadataMessage> updatedIdEntitiesMetadataMap) {
    lock.lock();
    try {
      LOGGER.debug("Replace current entities metadata. Thread {}", Thread.currentThread().getName());
      tokensEntityMetadataMap.clear();
      idEntitiesMetadataMap.clear();
      tokensEntityMetadataMap.putAll(updatedTokensEntityMetadataMap);
      idEntitiesMetadataMap.putAll(updatedIdEntitiesMetadataMap);
      LOGGER.debug("Replaced current entities metadata. Thread {}", Thread.currentThread().getName());
    } finally {
      lock.unlock();
    }
  }

}
