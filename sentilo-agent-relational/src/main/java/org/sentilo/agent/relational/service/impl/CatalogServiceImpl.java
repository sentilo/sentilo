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
package org.sentilo.agent.relational.service.impl;

import java.util.Optional;

import org.sentilo.agent.relational.domain.CatalogAdditionalFields;
import org.sentilo.agent.relational.service.CatalogService;
import org.sentilo.common.cache.LRUCache;
import org.sentilo.common.cache.impl.LRUCacheImpl;
import org.sentilo.common.domain.EventMessage;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.mongodb.core.MongoOperations;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.stereotype.Component;;

@Component
public class CatalogServiceImpl implements CatalogService {

  private static final Logger LOGGER = LoggerFactory.getLogger(CatalogService.class);

  @Autowired
  private MongoOperations mongoOperations;

  private final LRUCache<String, CatalogAdditionalFields> additionalFieldsCache = new LRUCacheImpl<String, CatalogAdditionalFields>(1000);

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.agent.relational.service.CatalogService#getAdditionalFields(org.sentilo.common.
   * domain.EventMessage)
   */
  @Override
  public Optional<CatalogAdditionalFields> getAdditionalFields(final EventMessage eventMessage) {
    final String cacheKey = eventMessage.getProvider() + "#" + eventMessage.getSensor();
    if (!additionalFieldsCache.contains(cacheKey)) {
      final Criteria sensorQueryCriteria = Criteria.where("sensorId").is(eventMessage.getSensor());
      sensorQueryCriteria.andOperator(Criteria.where("providerId").is(eventMessage.getProvider()));
      final Query sensorQuery = new Query(sensorQueryCriteria);
      final CatalogAdditionalFields saf = mongoOperations.findOne(sensorQuery, CatalogAdditionalFields.class, "sensor");

      if (saf != null) {
        final Query componentQuery = Query.query(Criteria.where("_id").is(saf.getComponentId()));
        componentQuery.fields().include("componentType").include("location.centroid").include("name");
        final CatalogAdditionalFields caf = mongoOperations.findOne(componentQuery, CatalogAdditionalFields.class, "component");
        saf.addAll(caf);
        additionalFieldsCache.put(cacheKey, saf);
      } else {
        LOGGER.warn("Not found into Catalog sensor [{}] from provider [{}]", eventMessage.getSensor(), eventMessage.getProvider());
      }
    }

    return additionalFieldsCache.contains(cacheKey) ? Optional.of(additionalFieldsCache.get(cacheKey)) : Optional.empty();
  }

}
