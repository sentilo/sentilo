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
package org.sentilo.agent.activity.monitor.service.impl;

import org.sentilo.agent.activity.monitor.domain.CatalogAdditionalFields;
import org.sentilo.agent.activity.monitor.service.CatalogService;
import org.sentilo.common.cache.LRUCache;
import org.sentilo.common.cache.impl.LRUCacheImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.mongodb.core.MongoOperations;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.stereotype.Component;

@Component
public class CatalogServiceImpl implements CatalogService {

  @Autowired
  private MongoOperations mongoOperations;

  private LRUCache<String, String> sensorTypesCache = new LRUCacheImpl<String, String>(1000);
  private LRUCache<String, CatalogAdditionalFields> alertAdditionalFieldsCache = new LRUCacheImpl<String, CatalogAdditionalFields>(1000);

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.agent.activity.monitor.service.CatalogService#getSensorType(java.lang.String,
   * java.lang.String)
   */
  public String getSensorType(final String provider, final String sensor) {
    final String cacheKey = provider + "#" + sensor;
    if (sensorTypesCache.get(cacheKey) == null) {
      final Criteria queryCriteria = Criteria.where("sensorId").is(sensor);
      queryCriteria.andOperator(Criteria.where("providerId").is(provider));

      final Query query = new Query(queryCriteria);
      query.fields().include("type");

      sensorTypesCache.put(cacheKey, mongoOperations.findOne(query, CatalogAdditionalFields.class, "sensor").getType());
    }

    return sensorTypesCache.get(cacheKey);
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.agent.activity.monitor.service.CatalogService#getAlarmLocation(java.lang.String)
   */
  public CatalogAdditionalFields getAdditionalFields(final String alertId) {
    CatalogAdditionalFields additionalFields = alertAdditionalFieldsCache.get(alertId);

    if (additionalFields == null) {
      // The first step is to retrieve the component associated with alert {alertId}
      Query query = Query.query(Criteria.where("_id").is(alertId));
      query.fields().include("componentId");
      query.fields().exclude("_id");
      final CatalogAdditionalFields alertFields = mongoOperations.findOne(query, CatalogAdditionalFields.class, "alert");
      // Later, retrieve the component location and its type (mobile or static)
      if (alertFields != null) {
        query = Query.query(Criteria.where("_id").is(alertFields.getComponentId()));
        query.fields().include("location.centroid");
        query.fields().exclude("_id");
        query.fields().include("mobile");
        final CatalogAdditionalFields componentFields = mongoOperations.findOne(query, CatalogAdditionalFields.class, "component");

        if (componentFields.getMobile() == 0) {
          componentFields.setComponentId(alertFields.getComponentId());
          // If component is static (i.e mobile == 0), then componentFields is cached.
          alertAdditionalFieldsCache.put(alertId, componentFields);
          additionalFields = componentFields;
        }
      }
    }

    return additionalFields;
  }

}
