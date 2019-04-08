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
package org.sentilo.web.catalog.service.impl;

import java.util.Collections;
import java.util.List;

import org.sentilo.web.catalog.domain.Sensor;
import org.sentilo.web.catalog.domain.SensorType;
import org.sentilo.web.catalog.repository.SensorTypesRepository;
import org.sentilo.web.catalog.service.SensorTypesService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

@Service
public class SensorTypesServiceImpl extends AbstractBaseCrudServiceImpl<SensorType> implements SensorTypesService {

  @Autowired
  private SensorTypesRepository repository;

  public SensorTypesServiceImpl() {
    super(SensorType.class);
  }

  @Override
  public SensorTypesRepository getRepository() {
    return repository;
  }

  public void setRepository(final SensorTypesRepository repository) {
    this.repository = repository;
  }

  @Override
  public String getEntityId(final SensorType entity) {
    return entity.getId();
  }

  @Override
  public List<SensorType> findSensorTypesByProvider(final String providerId) {
    if (StringUtils.hasText(providerId)) {
      final String sensorCollectionName = getMongoOps().getCollectionName(Sensor.class);
      final Query query = getDistinctSensorByProviderTypeQuery(providerId);

      // Get uniques provider's components types id list
      final List<String> providerSensorTypes = distinct(sensorCollectionName, "type", query);

      return CollectionUtils.isEmpty(providerSensorTypes) ? Collections.<SensorType>emptyList()
          : getMongoOps().find(buildQueryForIdInCollection(providerSensorTypes), SensorType.class);
    } else {
      // If the provider is null, return all component types
      return findAll();
    }
  }

  public Query getDistinctSensorByProviderTypeQuery(final String providerId) {
    final Query query = Query.query(Criteria.where("providerId").is(providerId));
    query.fields().include("type");
    query.fields().exclude("_id");
    return query;
  }
}
