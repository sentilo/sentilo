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
import java.util.Set;

import org.sentilo.web.catalog.domain.Component;
import org.sentilo.web.catalog.domain.ComponentType;
import org.sentilo.web.catalog.repository.ComponentTypesRepository;
import org.sentilo.web.catalog.service.ComponentTypesService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

@Service
public class ComponentTypesServiceImpl extends AbstractBaseCrudServiceImpl<ComponentType> implements ComponentTypesService {

  @Autowired
  private ComponentTypesRepository repository;

  public ComponentTypesServiceImpl() {
    super(ComponentType.class);
  }

  @Override
  public ComponentTypesRepository getRepository() {
    return repository;
  }

  public void setRepository(final ComponentTypesRepository repository) {
    this.repository = repository;
  }

  @Override
  public String getEntityId(final ComponentType entity) {
    return entity.getId();
  }

  @Override
  public void insertNewComponentTypesIfNotExists(final Set<String> componentTypes) {
    for (final String type : componentTypes) {
      if (!exists(type)) {
        create(new ComponentType(type, type));
      }
    }
  }

  @Override
  public List<ComponentType> getActiveComponentTypes(final boolean onlyPublics) {
    // Returns componentType for which there is a Component with this type.
    final String componentCollectionName = getMongoOps().getCollectionName(Component.class);
    List<String> componentTypesIds = null;

    if (onlyPublics) {
      final Criteria criteria = Criteria.where("publicAccess").is(Boolean.TRUE);
      final Query query = new Query(criteria);
      componentTypesIds = distinct(componentCollectionName, "componentType", query);
    } else {
      componentTypesIds = distinct(componentCollectionName, "componentType");
    }

    return CollectionUtils.isEmpty(componentTypesIds) ? Collections.<ComponentType>emptyList()
        : getMongoOps().find(buildQueryForIdInCollection(componentTypesIds), ComponentType.class);
  }

  @Override
  public List<ComponentType> findComponentTypesByProvider(final String providerId) {
    if (StringUtils.hasText(providerId)) {
      final String componentCollectionName = getMongoOps().getCollectionName(Component.class);

      // Get unique provider's components types id list
      final Query query = getDistinctComponentByProviderTypeQuery(providerId);
      final List<String> providerComponentTypes = distinct(componentCollectionName, "componentType", query);

      return CollectionUtils.isEmpty(providerComponentTypes) ? Collections.<ComponentType>emptyList()
          : getMongoOps().find(buildQueryForIdInCollection(providerComponentTypes), ComponentType.class);
    } else {
      // If the provider is null, return all component types
      return findAll();
    }
  }

  public Query getDistinctComponentByProviderTypeQuery(final String providerId) {
    final Query query = Query.query(Criteria.where("providerId").is(providerId));
    query.fields().include("componentType");
    query.fields().exclude("_id");
    return query;
  }

}
