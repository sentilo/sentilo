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
package org.sentilo.web.catalog.service.impl;

import java.awt.Component;
import java.util.Collections;
import java.util.List;
import java.util.Set;

import org.sentilo.web.catalog.domain.ComponentType;
import org.sentilo.web.catalog.repository.ComponentTypesRepository;
import org.sentilo.web.catalog.service.ComponentTypesService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

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
      if (find(new ComponentType(type)) == null) {
        create(new ComponentType(type, type));
      }
    }
  }

  @SuppressWarnings("unchecked")
  @Override
  public List<ComponentType> getActiveComponentTypes(final boolean onlyPublics) {

    final String componentCollectionName = getMongoOps().getCollectionName(Component.class);
    List<String> componentTypesIds = null;

    if (onlyPublics) {
      final Criteria criteria = Criteria.where("publicAccess").is(Boolean.TRUE);
      final Query query = new Query(criteria);
      componentTypesIds = getMongoOps().getCollection(componentCollectionName).distinct("componentType", query.getQueryObject());
    } else {
      componentTypesIds = getMongoOps().getCollection(componentCollectionName).distinct("componentType");
    }

    return (CollectionUtils.isEmpty(componentTypesIds) ? Collections.<ComponentType>emptyList() : getMongoOps().find(
        buildQueryForIdInCollection(componentTypesIds), ComponentType.class));
  }
}
