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
package org.sentilo.web.catalog.service.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import org.sentilo.web.catalog.domain.Component;
import org.sentilo.web.catalog.repository.ComponentRepository;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.service.ComponentService;
import org.sentilo.web.catalog.service.SensorService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.data.mongodb.core.query.Update;
import org.springframework.stereotype.Service;

@Service
public class ComponentServiceImpl extends AbstractBaseServiceImpl<Component> implements ComponentService {

  @Autowired
  private ComponentRepository repository;

  @Autowired
  private SensorService sensorService;

  public ComponentServiceImpl() {
    super(Component.class);
  }

  @Override
  public ComponentRepository getRepository() {
    return repository;
  }

  public void setRepository(final ComponentRepository repository) {
    this.repository = repository;
  }

  /*
   * (non-Javadoc)
   * @see org.sentilo.web.catalog.service.impl.AbstractBaseServiceImpl#getEntityId(org.sentilo.web.catalog.domain.CatalogDocument)
   */
  public String getEntityId(final Component entity) {
    return entity.getId();
  }

  /*
   * (non-Javadoc)
   * @see org.sentilo.web.catalog.service.ComponentService#updateMulti(java.util.Collection, java.lang.String, java.lang.Object)
   */
  public void updateMulti(final Collection<String> componentsIds, final String param, final Object value) {
    final Update update = Update.update(param, value);
    getMongoOps().updateMulti(buildQueryForIdInCollection(componentsIds), update, Component.class);
  }

  /*
   * (non-Javadoc)
   * @see org.sentilo.web.catalog.service.impl.AbstractBaseServiceImpl#delete(org.sentilo.web.catalog.domain.CatalogDocument)
   */
  public void delete(final Component entity) {
    final List<Component> components = new ArrayList<Component>();
    components.add(entity);
    delete(components);
  }

  /*
   * (non-Javadoc)
   * @see org.sentilo.web.catalog.service.impl.AbstractBaseServiceImpl#delete(java.util.Collection)
   */
  public void delete(final Collection<Component> entities) {
    final List<String> componentsIds = new ArrayList<String>();
    for (final Component component : entities) {
      componentsIds.add(component.getId());
    }

    deleteComponentsAndChilds(componentsIds);
  }

  /*
   * (non-Javadoc)
   * @see org.sentilo.web.catalog.service.ComponentService#deleteComponents(java.lang.String[])
   */
  public void deleteComponents(final String[] componentsNames) {
    // A la hora de borrar los componentes hay que hacer lo siguiente:
    // 0. Primero recuperamos los ids de los componentes a eliminar
    // 1. Borrar sensores asociados
    // 2. Borrar los componentes
    // 3. Eliminar referencias en componentes que lo tengan como padre
    final List<String> componentsIds = getComponetsIdsFromNames(componentsNames);
    deleteComponentsAndChilds(componentsIds);
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.sentilo.web.catalog.service.ComponentService#findByName(java.lang.String, java.lang.String)
   */
  public Component findByName(final String providerId, final String name) {
    final SearchFilter filter = new SearchFilter();
    filter.addAndParam("providerId", providerId);
    filter.addAndParam("name", name);

    return getMongoOps().findOne(buildQuery(filter), Component.class);
  }

  private List<String> getComponetsIdsFromNames(final String[] componentsNames) {
    final List<String> values = Arrays.asList(componentsNames);
    final Query nameFilter = buildQueryForParamInCollection("name", values);
    final List<Component> components = getMongoOps().find(nameFilter, Component.class);
    final List<String> ids = new ArrayList<String>();

    for (final Component component : components) {
      ids.add(component.getId());
    }

    return ids;
  }

  private void deleteComponentsAndChilds(final List<String> componentsIds) {
    sensorService.deleteSensorsFromComponents(componentsIds);
    disconnectChildComponents(componentsIds);
    deleteComponents(componentsIds);
  }

  private void deleteComponents(final List<String> componentsIds) {
    final Query idsFilter = buildQueryForIdInCollection(componentsIds);
    getMongoOps().remove(idsFilter, Component.class);
  }

  private void disconnectChildComponents(final List<String> componentsIds) {
    final Query idsFilter = buildQueryForParamInCollection("parentId", componentsIds);
    final Update update = Update.update("parentId", null);
    getMongoOps().updateMulti(idsFilter, update, Component.class);
  }
}
