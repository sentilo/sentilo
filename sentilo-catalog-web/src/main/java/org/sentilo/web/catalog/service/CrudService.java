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
package org.sentilo.web.catalog.service;

import java.util.Collection;
import java.util.List;

import org.sentilo.web.catalog.domain.CatalogDocument;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.search.SearchFilterResult;

public interface CrudService<T> {

  T create(T entity);

  T update(T entity);

  Collection<T> insertAll(Collection<T> entities);

  Collection<T> updateAll(Collection<T> entities);

  void delete(T entity);

  void delete(Collection<T> entities);

  void delete(SearchFilter filter);

  <V extends CatalogDocument> void delete(SearchFilter filter, Class<V> resourceType);

  T find(T entity);

  T findById(String id);

  List<T> findAll();

  SearchFilterResult<T> search(SearchFilter filter);

  Long count();

  T findAndThrowErrorIfNotExist(T entity);

  boolean exists(String entityId);

  void updateMulti(final Collection<String> objectIds, final String param, final Object value);

  <V> void updateMulti(final Collection<String> objectIds, final List<String> params, final List<V> values);
}
