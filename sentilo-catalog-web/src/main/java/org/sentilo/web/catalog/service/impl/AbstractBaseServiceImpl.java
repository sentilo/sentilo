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

import java.util.Arrays;
import java.util.Collection;
import java.util.Map;
import java.util.Set;

import org.sentilo.web.catalog.domain.TenantResource;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.utils.TenantUtils;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

public abstract class AbstractBaseServiceImpl {

  protected Query buildQuery(final SearchFilter filter) {
    return buildQuery(filter, true);
  }

  protected Query buildCountQuery() {
    return buildQuery(new SearchFilter(), false);
  }

  protected Query buildCountQuery(final SearchFilter filter) {
    return buildQuery(filter, false);
  }

  protected Query buildQuery(final SearchFilter filter, final boolean pageable) {
    return buildQuery(filter, pageable, null);
  }

  protected Query buildQuery(final SearchFilter filter, final boolean pageable, final Criteria customCriteria) {

    Criteria queryCriteria = new Criteria();

    // If customCriteria is not null, initialize queryCriteria with this criteria
    if (customCriteria != null) {
      queryCriteria = customCriteria;
    }

    if (!filter.andParamsIsEmpty()) {
      final Criteria[] aCriteria = buildFilterParamsCriteria(filter.getAndParams());
      queryCriteria = queryCriteria.andOperator(aCriteria);
    }

    if (!filter.paramsIsEmpty()) {
      final Criteria[] aCriteria = buildOrParamsCriteria(filter.getParams());
      queryCriteria = queryCriteria.orOperator(aCriteria);
    }

    if (!filter.norParamsIsEmpty()) {
      final Criteria[] aCriteria = buildFilterParamsCriteria(filter.getNorParams());
      queryCriteria = queryCriteria.norOperator(aCriteria);
    }

    final Query query = new Query(queryCriteria);

    if (!filter.includeFieldsIsEmpty()) {
      for (final String field : filter.getIncludeFields()) {
        query.fields().include(field);
      }
    }

    if (!filter.excludeFieldsIsEmpty()) {
      for (final String field : filter.getExcludeFields()) {
        query.fields().exclude(field);
      }
    }

    if (pageable) {
      query.with(filter.getPageable());
    }

    return query;
  }

  protected Criteria[] buildFilterParamsCriteria(final Map<String, Object> filterParams) {
    // filterParams contiene la lista de filtros a aplicar en modo conjuncion, es decir, con el
    // operador AND
    // Además, en función del tipo de valor (colección o simple) la comparativa del valor siempre
    // será o bien mediante "es exactamente este valor", es decir, se debe comportar como un EQUALS,
    // o bien mediante "está dentro del conjunto {...}".

    // filterParams admite valores NULL
    final Set<String> filterParamsKeys = filterParams.keySet();
    final Criteria[] aCriteria = new Criteria[filterParamsKeys.size()];
    int i = 0;
    for (final String filterParam : filterParamsKeys) {
      if (isCollectionValue(filterParams.get(filterParam))) {
        aCriteria[i] = Criteria.where(filterParam).in(getCollectionValue(filterParams.get(filterParam)));
      } else {
        aCriteria[i] = Criteria.where(filterParam).is(filterParams.get(filterParam));
      }

      i++;
    }

    return aCriteria;
  }

  protected Criteria[] buildOrParamsCriteria(final Map<String, Object> orParams) {
    // params contiene la lista de filtros a aplicar en modo disjuncion, es decir, con el operador
    // OR
    // Además, la comparativa del valor siempre es mediante "contiene la palabra buscada", es
    // decir, se debe comportar como un LIKE %value% en SQL
    final Set<String> paramsKeys = orParams.keySet();
    final Criteria[] aCriteria = new Criteria[paramsKeys.size()];
    int i = 0;
    for (final String param : paramsKeys) {
      if (isCollectionValue(orParams.get(param))) {
        aCriteria[i] = Criteria.where(param).in(getCollectionValue(orParams.get(param)));
      } else if (!(orParams.get(param) instanceof String)) {
        aCriteria[i] = Criteria.where(param).is(orParams.get(param));
      } else {
        final String regexp = ".*" + orParams.get(param) + ".*";
        aCriteria[i] = Criteria.where(param).regex(regexp);
      }
      i++;
    }

    return aCriteria;
  }

  protected Query buildQuery(final String paramName, final Collection<String> values, final Map<String, Object> filterParams) {
    Criteria queryCriteria = Criteria.where(paramName).in(values);
    if (!CollectionUtils.isEmpty(filterParams)) {
      final Criteria[] aCriteria = buildFilterParamsCriteria(filterParams);
      queryCriteria = queryCriteria.andOperator(aCriteria);
    }

    return new Query(queryCriteria);
  }

  protected Query buildQueryForIdInCollection(final Collection<String> values) {
    return buildQueryForParamInCollection("id", values);
  }

  protected Query buildQueryForParamInCollection(final String paramName, final Collection<String> values) {
    final Criteria queryCriteria = Criteria.where(paramName).in(values);
    return new Query(queryCriteria);
  }

  protected boolean applyFilterByTenant(final Class<?> type) {
    try {
      return TenantResource.class.isAssignableFrom(type) && StringUtils.hasText(TenantUtils.getCurrentTenant());
    } catch (final Exception e) {
      return false;
    }
  }

  /**
   * Determines if <code>value</code> represents a collection value, either an array or a collection
   * class
   *
   * @param value
   * @return
   */
  private boolean isCollectionValue(final Object value) {
    return value != null && (value.getClass().isArray() || Collection.class.isAssignableFrom(value.getClass()));
  }

  private Collection<?> getCollectionValue(final Object value) {
    if (value.getClass().isArray()) {
      return Arrays.asList((Object[]) value);
    } else {
      return (Collection<?>) value;
    }
  }

}
