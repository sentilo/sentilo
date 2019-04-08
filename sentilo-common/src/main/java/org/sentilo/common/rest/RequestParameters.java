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
package org.sentilo.common.rest;

import java.util.Date;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

import org.sentilo.common.utils.DateUtils;
import org.springframework.util.CollectionUtils;

public class RequestParameters {

  public static final String FROM = "from";
  public static final String TO = "to";
  public static final String LIMIT = "limit";
  public static final String METHOD = "method";
  public static final String DELETE = "delete";

  private final Map<String, String> values;

  public RequestParameters() {
    values = new LinkedHashMap<String, String>();
  }

  public static RequestParameters buildDelete() {
    final RequestParameters parameters = new RequestParameters();
    parameters.put(METHOD, DELETE);
    return parameters;
  }

  public static RequestParameters build(final Date from, final Date to, final Integer limit) {
    final RequestParameters parameters = new RequestParameters();
    if (from != null) {
      parameters.put(RequestParameters.FROM, from);
    }
    if (to != null) {
      parameters.put(RequestParameters.TO, to);
    }
    if (limit != null) {
      parameters.put(RequestParameters.LIMIT, limit);
    }
    return parameters;
  }

  public void put(final String parameter, final Date date) {
    values.put(parameter, DateUtils.toStringTimestamp(date));
  }

  public void put(final String parameter, final String value) {
    values.put(parameter, value);
  }

  public void put(final String parameter, final Integer value) {
    values.put(parameter, Integer.toString(value));
  }

  public void put(final Map<String, String> parameters) {
    if (!CollectionUtils.isEmpty(parameters)) {
      for (final String key : parameters.keySet()) {
        values.put(key, parameters.get(key));
      }
    }
  }

  public int size() {
    return values.size();
  }

  public Set<String> keySet() {
    return values.keySet();
  }

  public String get(final String key) {
    return values.get(key);
  }
}
