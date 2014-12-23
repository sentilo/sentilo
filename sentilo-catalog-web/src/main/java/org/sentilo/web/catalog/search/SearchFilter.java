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
package org.sentilo.web.catalog.search;

import java.util.HashMap;
import java.util.Map;

import org.springframework.data.domain.Pageable;
import org.springframework.util.Assert;

public class SearchFilter {

  private Pageable pageable;
  private Map<String, Object> params = new HashMap<String, Object>();
  private final Map<String, Object> andParams = new HashMap<String, Object>();
  private String[] mapBounds;

  public SearchFilter() {
    super();
  }

  public SearchFilter(final Pageable pageable) {
    this();
    this.pageable = pageable;
  }

  public SearchFilter(final Map<String, Object> params) {
    this();
    Assert.notNull(params);
    this.params = params;
  }

  public SearchFilter(final Map<String, Object> params, final Pageable pageable) {
    this();
    Assert.notNull(params);
    this.params = params;
    this.pageable = pageable;
  }

  public void addParam(final String name, final Object value) {
    params.put(name, value);
  }

  public void addAndParam(final String name, final Object value) {
    andParams.put(name, value);
  }

  public boolean paramsIsEmpty() {
    return params.isEmpty();
  }

  public boolean andParamsIsEmpty() {
    return andParams.isEmpty();
  }

  public Pageable getPageable() {
    return pageable;
  }

  public Map<String, Object> getParams() {
    return params;
  }

  public Map<String, Object> getAndParams() {
    return andParams;
  }

  public void setMapBounds(final String[] bounds) {
    mapBounds = bounds;
  }

  public String[] getBounds() {
    return mapBounds;
  }
}
