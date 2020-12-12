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
package org.sentilo.agent.activity.monitor.repository.batch.support;

import java.util.Map;

import org.springframework.util.CollectionUtils;

public class Item {

  /**
   * For ES v2, response for the op_type "index" is create and in newer versions it's index
   */
  private Map<String, Object> create;
  private Map<String, Object> index;

  public Item() {
    super();
  }

  public Integer getStatus() {
    Integer status = null;
    if (!CollectionUtils.isEmpty(create) || !CollectionUtils.isEmpty(index)) {
      status = CollectionUtils.isEmpty(create) ? (Integer) getIndex().get("status") : (Integer) getCreate().get("status");
    }
    return status;
  }

  public Map<String, Object> getIndex() {
    return index;
  }

  public void setIndex(final Map<String, Object> index) {
    this.index = index;
  }

  public Map<String, Object> getCreate() {
    return create;
  }

  public void setCreate(final Map<String, Object> create) {
    this.create = create;
  }
}
