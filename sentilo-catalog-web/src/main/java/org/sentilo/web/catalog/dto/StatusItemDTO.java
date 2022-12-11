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
package org.sentilo.web.catalog.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

public class StatusItemDTO implements Comparable<StatusItemDTO> {

  private transient int order;
  @JsonInclude(value = Include.NON_NULL)
  private String id;
  @JsonInclude(value = Include.NON_NULL)
  private String name;
  @JsonInclude(value = Include.NON_NULL)
  private String description;
  @JsonInclude(value = Include.NON_NULL)
  private boolean status;
  @JsonInclude(value = Include.NON_NULL)
  private String stateDesc;

  public StatusItemDTO(final int order, final String name, final String description) {
    this.order = order;
    id = "sentilo-component-" + order;
    this.name = name;
    this.description = description;
  }

  public StatusItemDTO(final int order, final String name, final String description, final boolean status) {
    this(order, name, description);
    this.status = status;
  }

  public StatusItemDTO(final int order, final String name, final String description, final boolean status, final String stateDesc) {
    this(order, name, description, status);
    this.stateDesc = stateDesc;
  }

  @Override
  public int compareTo(final StatusItemDTO obj) {
    return Integer.valueOf(order).compareTo(Integer.valueOf(obj.getOrder()));
  }

  public String getName() {
    return name;
  }

  public void setName(final String name) {
    this.name = name;
  }

  public boolean getStatus() {
    return status;
  }

  public void setStatus(final boolean status) {
    this.status = status;
  }

  public String getStateDesc() {
    return stateDesc;
  }

  public void setStateDesc(final String stateDesc) {
    this.stateDesc = stateDesc;
  }

  public int getOrder() {
    return order;
  }

  public void setOrder(final int order) {
    this.order = order;
  }

  public String getDescription() {
    return description;
  }

  public void setDescription(final String description) {
    this.description = description;
  }

  public String getId() {
    return id;
  }

}
