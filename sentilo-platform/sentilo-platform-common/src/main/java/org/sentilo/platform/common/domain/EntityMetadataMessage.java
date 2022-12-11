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
package org.sentilo.platform.common.domain;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

public class EntityMetadataMessage {

  @JsonInclude(value = Include.NON_NULL)
  private String entity;
  @JsonInclude(value = Include.NON_NULL)
  private boolean active;
  @JsonInclude(value = Include.NON_NULL)
  private String token;
  @JsonInclude(value = Include.NON_NULL)
  private String tenantId;
  @JsonInclude(value = Include.NON_NULL)
  private boolean restHttps;
  @JsonInclude(value = Include.NON_NULL)
  private long apiInputQuota;
  @JsonInclude(value = Include.NON_NULL)
  private long apiOutputQuota;

  public void setToken(final String token) {
    this.token = token;
  }

  public String getToken() {
    return token;
  }

  public void setEntity(final String entity) {
    this.entity = entity;
  }

  public String getEntity() {
    return entity;
  }

  public String getTenantId() {
    return tenantId;
  }

  public void setTenantId(final String tenantId) {
    this.tenantId = tenantId;
  }

  public boolean isRestHttps() {
    return restHttps;
  }

  public void setRestHttps(final boolean restHttps) {
    this.restHttps = restHttps;
  }

  public long getApiInputQuota() {
    return apiInputQuota;
  }

  public void setApiInputQuota(final long apiInputQuota) {
    this.apiInputQuota = apiInputQuota;
  }

  public long getApiOutputQuota() {
    return apiOutputQuota;
  }

  public void setApiOutputQuota(final long apiOutputQuota) {
    this.apiOutputQuota = apiOutputQuota;
  }

  public boolean isActive() {
    return active;
  }

  public void setActive(final boolean active) {
    this.active = active;
  }

}
