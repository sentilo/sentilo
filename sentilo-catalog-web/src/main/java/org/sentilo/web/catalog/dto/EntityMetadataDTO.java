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

public class EntityMetadataDTO {

  private String entity;
  private String token;
  private String tenantId;
  private boolean restHttps;
  private int apiInputQuota;
  private int apiOutputQuota;

  protected EntityMetadataDTO(final String entity, final String token) {
    this.entity = entity;
    this.token = token;
  }

  public EntityMetadataDTO(final String entity, final String token, final String tenantId, final boolean restHttps, final int apiInputQuota,
      final int apiOutputQuota) {
    this(entity, token);
    this.tenantId = tenantId;
    this.restHttps = restHttps;
    this.apiInputQuota = apiInputQuota;
    this.apiOutputQuota = apiOutputQuota;
  }

  @Override
  public boolean equals(final Object obj) {
    if (!(obj instanceof EntityMetadataDTO)) {
      return false;
    }
    final EntityMetadataDTO other = (EntityMetadataDTO) obj;
    return entity.equals(other.entity);
  }

  @Override
  public int hashCode() {
    // Hashcode return must be consistent with the equals method
    final int prime = 73;
    int result = 1;
    result = prime * result + (entity == null ? 0 : entity.hashCode());
    return result;
  }

  public String getEntity() {
    return entity;
  }

  public void setEntity(final String entity) {
    this.entity = entity;
  }

  public String getToken() {
    return token;
  }

  public void setToken(final String token) {
    this.token = token;
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

  public int getApiInputQuota() {
    return apiInputQuota;
  }

  public void setApiInputQuota(final int apiInputQuota) {
    this.apiInputQuota = apiInputQuota;
  }

  public int getApiOutputQuota() {
    return apiOutputQuota;
  }

  public void setApiOutputQuota(final int apiOutputQuota) {
    this.apiOutputQuota = apiOutputQuota;
  }

}
