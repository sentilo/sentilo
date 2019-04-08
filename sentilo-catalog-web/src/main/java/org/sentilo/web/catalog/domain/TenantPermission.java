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
package org.sentilo.web.catalog.domain;

import java.util.Date;

import org.sentilo.web.catalog.utils.Constants;
import org.sentilo.web.catalog.utils.enums.EntityType;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;

import com.fasterxml.jackson.annotation.JsonIgnore;

@Document
public class TenantPermission implements CatalogDocument {

  private static final long serialVersionUID = -6452111945014928465L;

  @Id
  @JsonIgnore
  private String id;

  private String source;

  private String entity;

  private String target;

  private Type type;

  private EntityType entityType;

  @JsonIgnore
  private Date createdAt;

  @JsonIgnore
  private Date updatedAt;

  private Boolean visible;

  private Boolean listVisible;

  private String createdBy;

  private String updatedBy;

  @JsonIgnore
  private Direction direction;

  public enum Type {
    READ, WRITE;
  }

  public enum Direction {
    FROM, TO
  }

  public TenantPermission() {
    super();
  }

  public TenantPermission(final String id) {
    this.id = id;
  }

  public TenantPermission(final String source, final String entity, final String target, final Type type, final EntityType entityType,
      final boolean visible, final boolean listVisible) {
    this.source = source;
    this.entity = entity;
    this.target = target;
    this.type = type;
    this.entityType = entityType;
    this.visible = visible;
    this.listVisible = listVisible;
    buildId();
  }

  @Override
  public String getId() {
    return id;
  }

  public void setId(final String id) {
    this.id = id;
  }

  public String getSource() {
    return source;
  }

  public void setSource(final String source) {
    this.source = source;
  }

  public String getEntity() {
    return entity;
  }

  public void setEntity(final String entity) {
    this.entity = entity;
  }

  public String getTarget() {
    return target;
  }

  public void setTarget(final String target) {
    this.target = target;
  }

  public Type getType() {
    return type;
  }

  public void setType(final Type type) {
    this.type = type;
  }

  public EntityType getEntityType() {
    return entityType;
  }

  public void setEntityType(final EntityType entityType) {
    this.entityType = entityType;
  }

  @Override
  public Date getCreatedAt() {
    return createdAt;
  }

  @Override
  public void setCreatedAt(final Date createdAt) {
    this.createdAt = createdAt;
  }

  @Override
  public Date getUpdatedAt() {
    return updatedAt;
  }

  @Override
  public void setUpdatedAt(final Date updatedAt) {
    this.updatedAt = updatedAt;
  }

  public Boolean getVisible() {
    return visible;
  }

  public void setVisible(final Boolean visible) {
    this.visible = visible;
  }

  public Boolean getListVisible() {
    return listVisible;
  }

  public void setListVisible(final Boolean listVisible) {
    this.listVisible = listVisible;
  }

  @Override
  public String getCreatedBy() {
    return createdBy;
  }

  @Override
  public void setCreatedBy(final String createdBy) {
    this.createdBy = createdBy;
  }

  @Override
  public String getUpdatedBy() {
    return updatedBy;
  }

  @Override
  public void setUpdatedBy(final String updatedBy) {
    this.updatedBy = updatedBy;
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + (entity == null ? 0 : entity.hashCode());
    result = prime * result + (source == null ? 0 : source.hashCode());
    result = prime * result + (target == null ? 0 : target.hashCode());
    return result;
  }

  @Override
  public boolean equals(final Object obj) {
    if (this == obj) {
      return true;
    }
    if (obj == null) {
      return false;
    }
    if (getClass() != obj.getClass()) {
      return false;
    }
    final TenantPermission other = (TenantPermission) obj;
    if (entity == null) {
      if (other.entity != null) {
        return false;
      }
    } else if (!entity.equals(other.entity)) {
      return false;
    }
    if (source == null) {
      if (other.source != null) {
        return false;
      }
    } else if (!source.equals(other.source)) {
      return false;
    }
    if (target == null) {
      if (other.target != null) {
        return false;
      }
    } else if (!target.equals(other.target)) {
      return false;
    }
    return true;
  }

  private void buildId() {
    // Internal id property has the following format source@entity@target, identifies the entity and
    // the sorce and destination tenants that performs the permission
    id = source + Constants.PERMISSION_TOKEN_SPLITTER + entity + Constants.PERMISSION_TOKEN_SPLITTER + target;
  }
}
