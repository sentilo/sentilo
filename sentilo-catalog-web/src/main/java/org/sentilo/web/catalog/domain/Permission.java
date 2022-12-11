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
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;

import com.fasterxml.jackson.annotation.JsonIgnore;

@Document
public class Permission implements CatalogDocument {

  private static final long serialVersionUID = 1L;

  @Id
  @JsonIgnore
  private String id;
  private String source;
  private String target;
  private Type type;
  @JsonIgnore
  private Date createdAt;
  @JsonIgnore
  private Date updatedAt;
  @JsonIgnore
  private String createdBy;
  @JsonIgnore
  private String updatedBy;

  public enum Type {
    READ, WRITE, ADMIN;
  }

  public Permission() {
  }

  public Permission(final String source) {
    this(source, source, Type.ADMIN);
  }

  public Permission(final String source, final String target, final Type type) {
    this.source = source;
    this.target = target;
    this.type = type;
    buildId();

  }

  private void buildId() {
    // Internal ID_PROP has the following format source@target
    id = source + Constants.PERMISSION_TOKEN_SPLITTER + target;
  }

  @Override
  public boolean equals(final Object obj) {

    if (!(obj instanceof Permission)) {
      return false;
    }
    final Permission other = (Permission) obj;
    return source.equals(other.source) && target.equals(other.target);
  }

  @Override
  public int hashCode() {
    // Hashcode return must be consistent with the equals method
    final int prime = 37;
    int result = 1;
    result = prime * result + (source == null ? 0 : source.hashCode());
    result = prime * result + (target == null ? 0 : target.hashCode());
    return result;
  }

  @Override
  public String toString() {
    return String.format("Permission. Source: %s Target: %s Type: %s", source, target, type);
  }

  public String getSource() {
    return source;
  }

  public void setSource(final String source) {
    this.source = source;
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

  public void setId(final String id) {
    this.id = id;
  }

  @Override
  public String getId() {
    return id;
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

}
