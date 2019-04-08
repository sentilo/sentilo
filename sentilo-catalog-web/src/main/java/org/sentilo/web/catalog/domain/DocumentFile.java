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
import java.util.HashSet;
import java.util.Set;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.Pattern;

import org.sentilo.web.catalog.utils.CompoundKeyBuilder;
import org.sentilo.web.catalog.utils.Constants;
import org.springframework.data.annotation.Id;
import org.springframework.data.annotation.Transient;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.util.StringUtils;
import org.springframework.web.multipart.MultipartFile;

public class DocumentFile implements CatalogDocument, TenantResource, EntityResource {

  private static final long serialVersionUID = 6553719639654955139L;

  @Id
  private String id;

  @NotBlank
  @Pattern(regexp = Constants.VALIDATION_FILENAME_REGEXP)
  private String name;

  @NotBlank
  private String description;

  @Transient
  private MultipartFile file;

  /* Metadata file fields which are derived from the multipart attribute */
  private String filename;

  private String contentType;

  private byte[] content;

  private long filesize;

  private String tenantId;

  @NotBlank
  private String entityId;

  @DateTimeFormat(pattern = Constants.DATETIME_FORMAT)
  private Date createdAt;

  private String createdBy;

  @DateTimeFormat(pattern = Constants.DATETIME_FORMAT)
  private Date updatedAt;

  private String updatedBy;

  private Set<String> tenantsAuth;

  private Set<String> tenantsListVisible;

  public DocumentFile() {
    tenantsAuth = new HashSet<String>();
    tenantsListVisible = new HashSet<String>();
  }

  public DocumentFile(final String id) {
    this();
    this.id = id;
  }

  public DocumentFile(final String entityId, final String filename) {
    this();
    this.filename = filename;
    this.entityId = entityId;
  }

  public static String buildId(final String resourceId, final String name) {
    return CompoundKeyBuilder.buildCompoundKey(resourceId, name);
  }

  @Override
  public String getId() {
    if (!StringUtils.hasText(id) && StringUtils.hasText(filename) && StringUtils.hasText(entityId)) {
      id = buildId(entityId, filename);
    }
    return id;
  }

  public void setId(final String id) {
    this.id = id;
  }

  public String getName() {
    return name;
  }

  public void setName(final String name) {
    this.name = name;
  }

  public String getFilename() {
    return filename;
  }

  public void setFilename(final String filename) {
    this.filename = filename;
  }

  public String getDescription() {
    return description;
  }

  public void setDescription(final String description) {
    this.description = description;
  }

  public String getContentType() {
    return contentType;
  }

  public void setContentType(final String contentType) {
    this.contentType = contentType;
  }

  public MultipartFile getFile() {
    return file;
  }

  public void setFile(final MultipartFile file) {
    this.file = file;
  }

  public long getFilesize() {
    return filesize;
  }

  public void setFilesize(final long filesize) {
    this.filesize = filesize;
  }

  @Override
  public String getTenantId() {
    return tenantId;
  }

  @Override
  public void setTenantId(final String tenantId) {
    this.tenantId = tenantId;
  }

  public String getEntityId() {
    return entityId;
  }

  public void setEntityId(final String entityId) {
    this.entityId = entityId;
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
  public String getCreatedBy() {
    return createdBy;
  }

  @Override
  public void setCreatedBy(final String createdBy) {
    this.createdBy = createdBy;
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
  public String getUpdatedBy() {
    return updatedBy;
  }

  @Override
  public void setUpdatedBy(final String updatedBy) {
    this.updatedBy = updatedBy;
  }

  @Override
  public Set<String> getTenantsAuth() {
    return tenantsAuth;
  }

  @Override
  public void setTenantsAuth(final Set<String> tenantsAuth) {
    this.tenantsAuth = tenantsAuth;
  }

  @Override
  public Set<String> getTenantsListVisible() {
    return tenantsListVisible;
  }

  @Override
  public void setTenantsListVisible(final Set<String> tenantsListVisible) {
    this.tenantsListVisible = tenantsListVisible;
  }

  @Override
  public int hashCode() {
    final int prime = 47;
    int result = 1;
    result = prime * result + (id == null ? 0 : id.hashCode());
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
    final DocumentFile other = (DocumentFile) obj;
    if (id == null) {
      if (other.id != null) {
        return false;
      }
    } else if (!id.equals(other.id)) {
      return false;
    }
    return true;
  }

  @Override
  public String getEntityOwner() {
    return entityId;
  }

  public byte[] getContent() {
    return content;
  }

  public void setContent(final byte[] content) {
    this.content = content;
  }
}
