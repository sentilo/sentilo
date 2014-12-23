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
package org.sentilo.web.catalog.domain;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.validation.constraints.Pattern;

import org.hibernate.validator.constraints.Email;
import org.hibernate.validator.constraints.NotBlank;
import org.sentilo.web.catalog.utils.Constants;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.format.annotation.DateTimeFormat;

@Document
public class Application implements CatalogDocument {

  private static final long serialVersionUID = 1L;

  @Id
  @NotBlank
  @Pattern(regexp = Constants.VALIDATION_ENTITY_NAME_REGEXP)
  private String id;

  private String name;

  private String token;

  private String description;

  @NotBlank
  @Email
  private String email;

  @DateTimeFormat(pattern = Constants.DATE_FORMAT)
  private Date createdAt;

  @DateTimeFormat(pattern = Constants.DATE_FORMAT)
  private Date updateAt;

  private List<Provider> authorizedProviders;

  public Application() {
    authorizedProviders = new ArrayList<Provider>();
  }

  public Application(final String id) {
    this();
    this.id = id;
  }

  @Override
  public boolean equals(final Object obj) {
    if (!(obj instanceof Application) || id == null) {
      return false;
    }
    final Application other = (Application) obj;
    return id.equals(other.id);
  }

  @Override
  public int hashCode() {
    // Hashcode return must be consistent with the equals method
    final int prime = 19;
    int result = 1;
    result = prime * result + ((id == null) ? 0 : id.hashCode());
    return result;
  }

  public String getId() {
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

  public Date getCreatedAt() {
    return createdAt;
  }

  public void setCreatedAt(final Date createdAt) {
    this.createdAt = createdAt;
  }

  public List<Provider> getAuthorizedProviders() {
    return authorizedProviders;
  }

  public void setAuthorizedProviders(final List<Provider> authorizedProviders) {
    this.authorizedProviders = authorizedProviders;
  }

  public String getToken() {
    return token;
  }

  public void setToken(final String token) {
    this.token = token;
  }

  public String getEmail() {
    return email;
  }

  public void setEmail(final String email) {
    this.email = email;
  }

  public String getDescription() {
    return description;
  }

  public void setDescription(final String description) {
    this.description = description;
  }

  public void setUpdateAt(final Date updateAt) {
    this.updateAt = updateAt;
  }

  public Date getUpdateAt() {
    return updateAt;
  }
}
