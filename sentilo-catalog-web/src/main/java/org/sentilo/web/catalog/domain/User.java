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
import java.util.List;
import java.util.Set;

import javax.validation.Valid;
import javax.validation.constraints.Email;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.Pattern;

import org.sentilo.web.catalog.security.Role;
import org.sentilo.web.catalog.utils.Constants;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.format.annotation.DateTimeFormat;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

@Document
public class User implements CatalogDocument, TenantResource {

  private static final long serialVersionUID = 1L;

  @Id
  @NotBlank
  @Pattern(regexp = Constants.VALIDATION_USER_NAME_REGEXP)
  private String userName;

  private String password;

  @NotBlank
  private String name;
  private String description;

  @Email
  private String email;

  // Visual configuration of sensor on detail views
  @Valid
  @JsonInclude(value = Include.NON_NULL)
  private VisualConfiguration visualConfiguration;

  @DateTimeFormat(pattern = Constants.DATETIME_FORMAT)
  private Date createdAt;

  private String createdBy;

  @DateTimeFormat(pattern = Constants.DATETIME_FORMAT)
  private Date updatedAt;

  private String updatedBy;

  private boolean active;

  private String tenantId;

  private Set<String> tenantsAuth;

  private Set<String> tenantsListVisible;

  @NotEmpty
  private List<Role> roles;

  public User() {
    tenantsAuth = new HashSet<String>();
    tenantsListVisible = new HashSet<String>();
    visualConfiguration = new VisualConfiguration();
  }

  public User(final String userName) {
    this();
    this.userName = userName;
  }

  @Override
  public boolean equals(final Object obj) {
    if (!(obj instanceof User) || userName == null) {
      return false;
    }
    final User other = (User) obj;
    return userName.equals(other.userName);
  }

  @Override
  public int hashCode() {
    // Hashcode return must be consistent with the equals method
    final int prime = 71;
    int result = 1;
    result = prime * result + (userName == null ? 0 : userName.hashCode());
    return result;
  }

  @Override
  public String getId() {
    return getUserName();
  }

  public String getUserName() {
    return userName;
  }

  public void setUserName(final String userName) {
    this.userName = userName;
  }

  public String getPassword() {
    return password;
  }

  public void setPassword(final String password) {
    this.password = password;
  }

  public String getName() {
    return name;
  }

  public void setName(final String name) {
    this.name = name;
  }

  public String getDescription() {
    return description;
  }

  public void setDescription(final String description) {
    this.description = description;
  }

  public String getEmail() {
    return email;
  }

  public void setEmail(final String email) {
    this.email = email;
  }

  public VisualConfiguration getVisualConfiguration() {
    return visualConfiguration;
  }

  public void setVisualConfiguration(final VisualConfiguration visualConfiguration) {
    this.visualConfiguration = visualConfiguration;
  }

  @Override
  public Date getCreatedAt() {
    return createdAt;
  }

  @Override
  public void setCreatedAt(final Date createdAt) {
    this.createdAt = createdAt;
  }

  public boolean isActive() {
    return active;
  }

  public void setActive(final boolean active) {
    this.active = active;
  }

  public List<Role> getRoles() {
    return roles;
  }

  public void setRoles(final List<Role> roles) {
    this.roles = roles;
  }

  @Override
  public void setUpdatedAt(final Date updatedAt) {
    this.updatedAt = updatedAt;
  }

  @Override
  public Date getUpdatedAt() {
    return updatedAt;
  }

  @Override
  public String getTenantId() {
    return tenantId;
  }

  @Override
  public void setTenantId(final String tenantId) {
    this.tenantId = tenantId;
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
