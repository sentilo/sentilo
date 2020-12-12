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
package org.sentilo.agent.federation.domain;

import java.io.Serializable;
import java.util.Date;

import org.hibernate.validator.constraints.NotBlank;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.format.annotation.DateTimeFormat;

@Document
public class FederationConfig implements Serializable {

  private static final long serialVersionUID = 1L;

  @Id
  private String id;

  @NotBlank
  private String name;

  private boolean active;

  private String description;
  private String appClientName;
  private String appClientToken;
  private String sourceEndpoint;
  private String sourceContactName;
  private String sourceContactMail;

  @DateTimeFormat(pattern = "dd/MM/yyyy HH:mm Z")
  private Date createdAt;

  @DateTimeFormat(pattern = "dd/MM/yyyy HH:mm Z")
  private Date updatedAt;

  private String tenantId;

  public FederationConfig() {
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

  public String getDescription() {
    return description;
  }

  public void setDescription(final String description) {
    this.description = description;
  }

  public String getAppClientName() {
    return appClientName;
  }

  public void setAppClientName(final String appClientName) {
    this.appClientName = appClientName;
  }

  public String getAppClientToken() {
    return appClientToken;
  }

  public void setAppClientToken(final String appClientToken) {
    this.appClientToken = appClientToken;
  }

  public String getSourceEndpoint() {
    return sourceEndpoint;
  }

  public void setSourceEndpoint(final String sourceEndpoint) {
    this.sourceEndpoint = sourceEndpoint;
  }

  public String getSourceContactName() {
    return sourceContactName;
  }

  public void setSourceContactName(final String sourceContactName) {
    this.sourceContactName = sourceContactName;
  }

  public String getSourceContactMail() {
    return sourceContactMail;
  }

  public void setSourceContactMail(final String sourceContactMail) {
    this.sourceContactMail = sourceContactMail;
  }

  public Date getCreatedAt() {
    return createdAt;
  }

  public void setCreatedAt(final Date createdAt) {
    this.createdAt = createdAt;
  }

  public Date getUpdatedAt() {
    return updatedAt;
  }

  public void setUpdatedAt(final Date updatedAt) {
    this.updatedAt = updatedAt;
  }

  public String getTenantId() {
    return tenantId;
  }

  public void setTenantId(final String tenantId) {
    this.tenantId = tenantId;
  }

  public boolean isActive() {
    return active;
  }

  public void setActive(final boolean active) {
    this.active = active;
  }

}
