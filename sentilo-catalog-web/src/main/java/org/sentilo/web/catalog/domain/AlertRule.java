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

import org.sentilo.common.enums.AlertTriggerType;
import org.sentilo.web.catalog.utils.Constants;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.format.annotation.DateTimeFormat;

@Document
public class AlertRule implements CatalogDocument, TenantResource {

  private static final long serialVersionUID = 1L;

  @Id
  private String id;

  @DateTimeFormat(pattern = Constants.DATETIME_FORMAT)
  private Date createdAt;

  private String createdBy;

  @DateTimeFormat(pattern = Constants.DATETIME_FORMAT)
  private Date updatedAt;

  private String updatedBy;

  @NotBlank
  private String name;

  private String description;

  @NotBlank
  private String providerId;

  private String componentType;

  private String sensorType;

  private AlertTriggerType trigger;

  private String expression;

  private int totalSensors;

  /** Alerts generated in the last run */
  private int generatedAlerts;

  /**
   * Identificador de la organización a la que pertenece la regla
   */
  private String tenantId;

  /**
   * Listado de organizaciones con permisos sobre la regla: en este caso sólo será 1
   */
  private Set<String> tenantsAuth;

  /**
   * Listado de organizaciones con permisos para listar la alerta
   */
  private Set<String> tenantsListVisible;

  public AlertRule() {
    tenantsAuth = new HashSet<String>();
    tenantsListVisible = new HashSet<String>();
  }

  public AlertRule(final String id) {
    super();
    this.id = id;
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + (id == null ? 0 : id.hashCode());
    return result;
  }

  @Override
  public boolean equals(final Object obj) {
    if (!(obj instanceof AlertRule) || id == null) {
      return false;
    }
    final AlertRule other = (AlertRule) obj;
    return id.equals(other.id);
  }

  @Override
  public String getId() {
    return id;
  }

  public void setId(final String id) {
    this.id = id;
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

  public String getProviderId() {
    return providerId;
  }

  public void setProviderId(final String providerId) {
    this.providerId = providerId;
  }

  public String getComponentType() {
    return componentType;
  }

  public void setComponentType(final String componentType) {
    this.componentType = componentType;
  }

  public String getSensorType() {
    return sensorType;
  }

  public void setSensorType(final String sensorType) {
    this.sensorType = sensorType;
  }

  public AlertTriggerType getTrigger() {
    return trigger;
  }

  public void setTrigger(final AlertTriggerType trigger) {
    this.trigger = trigger;
  }

  public String getExpression() {
    return expression;
  }

  public void setExpression(final String expression) {
    this.expression = expression;
  }

  public int getTotalSensors() {
    return totalSensors;
  }

  public void setTotalSensors(final int totalSensors) {
    this.totalSensors = totalSensors;
  }

  public int getGeneratedAlerts() {
    return generatedAlerts;
  }

  public void setGeneratedAlerts(final int generatedAlerts) {
    this.generatedAlerts = generatedAlerts;
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
  public String getTenantId() {
    return tenantId;
  }

  @Override
  public Set<String> getTenantsAuth() {
    return tenantsAuth;
  }

  @Override
  public void setTenantId(final String tenantId) {
    this.tenantId = tenantId;
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
}
