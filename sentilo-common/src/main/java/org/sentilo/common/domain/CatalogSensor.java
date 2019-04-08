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
package org.sentilo.common.domain;

import java.util.Map;

import org.sentilo.common.enums.SensorState;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

public class CatalogSensor implements MutableCatalogElement {

  private String sensor;
  @JsonInclude(value = Include.NON_NULL)
  private String provider;
  @JsonInclude(value = Include.NON_NULL)
  private String description;
  @JsonInclude(value = Include.NON_NULL)
  private String dataType;
  @JsonInclude(value = Include.NON_NULL)
  private String location;
  @JsonInclude(value = Include.NON_NULL)
  private String type;
  @JsonInclude(value = Include.NON_NULL)
  private String unit;
  @JsonInclude(value = Include.NON_NULL)
  private String timeZone;
  @JsonInclude(value = Include.NON_NULL)
  private Boolean publicAccess;
  @JsonInclude(value = Include.NON_NULL)
  private SensorState state;
  @JsonInclude(value = Include.NON_NULL)
  private Integer ttl;

  @JsonInclude(value = Include.NON_NULL)
  private String component;
  @JsonInclude(value = Include.NON_NULL)
  private String componentType;
  @JsonInclude(value = Include.NON_NULL)
  private String componentDesc;
  @JsonInclude(value = Include.NON_NULL)
  private Boolean componentPublicAccess;

  @JsonInclude(value = Include.NON_EMPTY)
  private Map<String, String> additionalInfo;
  @JsonInclude(value = Include.NON_EMPTY)
  private Map<String, String> componentAdditionalInfo;

  @JsonInclude(value = Include.NON_NULL)
  private TechnicalDetails technicalDetails;
  @JsonInclude(value = Include.NON_NULL)
  private TechnicalDetails componentTechnicalDetails;

  @JsonInclude(value = Include.NON_NULL)
  private Long createdAt;
  @JsonInclude(value = Include.NON_NULL)
  private Long updatedAt;
  @JsonInclude(value = Include.NON_NULL)
  private Long componentCreatedAt;
  @JsonInclude(value = Include.NON_NULL)
  private Long componentUpdatedAt;

  public CatalogSensor() {
    super();
  }

  public String getSensor() {
    return sensor;
  }

  public void setSensor(final String sensor) {
    this.sensor = sensor;
  }

  public String getProvider() {
    return provider;
  }

  public void setProvider(final String provider) {
    this.provider = provider;
  }

  public String getDescription() {
    return description;
  }

  public void setDescription(final String description) {
    this.description = description;
  }

  public String getDataType() {
    return dataType;
  }

  public void setDataType(final String dataType) {
    this.dataType = dataType;
  }

  public String getLocation() {
    return location;
  }

  public void setLocation(final String location) {
    this.location = location;
  }

  public String getType() {
    return type;
  }

  public void setType(final String type) {
    this.type = type;
  }

  public String getUnit() {
    return unit;
  }

  public void setUnit(final String unit) {
    this.unit = unit;
  }

  public String getComponent() {
    return component;
  }

  public void setComponent(final String component) {
    this.component = component;
  }

  public String getComponentType() {
    return componentType;
  }

  public void setComponentType(final String componentType) {
    this.componentType = componentType;
  }

  public void setComponentDesc(final String componentDesc) {
    this.componentDesc = componentDesc;
  }

  public String getComponentDesc() {
    return componentDesc;
  }

  public void setAdditionalInfo(final Map<String, String> additionalInfo) {
    this.additionalInfo = additionalInfo;
  }

  public Map<String, String> getAdditionalInfo() {
    return additionalInfo;
  }

  public void setPublicAccess(final Boolean publicAccess) {
    this.publicAccess = publicAccess;
  }

  public Boolean getPublicAccess() {
    return publicAccess;
  }

  public void setComponentPublicAccess(final Boolean componentPublicAccess) {
    this.componentPublicAccess = componentPublicAccess;
  }

  public Boolean getComponentPublicAccess() {
    return componentPublicAccess;
  }

  public void setTimeZone(final String timeZone) {
    this.timeZone = timeZone;
  }

  public String getTimeZone() {
    return timeZone;
  }

  public Map<String, String> getComponentAdditionalInfo() {
    return componentAdditionalInfo;
  }

  public void setComponentAdditionalInfo(final Map<String, String> componentAdditionalInfo) {
    this.componentAdditionalInfo = componentAdditionalInfo;
  }

  public TechnicalDetails getTechnicalDetails() {
    return technicalDetails;
  }

  public void setTechnicalDetails(final TechnicalDetails technicalDetails) {
    this.technicalDetails = technicalDetails;
  }

  public TechnicalDetails getComponentTechnicalDetails() {
    return componentTechnicalDetails;
  }

  public void setComponentTechnicalDetails(final TechnicalDetails componentTechnicalDetails) {
    this.componentTechnicalDetails = componentTechnicalDetails;
  }

  public SensorState getState() {
    return state;
  }

  public void setState(final SensorState state) {
    this.state = state;
  }

  public Integer getTtl() {
    return ttl;
  }

  public void setTtl(final Integer ttl) {
    this.ttl = ttl;
  }

  public Long getCreatedAt() {
    return createdAt;
  }

  public void setCreatedAt(final Long createdAt) {
    this.createdAt = createdAt;
  }

  public Long getUpdatedAt() {
    return updatedAt;
  }

  public void setUpdatedAt(final Long updatedAt) {
    this.updatedAt = updatedAt;
  }

  public Long getComponentCreatedAt() {
    return componentCreatedAt;
  }

  public void setComponentCreatedAt(final Long componentCreatedAt) {
    this.componentCreatedAt = componentCreatedAt;
  }

  public Long getComponentUpdatedAt() {
    return componentUpdatedAt;
  }

  public void setComponentUpdatedAt(final Long componentUpdatedAt) {
    this.componentUpdatedAt = componentUpdatedAt;
  }

}
