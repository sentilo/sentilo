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
import java.util.Map;
import java.util.Set;

import javax.validation.Valid;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.Pattern;

import org.sentilo.common.domain.TechnicalDetails;
import org.sentilo.common.enums.SensorState;
import org.sentilo.web.catalog.utils.CatalogUtils;
import org.sentilo.web.catalog.utils.CompoundKeyBuilder;
import org.sentilo.web.catalog.utils.Constants;
import org.sentilo.web.catalog.validator.ValidTimeZone;
import org.springframework.data.annotation.Id;
import org.springframework.data.annotation.Transient;
import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.util.StringUtils;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

@Document
public class Sensor implements FederatedResource, TenantResource, EntityResource, SyncResource, AlphabeticalSortable {

  private static final long serialVersionUID = 1L;

  public enum DataType {
    NUMBER, BOOLEAN, TEXT, JSON, LINK, AUDIO_LINK, VIDEO_LINK, IMAGE_LINK, FILE_LINK
  }

  @Id
  private String id;

  @NotBlank
  @Pattern(regexp = Constants.VALIDATION_ENTITY_NAME_REGEXP)
  private String sensorId;

  @NotBlank
  private String providerId;

  @NotBlank
  private String componentId;

  private String description;

  private DataType dataType;

  @DateTimeFormat(pattern = Constants.DATETIME_FORMAT)
  private Date createdAt;

  private String createdBy;

  @DateTimeFormat(pattern = Constants.DATETIME_FORMAT)
  private Date updatedAt;

  private String updatedBy;

  @NotBlank
  private String type;

  private String unit;

  private String validTime;

  @ValidTimeZone
  private String timeZone;

  @JsonInclude(value = Include.NON_EMPTY)
  private String tags;

  private Boolean publicAccess = Boolean.FALSE;

  @JsonInclude(value = Include.NON_EMPTY)
  private String metaData;

  private String tenantId;

  private Set<String> tenantsAuth;

  private Set<String> tenantsListVisible;

  private SensorState state;

  private String substate;

  private Integer ttl;

  @Transient
  private String substateDesc;

  private Boolean federatedResource = Boolean.FALSE;

  private String federatedServiceId;

  // Additional info
  @JsonInclude(value = Include.NON_EMPTY)
  private Map<String, String> additionalInfo;

  @JsonInclude(value = Include.NON_NULL)
  private TechnicalDetails technicalDetails;

  // Visual configuration of sensor on detail views
  @Valid
  @JsonInclude(value = Include.NON_NULL)
  private VisualConfiguration visualConfiguration;

  public Sensor() {
    tenantsAuth = new HashSet<String>();
    tenantsListVisible = new HashSet<String>();
  }

  public Sensor(final String id) {
    this();
    this.id = id;
    if (StringUtils.hasText(id)) {
      splitId();
    }
  }

  public Sensor(final String providerId, final String componentId, final String sensorId) {
    this();
    this.providerId = providerId;
    this.sensorId = sensorId;
    this.componentId = componentId;
    id = getId();
  }

  public static String buildId(final String componentId, final String sensorId) {
    return CompoundKeyBuilder.buildCompoundKey(componentId, sensorId);
  }

  @Override
  public int hashCode() {
    // Hashcode return must be consistent with the equals method
    final int prime = 61;
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
    final Sensor other = (Sensor) obj;
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
  public String getId() {
    if (!StringUtils.hasText(id) && StringUtils.hasText(sensorId) && StringUtils.hasText(componentId)) {
      id = buildId(componentId, sensorId);
    }

    return id;
  }

  protected void splitId() {
    // If Id is a compound key, split it to get its parts
    final String[] tokens = CompoundKeyBuilder.splitCompoundKey(id);
    if (tokens.length == 3) {
      providerId = tokens[0];
      componentId = CompoundKeyBuilder.buildCompoundKey(tokens[0], tokens[1]);
      sensorId = tokens[2];
    }
  }

  @Override
  public String getEntityOwner() {
    return providerId;
  }

  public String getType() {
    return type;
  }

  public void setType(final String type) {
    this.type = type;
  }

  public String getDescription() {
    return description;
  }

  public void setDescription(final String description) {
    this.description = description;
  }

  public DataType getDataType() {
    return dataType;
  }

  public void setDataType(final DataType dataType) {
    this.dataType = dataType;
  }

  public String getUnit() {
    return unit;
  }

  public void setUnit(final String unit) {
    this.unit = unit;
  }

  public String getValidTime() {
    return validTime;
  }

  public void setValidTime(final String validTime) {
    this.validTime = validTime;
  }

  public String getProviderId() {
    return providerId;
  }

  public void setProviderId(final String providerId) {
    this.providerId = providerId;
  }

  public void setSensorId(final String sensorId) {
    this.sensorId = sensorId;
  }

  public String getSensorId() {
    return sensorId;
  }

  @Override
  public void setCreatedAt(final Date createdAt) {
    this.createdAt = createdAt;
  }

  @Override
  public Date getCreatedAt() {
    return createdAt;
  }

  @Override
  public void setUpdatedAt(final Date updatedAt) {
    this.updatedAt = updatedAt;
  }

  @Override
  public Date getUpdatedAt() {
    return updatedAt;
  }

  public void setId(final String id) {
    this.id = id;
  }

  public String getTags() {
    return tags;
  }

  public List<String> getTagsAsList() {
    return CatalogUtils.tagsToStringList(tags);
  }

  public void setTags(final String tags) {
    this.tags = tags;
  }

  public Boolean getPublicAccess() {
    return publicAccess;
  }

  public void setPublicAccess(final Boolean publicAccess) {
    this.publicAccess = publicAccess;
  }

  public String getComponentId() {
    return componentId;
  }

  public void setComponentId(final String componentId) {
    this.componentId = componentId;
  }

  public void setAdditionalInfo(final Map<String, String> additionalInfo) {
    this.additionalInfo = additionalInfo;
  }

  public Map<String, String> getAdditionalInfo() {
    return additionalInfo;
  }

  public void setMetaData(final String metaData) {
    this.metaData = metaData;
  }

  public String getMetaData() {
    return metaData;
  }

  public void setTimeZone(final String timeZone) {
    this.timeZone = timeZone;
  }

  public String getTimeZone() {
    return timeZone;
  }

  public TechnicalDetails getTechnicalDetails() {
    return technicalDetails;
  }

  public void setTechnicalDetails(final TechnicalDetails technicalDetails) {
    this.technicalDetails = technicalDetails;
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

  public SensorState getState() {
    return state;
  }

  public void setState(final SensorState state) {
    this.state = state;
  }

  public String getSubstate() {
    return substate;
  }

  public Integer getTtl() {
    return ttl;
  }

  public void setTtl(final Integer ttl) {
    this.ttl = ttl;
  }

  public void setSubstate(final String substate) {
    this.substate = substate;
  }

  public String getSubstateDesc() {
    return substateDesc;
  }

  public void setSubstateDesc(final String substateDesc) {
    this.substateDesc = substateDesc;
  }

  public VisualConfiguration getVisualConfiguration() {
    return visualConfiguration;
  }

  public void setVisualConfiguration(final VisualConfiguration visualConfiguration) {
    this.visualConfiguration = visualConfiguration;
  }

  @Override
  public String getSortableValue() {
    return sensorId;
  }

  public void setFederatedResource(final Boolean federatedResource) {
    this.federatedResource = federatedResource;
  }

  public String getFederatedServiceId() {
    return federatedServiceId;
  }

  public void setFederatedServiceId(final String federatedServiceId) {
    this.federatedServiceId = federatedServiceId;
  }

  @Override
  public Boolean getFederatedResource() {
    return federatedResource;
  }
}
