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

import java.util.Date;
import java.util.List;
import java.util.Map;

import javax.validation.constraints.Pattern;

import org.codehaus.jackson.map.annotate.JsonSerialize;
import org.codehaus.jackson.map.annotate.JsonSerialize.Inclusion;
import org.hibernate.validator.constraints.NotBlank;
import org.sentilo.common.domain.TechnicalDetails;
import org.sentilo.web.catalog.utils.CompoundKeyBuilder;
import org.sentilo.web.catalog.utils.Constants;
import org.sentilo.web.catalog.utils.TagUtils;
import org.sentilo.web.catalog.validator.ValidTimeZone;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.util.StringUtils;

@Document
public class Sensor implements CatalogDocument {

  private static final long serialVersionUID = 1L;

  public enum DataType {
    NUMBER, BOOLEAN, TEXT,
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

  @DateTimeFormat(pattern = Constants.DATE_FORMAT)
  private Date createdAt;

  @DateTimeFormat(pattern = Constants.DATE_FORMAT)
  private Date updateAt;

  @NotBlank
  private String type;

  private String unit;

  private String validTime;

  @ValidTimeZone
  private String timeZone;

  @JsonSerialize(include = Inclusion.NON_EMPTY)
  private String tags;

  private Boolean publicAccess = Boolean.FALSE;

  @JsonSerialize(include = Inclusion.NON_EMPTY)
  private String metaData;

  // Additional info
  @JsonSerialize(include = JsonSerialize.Inclusion.NON_EMPTY)
  private Map<String, String> additionalInfo;

  @JsonSerialize(include = JsonSerialize.Inclusion.NON_NULL)
  private TechnicalDetails technicalDetails;

  public Sensor() {

  }

  public Sensor(final String id) {
    this();
    this.id = id;
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
    result = prime * result + ((id == null) ? 0 : id.hashCode());
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

  public String getId() {
    if (!StringUtils.hasText(id) && StringUtils.hasText(sensorId) && StringUtils.hasText(componentId)) {
      id = buildId(componentId, sensorId);
    }

    return id;
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

  public void setCreatedAt(final Date createdAt) {
    this.createdAt = createdAt;
  }

  public Date getCreatedAt() {
    return createdAt;
  }

  public void setUpdateAt(final Date updateAt) {
    this.updateAt = updateAt;
  }

  public Date getUpdateAt() {
    return updateAt;
  }

  public void setId(final String id) {
    this.id = id;
  }

  public String getTags() {
    return tags;
  }

  public List<String> getTagsAsList() {
    return TagUtils.toStringList(tags);
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
}
