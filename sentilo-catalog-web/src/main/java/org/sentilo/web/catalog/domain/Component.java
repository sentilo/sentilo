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

import org.codehaus.jackson.annotate.JsonIgnore;
import org.codehaus.jackson.map.annotate.JsonSerialize;
import org.hibernate.validator.constraints.NotBlank;
import org.sentilo.common.domain.TechnicalDetails;
import org.sentilo.web.catalog.utils.CompoundKeyBuilder;
import org.sentilo.web.catalog.utils.Constants;
import org.sentilo.web.catalog.utils.TagUtils;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.util.StringUtils;

@Document
public class Component implements CatalogDocument {

  private static final long serialVersionUID = 1L;

  @Id
  private String id;

  @NotBlank
  @Pattern(regexp = Constants.VALIDATION_ENTITY_NAME_REGEXP)
  private String name;

  private String description;

  @NotBlank
  private String providerId;

  @DateTimeFormat(pattern = Constants.DATE_FORMAT)
  private Date createdAt;

  @DateTimeFormat(pattern = Constants.DATE_FORMAT)
  private Date updateAt;

  private int mobile = Constants.MOBILE;

  private String parentId;

  private String tags;

  private Boolean publicAccess = Boolean.FALSE;

  private String componentType;

  private Location location;

  private RoutePointList routePointList;

  private String photoUrl;

  // Additional info
  @JsonSerialize(include = JsonSerialize.Inclusion.NON_EMPTY)
  private Map<String, String> additionalInfo;

  @JsonSerialize(include = JsonSerialize.Inclusion.NON_NULL)
  private TechnicalDetails technicalDetails;

  public Component() {

  }

  public Component(final String id) {
    this();
    this.id = id;
  }

  public static String buildId(final String providerId, final String name) {
    return CompoundKeyBuilder.buildCompoundKey(providerId, name);
  }

  @Override
  public boolean equals(final Object obj) {
    if (!(obj instanceof Component) || id == null) {
      return false;
    }
    final Component other = (Component) obj;
    return getId().equals(other.getId());
  }

  @Override
  public int hashCode() {
    // Hashcode return must be consistent with the equals method
    final int prime = 23;
    int result = 1;
    result = prime * result + ((id == null) ? 0 : id.hashCode());
    return result;
  }

  @Override
  public String getId() {
    if (!StringUtils.hasText(id) && StringUtils.hasText(name) && StringUtils.hasText(providerId)) {
      id = buildId(providerId, name);
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

  public String getDescription() {
    return description;
  }

  public void setDescription(final String description) {
    this.description = description;
  }

  @Override
  public Date getCreatedAt() {
    return createdAt;
  }

  @Override
  public void setCreatedAt(final Date createdAt) {
    this.createdAt = createdAt;
  }

  public Date getUpdateAt() {
    return updateAt;
  }

  @Override
  public void setUpdateAt(final Date updateAt) {
    this.updateAt = updateAt;
  }

  public Location getLocation() {
    return location;
  }

  public void setLocation(final Location location) {
    this.location = location;
  }

  public int getMobile() {
    return mobile;
  }

  @JsonIgnore
  public boolean isMobileComponent() {
    return mobile == Constants.MOBILE;
  }

  @JsonIgnore
  public boolean isStaticComponent() {
    return mobile == Constants.STATIC;
  }

  public void setMobile(final int mobile) {
    this.mobile = mobile;
  }

  public String getParentId() {
    return parentId;
  }

  public void setParentId(final String parentId) {
    this.parentId = parentId;
  }

  @JsonIgnore
  public boolean isRoot() {
    return !StringUtils.hasText(parentId);
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

  public String getComponentType() {
    return componentType;
  }

  public void setComponentType(final String componentType) {
    this.componentType = componentType;
  }

  public String getProviderId() {
    return providerId;
  }

  public void setProviderId(final String providerId) {
    this.providerId = providerId;
  }

  public RoutePointList getRoutePointList() {
    return routePointList;
  }

  public void setRoutePointList(final RoutePointList routePointList) {
    this.routePointList = routePointList;
  }

  public String getPhotoUrl() {
    return photoUrl;
  }

  public void setPhotoUrl(final String photoUrl) {
    this.photoUrl = photoUrl;
  }

  public Map<String, String> getAdditionalInfo() {
    return additionalInfo;
  }

  public void setAdditionalInfo(final Map<String, String> additionalInfo) {
    this.additionalInfo = additionalInfo;
  }

  public TechnicalDetails getTechnicalDetails() {
    return technicalDetails;
  }

  public void setTechnicalDetails(final TechnicalDetails technicalDetails) {
    this.technicalDetails = technicalDetails;
  }

}
