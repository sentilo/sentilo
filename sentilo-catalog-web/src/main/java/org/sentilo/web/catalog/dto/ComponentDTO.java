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
package org.sentilo.web.catalog.dto;

import org.sentilo.web.catalog.domain.Component;
import org.sentilo.web.catalog.domain.ComponentType;
import org.sentilo.web.catalog.domain.Provider;
import org.sentilo.web.catalog.utils.Constants;
import org.springframework.util.StringUtils;

public class ComponentDTO {

  private String id;
  private String name;
  private String type;
  private String typeName;
  private String description;
  private String providerId;
  private String providerName;
  private Double[] location;
  private String photoUrl;
  private boolean mobile;

  public ComponentDTO(final Provider provider, final Component component) {
    super();

    if (component != null) {
      id = component.getId();
      name = component.getName();
      type = component.getComponentType();
      typeName = component.getComponentType();
      description = component.getDescription();
      providerId = component.getProviderId();
      providerName = provider.getName();
      location = component.getLocation().getCentroid();
      photoUrl = component.getPhotoUrl();
      mobile = component.getMobile() == Constants.MOBILE;
    }
  }

  public ComponentDTO(final Provider provider, final Component component, final ComponentType componentType) {
    this(provider, component);

    if (componentType != null) {
      typeName = componentType.getName();
      if (!StringUtils.hasText(photoUrl) && StringUtils.hasText(componentType.getPhotoUrl())) {
        photoUrl = componentType.getPhotoUrl();
      }
    }
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

  public String getType() {
    return type;
  }

  public void setType(final String type) {
    this.type = type;
  }

  public String getTypeName() {
    return typeName;
  }

  public void setTypeName(final String typeName) {
    this.typeName = typeName;
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

  public String getProviderName() {
    return providerName;
  }

  public void setProviderName(final String providerName) {
    this.providerName = providerName;
  }

  public Double[] getLocation() {
    return location;
  }

  public void setLocation(final Double[] location) {
    this.location = location;
  }

  public String getPhotoUrl() {
    return photoUrl;
  }

  public void setPhotoUrl(final String photoUrl) {
    this.photoUrl = photoUrl;
  }

  public boolean isMobile() {
    return mobile;
  }

  public void setMobile(final boolean mobile) {
    this.mobile = mobile;
  }

}
