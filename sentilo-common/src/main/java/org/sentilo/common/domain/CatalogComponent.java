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
package org.sentilo.common.domain;

import java.util.Map;

import org.codehaus.jackson.map.annotate.JsonSerialize;

public class CatalogComponent implements CatalogElement {

  private String component;
  @JsonSerialize(include = JsonSerialize.Inclusion.NON_NULL)
  private String location;
  @JsonSerialize(include = JsonSerialize.Inclusion.NON_NULL)
  private String componentType;
  @JsonSerialize(include = JsonSerialize.Inclusion.NON_NULL)
  private String componentDesc;
  @JsonSerialize(include = JsonSerialize.Inclusion.NON_NULL)
  private Boolean componentPublicAccess;
  @JsonSerialize(include = JsonSerialize.Inclusion.NON_EMPTY)
  private Map<String, String> componentAdditionalInfo;
  @JsonSerialize(include = JsonSerialize.Inclusion.NON_NULL)
  private TechnicalDetails componentTechnicalDetails;

  public CatalogComponent() {
    super();
  }

  public String getLocation() {
    return location;
  }

  public void setLocation(final String location) {
    this.location = location;
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

  public void setComponentPublicAccess(final Boolean componentPublicAccess) {
    this.componentPublicAccess = componentPublicAccess;
  }

  public Boolean getComponentPublicAccess() {
    return componentPublicAccess;
  }

  public Map<String, String> getComponentAdditionalInfo() {
    return componentAdditionalInfo;
  }

  public void setComponentAdditionalInfo(final Map<String, String> componentAdditionalInfo) {
    this.componentAdditionalInfo = componentAdditionalInfo;
  }

  public TechnicalDetails getComponentTechnicalDetails() {
    return componentTechnicalDetails;
  }

  public void setComponentTechnicalDetails(final TechnicalDetails componentTechnicalDetails) {
    this.componentTechnicalDetails = componentTechnicalDetails;
  }
}
