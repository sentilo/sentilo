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
package org.sentilo.platform.client.core.domain;

import java.util.ArrayList;
import java.util.List;

import org.codehaus.jackson.annotate.JsonIgnore;
import org.codehaus.jackson.map.annotate.JsonSerialize;
import org.sentilo.platform.client.core.utils.ResourcesUtils;

public class CatalogDeleteInputMessage implements PlatformClientInputMessage {

  @JsonIgnore
  private String providerId;
  @JsonSerialize(include = JsonSerialize.Inclusion.NON_EMPTY)
  private String[] sensors;
  @JsonSerialize(include = JsonSerialize.Inclusion.NON_EMPTY)
  private String[] components;
  @JsonIgnore
  private String identityToken;

  /** Lista ordenada de los identificadores que forman el path del recurso. */
  @JsonIgnore
  private final List<String> resourcesValues = new ArrayList<String>();

  public CatalogDeleteInputMessage(final String providerId) {
    super();
    this.providerId = providerId;
    ResourcesUtils.addToResources(this.providerId, resourcesValues);
  }

  public List<String> getResourcesValues() {
    return resourcesValues;
  }

  public String getIdentityToken() {
    return identityToken;
  }

  public void setIdentityToken(final String identityToken) {
    this.identityToken = identityToken;
  }

  public void setProviderId(final String providerId) {
    this.providerId = providerId;
  }

  public String getProviderId() {
    return providerId;
  }

  public String[] getSensors() {
    return sensors;
  }

  public void setSensors(final String[] sensors) {
    this.sensors = sensors;
  }

  public String[] getComponents() {
    return components;
  }

  public void setComponents(final String[] components) {
    this.components = components;
  }

}
