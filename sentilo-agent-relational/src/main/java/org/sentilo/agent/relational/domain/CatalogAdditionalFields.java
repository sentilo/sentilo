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
package org.sentilo.agent.relational.domain;

import org.springframework.data.mongodb.core.mapping.Field;
import org.springframework.util.StringUtils;

public class CatalogAdditionalFields {

  /** Component Id */
  private String componentId;
  /** Sensor type */
  private String type;
  /** Component type */
  private String componentType;
  /** Component current location */
  @Field("location.centroid")
  private String location;
  @Field("name")
  private String componentName;

  public void addAll(final CatalogAdditionalFields caf) {
    if (caf != null) {
      if (StringUtils.hasText(caf.getLocation())) {
        setLocation(caf.getLocation());
      }

      if (StringUtils.hasText(caf.getComponentType())) {
        setComponentType(caf.getComponentType());
      }

      if (StringUtils.hasText(caf.getComponentName())) {
        setComponentName(caf.getComponentName());
      }
    }
  }

  public String getComponentId() {
    return componentId;
  }

  public void setComponentId(final String componentId) {
    this.componentId = componentId;
  }

  public String getComponentName() {
    return componentName;
  }

  public String getComponentType() {
    return componentType;
  }

  public void setComponentType(final String componentType) {
    this.componentType = componentType;
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

  public void setComponentName(final String componentName) {
    this.componentName = componentName;
  }
}
