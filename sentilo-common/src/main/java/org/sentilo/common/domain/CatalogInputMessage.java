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

import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

public class CatalogInputMessage implements PlatformInputMessage {

  @JsonInclude(value = Include.NON_NULL)
  private String providerId;
  @JsonIgnore
  private String entityId;
  @JsonIgnore
  private String body;

  @JsonInclude(value = Include.NON_EMPTY)
  private Map<String, String> parameters;

  @JsonInclude(value = Include.NON_EMPTY)
  private List<CatalogSensor> sensors;

  @JsonInclude(value = Include.NON_EMPTY)
  private List<CatalogComponent> components;

  @JsonInclude(value = Include.NON_EMPTY)
  private List<SensorLocationElement> locations;

  public CatalogInputMessage() {
    super();
  }

  public CatalogInputMessage(final String entityId, final Map<String, String> parameters) {
    this();
    this.entityId = entityId;
    this.parameters = parameters;
  }

  public void setProviderId(final String providerId) {
    this.providerId = providerId;
  }

  public String getProviderId() {
    return providerId;
  }

  public void setSensors(final List<CatalogSensor> sensors) {
    this.sensors = sensors;
  }

  public List<CatalogSensor> getSensors() {
    return sensors;
  }

  public void setBody(final String body) {
    this.body = body;
  }

  public String getBody() {
    return body;
  }

  public void setEntityId(final String entityId) {
    this.entityId = entityId;
  }

  public String getEntityId() {
    return entityId;
  }

  public List<CatalogComponent> getComponents() {
    return components;
  }

  public void setComponents(final List<CatalogComponent> components) {
    this.components = components;
  }

  public void setParameters(final Map<String, String> parameters) {
    this.parameters = parameters;
  }

  public Map<String, String> getParameters() {
    return parameters;
  }

  public List<SensorLocationElement> getLocations() {
    return locations;
  }

  public void setLocations(final List<SensorLocationElement> locations) {
    this.locations = locations;
  }
}
