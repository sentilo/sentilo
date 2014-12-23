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
package org.sentilo.platform.common.domain;

import java.util.List;

import org.codehaus.jackson.map.annotate.JsonSerialize;
import org.sentilo.common.domain.CatalogProvider;
import org.sentilo.common.domain.CatalogSensor;
import org.sentilo.common.domain.PlatformInputMessage;

public class AdminInputMessage implements PlatformInputMessage {

  public static enum AdminType {
    stats, subscriptions, delete
  };

  private String entity;
  private AdminType type;

  @JsonSerialize(include = JsonSerialize.Inclusion.NON_EMPTY)
  private List<CatalogSensor> sensors;

  @JsonSerialize(include = JsonSerialize.Inclusion.NON_EMPTY)
  private List<CatalogProvider> providers;

  public AdminInputMessage() {
    super();
  }

  public AdminInputMessage(final AdminType type) {
    this();
    setType(type);
  }

  public String getEntity() {
    return entity;
  }

  public void setEntity(final String entity) {
    this.entity = entity;
  }

  public AdminType getType() {
    return type;
  }

  public void setType(final AdminType type) {
    this.type = type;
  }

  public List<CatalogSensor> getSensors() {
    return sensors;
  }

  public void setSensors(final List<CatalogSensor> sensors) {
    this.sensors = sensors;
  }

  public List<CatalogProvider> getProviders() {
    return providers;
  }

  public void setProviders(final List<CatalogProvider> providers) {
    this.providers = providers;
  }

}
