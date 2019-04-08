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

public class AuthorizedProvider {

  private String provider;
  private String permission;
  private List<CatalogSensor> sensors;

  public AuthorizedProvider() {
    super();
  }

  public AuthorizedProvider(final String provider, final String permission, final List<CatalogSensor> sensors) {
    this();
    this.provider = provider;
    this.permission = permission;
    this.sensors = sensors;
  }

  public String getProvider() {
    return provider;
  }

  public void setProvider(final String provider) {
    this.provider = provider;
  }

  public String getPermission() {
    return permission;
  }

  public void setPermission(final String permission) {
    this.permission = permission;
  }

  public List<CatalogSensor> getSensors() {
    return sensors;
  }

  public void setSensors(final List<CatalogSensor> sensors) {
    this.sensors = sensors;
  }
}
