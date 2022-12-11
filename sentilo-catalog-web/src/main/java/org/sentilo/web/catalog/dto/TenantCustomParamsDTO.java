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

import org.sentilo.web.catalog.domain.MapParams;
import org.sentilo.web.catalog.domain.Tenant;

public class TenantCustomParamsDTO {

  private String tenantId;

  private String tenantName;

  private String styleClass;

  private MapParams mapParams;

  private String logoUrl;

  private String address;

  private String friendlyName;

  public TenantCustomParamsDTO() {
    super();
    mapParams = new MapParams();
  }

  public TenantCustomParamsDTO(final Tenant tenant, final String styleClass, final MapParams mapParams) {
    super();
    tenantId = tenant.getId();
    tenantName = tenant.getName();
    logoUrl = tenant.getLogoUrl();
    address = tenant.getAddress();
    friendlyName = tenant.getFriendlyName();
    this.styleClass = styleClass;
    this.mapParams = mapParams;
  }

  public String getTenantId() {
    return tenantId;
  }

  public void setTenantId(final String tenantId) {
    this.tenantId = tenantId;
  }

  public String getTenantName() {
    return tenantName;
  }

  public void setTenantName(final String tenantName) {
    this.tenantName = tenantName;
  }

  public String getStyleClass() {
    return styleClass;
  }

  public void setStyleClass(final String styleClass) {
    this.styleClass = styleClass;
  }

  public MapParams getMapParams() {
    return mapParams;
  }

  public void setMapParams(final MapParams mapParams) {
    this.mapParams = mapParams;
  }

  public String getLogoUrl() {
    return logoUrl;
  }

  public String getAddress() {
    return address;
  }

  public String getFriendlyName() {
    return friendlyName;
  }

}
