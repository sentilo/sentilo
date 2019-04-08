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

public class HomeBuilderDTO {

  private String tenantId;

  private String styleClass;

  private String jsScript;

  private String mapZoomLevel;

  private String mapCenterLat;

  private String mapCenterLng;

  public HomeBuilderDTO() {
    super();
  }

  public HomeBuilderDTO(final String tenantId, final String styleClass, final String jsScript, final String mapZoomLevel, final String mapCenterLat,
      final String mapCenterLng) {
    super();
    this.tenantId = tenantId;
    this.styleClass = styleClass;
    this.jsScript = jsScript;
    this.mapZoomLevel = mapZoomLevel;
    this.mapCenterLat = mapCenterLat;
    this.mapCenterLng = mapCenterLng;
  }

  public String getTenantId() {
    return tenantId;
  }

  public void setTenantId(final String tenantId) {
    this.tenantId = tenantId;
  }

  public String getStyleClass() {
    return styleClass;
  }

  public void setStyleClass(final String styleClass) {
    this.styleClass = styleClass;
  }

  public String getJsScript() {
    return jsScript;
  }

  public void setJsScript(final String jsScript) {
    this.jsScript = jsScript;
  }

  public String getMapZoomLevel() {
    return mapZoomLevel;
  }

  public void setMapZoomLevel(final String mapZoomLevel) {
    this.mapZoomLevel = mapZoomLevel;
  }

  public String getMapCenterLat() {
    return mapCenterLat;
  }

  public void setMapCenterLat(final String mapCenterLat) {
    this.mapCenterLat = mapCenterLat;
  }

  public String getMapCenterLng() {
    return mapCenterLng;
  }

  public void setMapCenterLng(final String mapCenterLng) {
    this.mapCenterLng = mapCenterLng;
  }

}
