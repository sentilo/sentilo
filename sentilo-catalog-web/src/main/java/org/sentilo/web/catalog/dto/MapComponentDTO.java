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
import org.sentilo.web.catalog.domain.LngLat;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

public class MapComponentDTO {

  private final String icon;
  private final String id;
  private final String type;

  @JsonInclude(value = Include.NON_EMPTY)
  private LngLat[] coordinates;

  @JsonInclude(value = Include.NON_NULL)
  private LngLat centroid;

  public MapComponentDTO(final Component component, final String icon) {
    id = component.getId();
    type = component.getComponentType();
    this.icon = icon;
    if (component.getLocation() != null) {
      coordinates = component.getLocation().getCoordinates();
      centroid = new LngLat(component.getLocation().getCentroid()[0], component.getLocation().getCentroid()[1]);
    }
  }

  public String getIcon() {
    return icon;
  }

  public String getId() {
    return id;
  }

  public LngLat[] getCoordinates() {
    return coordinates;
  }

  public String getType() {
    return type;
  }

  public LngLat getCentroid() {
    return centroid;
  }
}
