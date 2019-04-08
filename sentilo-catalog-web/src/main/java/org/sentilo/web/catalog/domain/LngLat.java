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
package org.sentilo.web.catalog.domain;

import java.io.Serializable;

import org.springframework.util.Assert;

public class LngLat implements Serializable {

  private static final long serialVersionUID = 1L;

  private Double latitude;
  private Double longitude;

  public LngLat() {
  }

  public LngLat(final Double longitude, final Double latitude) {
    Assert.notNull(longitude, "longitud value is mandatory");
    Assert.notNull(latitude, "latitude value is mandatory");
    this.longitude = longitude;
    this.latitude = latitude;
  }

  @Override
  public int hashCode() {
    // Hashcode return must be consistent with the equals method
    final int prime = 61;
    int result = 1;
    result = prime * result + latitude.hashCode() + longitude.hashCode();
    return result;
  }

  @Override
  public boolean equals(final Object obj) {
    if (this == obj) {
      return true;
    }
    if (obj == null) {
      return false;
    }
    if (getClass() != obj.getClass()) {
      return false;
    }
    final LngLat other = (LngLat) obj;
    return latitude.equals(other.getLatitude()) && longitude.equals(other.getLongitude());
  }

  @Override
  public String toString() {
    return longitude + " " + latitude;
  }

  public Double[] toArray() {
    return new Double[] {longitude, latitude};
  }

  public Double getLatitude() {
    return latitude;
  }

  public void setLatitude(final Double latitude) {
    this.latitude = latitude;
  }

  public Double getLongitude() {
    return longitude;
  }

  public void setLongitude(final Double longitude) {
    this.longitude = longitude;
  }

}
