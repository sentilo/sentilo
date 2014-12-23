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
package org.sentilo.web.catalog.domain;

import java.io.Serializable;

import org.sentilo.common.utils.SentiloUtils;
import org.sentilo.web.catalog.utils.Constants;

public class Location implements Serializable {

  private static final long serialVersionUID = 1L;

  /** The ordered sequence of location coordinates */
  private LngLat[] coordinates;

  /**
   * The centroid, or geometry center, of the component with this location. This attribute is needed
   * for does a geospatial search in MongoDB. Its values are [longitude, latitude]
   */
  private Double[] centroid;

  /** Timestamp that shows when the component changed its location to this one */
  private Long fromTsTime;

  public Location() {
  }

  public Location(final LngLat[] coordinates) {
    this.coordinates = coordinates;
  }

  public Location(final LngLat coordinate) {
    coordinates = new LngLat[] {coordinate};
  }

  public int getNumberOfCoordinates() {
    return (coordinates != null ? coordinates.length : 0);
  }

  public String toString() {
    final StringBuilder sb = new StringBuilder();
    boolean first = true;
    if (!SentiloUtils.arrayIsEmpty(coordinates)) {
      for (final LngLat coordinate : coordinates) {
        if (!first) {
          sb.append(Constants.LOCATION_TOKEN_SPLITTER);
        }
        sb.append(coordinate.getLatitude());
        sb.append(Constants.LOCATION_TOKEN_DIVIDER);
        sb.append(coordinate.getLongitude());

        first = false;
      }
    }

    return sb.toString();
  }

  public LngLat[] getCoordinates() {
    return coordinates;
  }

  public void setCoordinates(final LngLat[] coordinates) {
    this.coordinates = coordinates;
  }

  public Double[] getCentroid() {
    return centroid;
  }

  public void setCentroid(final Double[] centroid) {
    this.centroid = centroid;
  }

  public Long getFromTsTime() {
    return fromTsTime;
  }

  public void setFromTsTime(final Long fromTsTime) {
    this.fromTsTime = fromTsTime;
  }

}
