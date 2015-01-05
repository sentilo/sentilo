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

import org.sentilo.common.utils.DateUtils;

/**
 * This class represents one point from a route. As a route only have sense in mobile components
 * with simple coordinates (i.e. only have one LatLng in every instant), the location is represented
 * by an instance of the class LatLng
 */
public class RoutePoint {

  private LngLat location;
  /** Date that shows when the component changed its location to this one: could be null */
  private String fromTime;
  /** Date that shows when the component has left this location */
  private String toTime;

  public RoutePoint() {
    super();
  }

  public RoutePoint(final LngLat location, final Long fromTime) {
    this.location = location;
    // This time must be an UTC time like in the Real Time backend
    // this.fromTime = DateUtils.toStringTimestamp(new Date());
    this.fromTime = DateUtils.timestampToString(fromTime);
  }

  public LngLat getLocation() {
    return location;
  }

  public void setLocation(final LngLat location) {
    this.location = location;
  }

  public String getFromTime() {
    return fromTime;
  }

  public void setFromTime(final String fromTime) {
    this.fromTime = fromTime;
  }

  public String getToTime() {
    return toTime;
  }

  public void setToTime(final String toTime) {
    this.toTime = toTime;
  }
}
