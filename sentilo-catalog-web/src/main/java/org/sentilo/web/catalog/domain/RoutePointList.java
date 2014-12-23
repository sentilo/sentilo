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

import java.util.LinkedList;
import java.util.List;

import org.sentilo.common.utils.DateUtils;

/**
 * This class allows store the route points that a mobile component has followed (limited to the
 * last 20 locations).
 */
public class RoutePointList {

  // This list acts as a FIFO queue to retrieve locations in historical order: newest last and
  // oldest first
  private LinkedList<RoutePoint> list;
  private final int defaultMaxSize = 20;
  private int maxSize = defaultMaxSize;

  public RoutePointList() {
    super();
    list = new LinkedList<RoutePoint>();
  }

  public RoutePointList(final int maxSize) {
    this();
    this.maxSize = maxSize;
  }

  public void add(final RoutePoint routePoint) {
    // Set the toTime for the last location saved: toTime == fromTime - 1 SECOND
    // If fromTime is null, toTime == currentTime - 1 SECOND
    // 1 SECOND must be subtracted because observations with timestamp equals to fromTime
    // are related to the current location and not with the previous.
    final RoutePoint lastLocation = peek();
    if (lastLocation != null) {
      final Long fromTimeInMillis =
          (routePoint.getFromTime() != null ? DateUtils.parseTimestamp(routePoint.getFromTime()) : System.currentTimeMillis());
      final Long previousToTime = fromTimeInMillis - 1 * 1000;
      lastLocation.setToTime(DateUtils.timestampToString(previousToTime));
    }
    // add the new location to the end of the route list
    list.add(routePoint);

    // and if necessary remove the first location from the route
    if (list.size() > maxSize) {
      list.remove();
    }
  }

  /**
   * Retrieves, but does not remove, the last route point added to the route (i.e., the more recent
   * position)
   * 
   * @return
   */
  public RoutePoint peek() {
    return list.peekLast();
  }

  public List<RoutePoint> getInternalList() {
    return list;
  }

  public int getMaxSize() {
    return maxSize;
  }
}
