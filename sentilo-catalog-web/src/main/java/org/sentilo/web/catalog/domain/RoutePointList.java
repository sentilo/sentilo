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

import java.util.ArrayList;
import java.util.List;
import java.util.NavigableSet;
import java.util.TreeSet;

import org.sentilo.common.utils.DateUtils;

/**
 * This class allows store the route points that a mobile component has followed (limited to the
 * last 20 locations).
 *
 * To implement the list, a SortedSet is used because it guarantees that elements into the list are
 * always sorted.
 */
public class RoutePointList {

  // This list acts as a FIFO queue to retrieve locations in historical order: newest last and
  // oldest first
  private NavigableSet<RoutePoint> list;
  private static final int DEFAULT_MAX_SIZE = 20;
  private int maxSize = DEFAULT_MAX_SIZE;

  public RoutePointList() {
    super();
    list = new TreeSet<RoutePoint>();
  }

  public RoutePointList(final int maxSize) {
    this();
    this.maxSize = maxSize;
  }

  public void add(final RoutePoint routePoint) {
    final RoutePoint floor = list.floor(routePoint);
    final RoutePoint ceiling = list.ceiling(routePoint);

    // floor and ceiling are the greatest and least elements in the set that verify
    // floor.ts <= routePoint.ts <= ceiling.ts
    //
    // Therefore routePoint must be added to the set if and only if
    // 1. routePoint.ts != floor.ts and routePoint.ts!= ceiling.ts (one component can't be in two
    // locations at the same time)
    // 2. floor.loc != routePoint.loc (must modify the previous location on the route)

    // Likewise, if routePoint.loc == ceiling.loc then ceiling must be removed from the route
    // because currentPoint has a previous timestamp

    if (!areFromTimesEquals(routePoint, floor) && !areFromTimesEquals(routePoint, ceiling) && !areLocationEquals(routePoint, floor)) {
      list.add(routePoint);

      if (areLocationEquals(routePoint, ceiling)) {
        routePoint.setToTime(ceiling.getToTime());
        list.remove(ceiling);
      }

      if (floor != null) {
        // Set the toTime for the floor element to: floor.toTime == routePoint.fromTime - 1 SECOND
        // 1 SECOND must be subtracted because observations with timestamp equals to fromTime
        // are related to the current location and not with the previous.
        final Long fromTimeInMillis = routePoint.getFromTimeTs();
        final Long previousToTime = fromTimeInMillis - 1 * 1000;
        floor.setToTime(DateUtils.timestampToString(previousToTime));
      }

      // Finally, list.size() <= maxSize
      while (list.size() > maxSize) {
        list.pollFirst();
      }
    }
  }

  private boolean areLocationEquals(final RoutePoint rp1, final RoutePoint rp2) {
    return rp1 != null && rp2 != null && rp1.getLocation().equals(rp2.getLocation());
  }

  private boolean areFromTimesEquals(final RoutePoint rp1, final RoutePoint rp2) {
    return rp1 != null && rp2 != null && rp1.getFromTimeTs().equals(rp2.getFromTimeTs());
  }

  public List<RoutePoint> getInternalList() {
    return new ArrayList<RoutePoint>(list);
  }

  public int getMaxSize() {
    return maxSize;
  }
}
