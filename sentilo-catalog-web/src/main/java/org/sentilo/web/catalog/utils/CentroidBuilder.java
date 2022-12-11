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
package org.sentilo.web.catalog.utils;

import org.sentilo.common.utils.SentiloUtils;
import org.sentilo.web.catalog.domain.LngLat;
import org.sentilo.web.catalog.domain.Location;

public abstract class CentroidBuilder {

  protected CentroidBuilder() {
    // this prevents even the native class from calling this ctor as well :
    throw new AssertionError();
  }

  /**
   * Build the centroid of a set of coordinates
   *
   * @param location
   * @return
   */
  public static void build(final Location location) {
    Double[] centroid = null;

    if (!SentiloUtils.arrayIsEmpty(location.getCoordinates())) {

      // The centroid of a polyline (open or closed) is obtained by finding the midpoints of each
      // line segment and then forming their weighted average using the segment lengths as weights.
      // And to calculate the centroid we can treat latitude and longitude as Cartesian coordinates:
      // location points are so close to each other, in a global vision, that we can consider the
      // Earth locally flat.

      if (location.getNumberOfCoordinates() == 1) {
        centroid = location.getCoordinates()[0].toArray();
      } else {
        centroid = calculatePolylineCentroid(location.getCoordinates());
      }

      location.setCentroid(centroid);
    }
  }

  private static Double[] calculatePolylineCentroid(final LngLat[] coordinates) {
    double sumY = 0;
    double sumX = 0;
    double totalWeight = 0;

    final int n = coordinates.length;

    for (int i = 1; i < n; i++) {

      final double latP = coordinates[i].getLatitude();
      final double lngP = coordinates[i].getLongitude();
      final double latQ = coordinates[i - 1].getLatitude();
      final double lngQ = coordinates[i - 1].getLongitude();

      // weight := d(P,Q)
      final double weight = Math.sqrt(Math.pow(latP - latQ, 2) + Math.pow(lngP - lngQ, 2));

      sumX += (latP + latQ) / 2 * weight;
      sumY += (lngP + lngQ) / 2 * weight;
      totalWeight += weight;
    }

    return new LngLat(sumY / totalWeight, sumX / totalWeight).toArray();
  }
}
