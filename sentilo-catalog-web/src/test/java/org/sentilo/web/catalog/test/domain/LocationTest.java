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
package org.sentilo.web.catalog.test.domain;

import org.junit.Assert;
import org.junit.Test;
import org.sentilo.web.catalog.domain.LngLat;
import org.sentilo.web.catalog.domain.Location;

public class LocationTest {

  @Test
  public void emptyLocation() {
    final Location location = new Location();

    Assert.assertNull(location.getCoordinates());
    Assert.assertTrue(location.getNumberOfCoordinates() == 0);
    Assert.assertEquals("", location.toString());
  }

  @Test
  public void simpleLocation() {
    final LngLat point = new LngLat(2.111, 41.2222);
    final Location location = new Location(point);

    Assert.assertTrue(location.getNumberOfCoordinates() == 1);
    Assert.assertEquals("41.2222 2.111", location.toString());
  }

  @Test
  public void complexLocation() {
    final LngLat point1 = new LngLat(0d, 0d);
    final LngLat point2 = new LngLat(0d, 6d);
    final LngLat point3 = new LngLat(12d, 6d);

    final LngLat[] coordinates = {point1, point2, point3};

    final Location location = new Location(coordinates);

    Assert.assertTrue(location.getNumberOfCoordinates() == 3);
  }
}
