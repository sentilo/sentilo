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
package org.sentilo.web.catalog.test.utils;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;
import org.sentilo.web.catalog.domain.LngLat;
import org.sentilo.web.catalog.domain.Location;
import org.sentilo.web.catalog.utils.CentroidBuilder;

public class CentroidBuilderTest {

  static LngLat[] poi = {new LngLat(0d, 0d)};
  static LngLat[] wrongPolyline = {new LngLat(2d, 3d), new LngLat(2d, 3d)}; // polyline with two
                                                                            // identical points
  static LngLat[] polygon =
      {new LngLat(0d, 0d), new LngLat(0.5d, 0d), new LngLat(1d, 0d), new LngLat(1d, 1d), new LngLat(0d, 1d), new LngLat(0d, 0d)};
  static LngLat[] polyline = {new LngLat(0d, 0d), new LngLat(6d, 0d), new LngLat(6d, 12d)};

  @Spy
  private Location location;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void buildCentroidForNullCoordinates() {
    location.setCoordinates(null);

    CentroidBuilder.build(location);

    Assert.assertNull(location.getCentroid());
  }

  @Test
  public void buildCentroidForPoi() {
    final Double[] centroidExpected = {0d, 0d};
    location.setCoordinates(poi);

    CentroidBuilder.build(location);

    Assert.assertArrayEquals(centroidExpected, location.getCentroid());
  }

  @Test
  public void buildCentroidForPolyline() {
    final Double[] centroidExpected = {5d, 4d};
    location.setCoordinates(polyline);

    CentroidBuilder.build(location);

    Assert.assertArrayEquals(centroidExpected, location.getCentroid());
  }

  @Test
  public void buildCentroidForWrongPolyline() {
    location.setCoordinates(wrongPolyline);

    CentroidBuilder.build(location);

    Assert.assertTrue(location.getCentroid()[0].isNaN() && location.getCentroid()[1].isNaN());
  }

  @Test
  public void buildCentroidForPolygon() {
    final Double[] centroidExpected = {0.5d, 0.5d};
    location.setCoordinates(polygon);

    CentroidBuilder.build(location);

    Assert.assertArrayEquals(centroidExpected, location.getCentroid());
  }

}
