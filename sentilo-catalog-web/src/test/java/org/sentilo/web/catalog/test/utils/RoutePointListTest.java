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
import org.sentilo.common.utils.DateUtils;
import org.sentilo.web.catalog.domain.LngLat;
import org.sentilo.web.catalog.domain.RoutePoint;
import org.sentilo.web.catalog.domain.RoutePointList;
import org.sentilo.web.catalog.utils.Constants;

public class RoutePointListTest {

  private RoutePointList list;

  @Before
  public void setUp() {
    list = new RoutePointList();
    System.setProperty("user.timezone", Constants.DEFAULT_TIME_ZONE);
  }

  @Test
  public void add() {
    final RoutePoint location = new RoutePoint(new LngLat(1d, 2d), System.currentTimeMillis());
    list.add(location);
    Assert.assertTrue(list.getInternalList().size() == 1);
  }

  @Test
  public void maxSize() {
    for (int i = 0; i < list.getMaxSize() + 15; i++) {
      list.add(new RoutePoint(new LngLat(new Double(i), 2d), System.currentTimeMillis() - i * 1000));
    }

    Assert.assertTrue(list.getInternalList().size() == list.getMaxSize());
  }

  @Test
  public void previousTotime() {
    final String previousToTime = "28/07/2014T11:24:11";
    final String currentFromTime = "28/07/2014T11:24:12";

    list.add(new RoutePoint(new LngLat(1d, 2d), DateUtils.parseTimestamp(previousToTime)));
    list.add(new RoutePoint(new LngLat(2d, 3d), DateUtils.parseTimestamp(currentFromTime)));

    Assert.assertTrue(list.getInternalList().size() == 2);
    Assert.assertEquals(previousToTime, list.getInternalList().get(0).getToTime());
  }

  @Test
  public void elementNotAdded() {
    final String previousToTime = "28/07/2014T11:24:11";
    final String currentFromTime = "28/07/2014T11:24:12";

    final RoutePoint rp1 = new RoutePoint(new LngLat(1d, 2d), DateUtils.parseTimestamp(previousToTime));
    final RoutePoint rp2 = new RoutePoint(new LngLat(1d, 2d), DateUtils.parseTimestamp(currentFromTime));

    // If rp1 is added first then rp2 will not be added into the list because
    // rp2.location == rp1.location

    list.add(rp1);
    list.add(rp2);

    Assert.assertTrue(list.getInternalList().size() == 1);
    Assert.assertNull(list.getInternalList().get(0).getToTime());
    Assert.assertFalse(list.getInternalList().contains(rp2));
    Assert.assertTrue(list.getInternalList().contains(rp1));
  }

  @Test
  public void elementRemoved() {
    final String previousToTime = "28/07/2014T11:24:11";
    final String currentFromTime = "28/07/2014T11:24:12";

    final RoutePoint rp1 = new RoutePoint(new LngLat(1d, 2d), DateUtils.parseTimestamp(previousToTime));
    final RoutePoint rp2 = new RoutePoint(new LngLat(1d, 2d), DateUtils.parseTimestamp(currentFromTime));

    // If rp2 is added first then rp2 will be removed when rp1 is added because
    // rp2.location == rp1.location and rp1.timestamp < rp2.timestamp

    list.add(rp2);
    list.add(rp1);

    Assert.assertTrue(list.getInternalList().size() == 1);
    Assert.assertNull(list.getInternalList().get(0).getToTime());
    Assert.assertFalse(list.getInternalList().contains(rp2));
    Assert.assertTrue(list.getInternalList().contains(rp1));
  }
}
