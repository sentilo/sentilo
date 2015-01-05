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
package org.sentilo.web.catalog.test.utils;

import static org.mockito.Mockito.when;

import java.util.Date;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;
import org.sentilo.common.utils.DateUtils;
import org.sentilo.web.catalog.domain.RoutePoint;
import org.sentilo.web.catalog.domain.RoutePointList;

public class RoutePointListTest {

  @InjectMocks
  private RoutePointList list;

  @Spy
  private RoutePoint location;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    System.setProperty("user.timezone", "UTC");
  }

  @Test
  public void add() {
    list.add(location);
    Assert.assertTrue(list.getInternalList().size() == 1);
    Assert.assertEquals(location, list.peek());
  }

  @Test
  public void maxSize() {
    when(location.getFromTime()).thenReturn(DateUtils.toStringTimestamp(new Date()));
    for (int i = 0; i < (list.getMaxSize() + 15); i++) {
      list.add(location);
    }

    Assert.assertTrue(list.getInternalList().size() == list.getMaxSize());
  }

  @Test
  public void previousTotime() {
    final String currentFromTime = "28/07/2014T11:24:12";
    final String previousToTime = "28/07/2014T11:24:11";

    when(location.getFromTime()).thenReturn(currentFromTime);

    list.add(location);
    list.add(location);

    Assert.assertTrue(list.getInternalList().size() == 2);
    Assert.assertEquals(previousToTime, list.getInternalList().get(0).getToTime());
  }
}
