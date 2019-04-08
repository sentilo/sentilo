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
package org.sentilo.web.catalog.test.dto;

import static org.mockito.Mockito.when;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.web.catalog.domain.Component;
import org.sentilo.web.catalog.domain.LngLat;
import org.sentilo.web.catalog.domain.Location;
import org.sentilo.web.catalog.dto.MapComponentDTO;

public class MapComponentDTOTest {

  private final String icon = "poi23";
  private final String id = "provider1.component23";
  private final double latitude = 45.345672;
  private final double longitude = 32.987654;

  @Mock
  private Component component;

  @Mock
  private Location location;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void newInstance() {
    final LngLat[] coordinates = {new LngLat(longitude, latitude)};
    final Double[] centroid = {latitude, longitude};
    when(component.getLocation()).thenReturn(location);
    when(location.getCoordinates()).thenReturn(coordinates);
    when(location.getCentroid()).thenReturn(centroid);
    when(component.getId()).thenReturn(id);

    final MapComponentDTO dto = new MapComponentDTO(component, icon);

    Assert.assertEquals(id, dto.getId());
    Assert.assertEquals(icon, dto.getIcon());
    Assert.assertTrue(dto.getCoordinates().length == coordinates.length);

  }

  @Test
  public void newInstanceWithNullLocation() {
    when(component.getLocation()).thenReturn(null);
    when(component.getId()).thenReturn(id);

    final MapComponentDTO dto = new MapComponentDTO(component, icon);

    Assert.assertEquals(id, dto.getId());
    Assert.assertNull(dto.getCoordinates());
    Assert.assertEquals(icon, dto.getIcon());
  }
}
