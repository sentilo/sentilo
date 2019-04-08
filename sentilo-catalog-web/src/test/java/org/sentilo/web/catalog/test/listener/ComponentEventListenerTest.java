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
package org.sentilo.web.catalog.test.listener;

import static org.mockito.Matchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Date;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;
import org.sentilo.web.catalog.domain.Component;
import org.sentilo.web.catalog.domain.LngLat;
import org.sentilo.web.catalog.domain.Location;
import org.sentilo.web.catalog.domain.RoutePoint;
import org.sentilo.web.catalog.domain.RoutePointList;
import org.sentilo.web.catalog.listener.ComponentEventListener;
import org.sentilo.web.catalog.service.ComponentService;
import org.springframework.data.mongodb.core.mapping.event.BeforeConvertEvent;

public class ComponentEventListenerTest {

  @Spy
  private Location location;

  @Mock
  private BeforeConvertEvent<Component> eventConvert;

  @Mock
  private Component component;

  @Mock
  ComponentService componentService;

  @InjectMocks
  private ComponentEventListener listener;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    when(component.getUpdatedAt()).thenReturn(new Date());
    when(eventConvert.getSource()).thenReturn(component);
  }

  @Test
  public void onBeforeConvertForStaticComponent() {
    final LngLat[] coordinates = {new LngLat(41.528, 2.134)};
    location.setCoordinates(coordinates);

    when(component.getLocation()).thenReturn(location);
    when(component.isMobileComponent()).thenReturn(false);

    listener.onBeforeConvert(eventConvert);

    verify(location).setCentroid(coordinates[0].toArray());
    verify(component, times(0)).setRoutePointList(any(RoutePointList.class));
    verify(component, times(0)).addLocationCandidate(any(Location.class));

    Assert.assertArrayEquals(coordinates, location.getCoordinates());
  }

  @Test
  public void onBeforeConvertForMobileComponent() {
    final LngLat[] coordinates = {new LngLat(41.528, 2.134)};
    location.setCoordinates(coordinates);

    when(component.getLocation()).thenReturn(location);
    when(component.isMobileComponent()).thenReturn(true);
    when(component.getRoutePointList()).thenReturn(null);

    listener.onBeforeConvert(eventConvert);

    verify(location).setCentroid(coordinates[0].toArray());
    verify(component, times(5)).getLocation();
    verify(component).setRoutePointList(any(RoutePointList.class));
    verify(component).addLocationCandidate(any(Location.class));
    Assert.assertArrayEquals(coordinates, location.getCoordinates());
  }

  @Test
  public void onBeforeConvertWithNewCoordinates() {
    final LngLat point = new LngLat(41.528, 2.134);
    final LngLat[] newCoordinates = {new LngLat(41.522, 2.137)};
    location.setCoordinates(newCoordinates);
    location.setFromTsTime(System.currentTimeMillis());

    final RoutePointList route = new RoutePointList();
    route.add(new RoutePoint(point, System.currentTimeMillis() - 10000));

    when(component.getLocation()).thenReturn(location);
    when(component.isMobileComponent()).thenReturn(true);
    when(component.getRoutePointList()).thenReturn(route);

    listener.onBeforeConvert(eventConvert);

    verify(location).setCentroid(newCoordinates[0].toArray());
    verify(location, times(1)).setCoordinates(any(LngLat[].class));
    verify(component, times(0)).setRoutePointList(route);
    verify(component).addLocationCandidate(location);
    Assert.assertArrayEquals(newCoordinates, location.getCoordinates());
  }

  @Test
  public void onBeforeConvertWithoutChangeCoordinates() {
    final LngLat point = new LngLat(41.528, 2.134);
    final LngLat[] coordinates = {point};
    location.setCoordinates(coordinates);

    final RoutePointList route = new RoutePointList();
    route.add(new RoutePoint(point, System.currentTimeMillis()));

    when(component.getLocation()).thenReturn(location);
    when(component.isMobileComponent()).thenReturn(true);
    when(component.getRoutePointList()).thenReturn(route);

    listener.onBeforeConvert(eventConvert);

    verify(location).setCentroid(coordinates[0].toArray());
    verify(component, times(5)).getLocation();
    verify(component, times(0)).setRoutePointList(route);
    Assert.assertTrue(route.getInternalList().size() == 1);
    Assert.assertArrayEquals(coordinates, location.getCoordinates());
  }

  @Test
  public void onBeforeConvertWithNullLocation() {
    when(component.getLocation()).thenReturn(null);

    listener.onBeforeConvert(eventConvert);

    verify(location, times(0)).setCentroid(any(Double[].class));
    verify(location, times(0)).setCoordinates(any(LngLat[].class));
    verify(component, times(0)).setRoutePointList(any(RoutePointList.class));
  }

  @Test
  public void onBeforeConvertForDuplicateConsecutiveCoordinates() {
    final LngLat[] coordinates = {new LngLat(41.528, 2.134), new LngLat(41.528, 2.134), new LngLat(40.123, 2.100)};
    final LngLat[] expectedCoordinates = {new LngLat(41.528, 2.134), new LngLat(40.123, 2.100)};
    location.setCoordinates(coordinates);
    when(component.getLocation()).thenReturn(location);

    listener.onBeforeConvert(eventConvert);

    verify(location).setCoordinates(expectedCoordinates);
  }
}
