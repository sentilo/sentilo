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
import static org.mockito.Matchers.argThat;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Date;
import java.util.HashSet;
import java.util.Set;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentMatcher;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;
import org.sentilo.web.catalog.domain.Component;
import org.sentilo.web.catalog.domain.LngLat;
import org.sentilo.web.catalog.domain.Location;
import org.sentilo.web.catalog.domain.RoutePointList;
import org.sentilo.web.catalog.listener.ComponentEventListener;
import org.sentilo.web.catalog.service.ComponentService;
import org.springframework.data.mongodb.core.mapping.event.BeforeConvertEvent;
import org.springframework.test.util.ReflectionTestUtils;

public class ComponentEventListenerTest {

  private static final String COMPONENT_ID = "mockProviderId.mockComponentId";
  private static int ROUTE_MAX_SIZE = 10;

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
    when(component.getId()).thenReturn(COMPONENT_ID);
    when(eventConvert.getSource()).thenReturn(component);
    ReflectionTestUtils.setField(listener, "routeMaxSize", ROUTE_MAX_SIZE);
  }

  @Test
  public void onBeforeConvertForStaticComponent() {
    final Component currentComponent = Mockito.mock(Component.class);
    final LngLat[] coordinates = {new LngLat(41.528, 2.134)};
    location.setCoordinates(coordinates);

    when(componentService.findById(component.getId())).thenReturn(currentComponent);
    when(currentComponent.hasLocation()).thenReturn(false);
    when(component.getLocation()).thenReturn(location);
    when(component.isMobileComponent()).thenReturn(false);

    listener.onBeforeConvert(eventConvert);

    verify(component, times(0)).setRoutePointList(any(RoutePointList.class));
    verify(component, times(0)).addLocationCandidate(any(Location.class));

    Assert.assertArrayEquals(coordinates, location.getCoordinates());
  }

  @Test
  public void onBeforeConvertForMobileComponent() {
    final Component currentComponent = Mockito.mock(Component.class);
    final LngLat[] coordinates = {new LngLat(41.528, 2.134)};
    location.setCoordinates(coordinates);

    when(componentService.find(component)).thenReturn(currentComponent);
    when(currentComponent.hasLocation()).thenReturn(false);
    when(component.getRoutePointList()).thenReturn(null);
    when(component.hasLocation()).thenReturn(true);
    when(component.getLocation()).thenReturn(location);
    when(component.isMobileComponent()).thenReturn(true);
    when(component.getRoutePointList()).thenReturn(null);

    listener.onBeforeConvert(eventConvert);

    verify(component, times(4)).getLocation();
    verify(component).setRoutePointList(any(RoutePointList.class));
    verify(component).addLocationCandidate(location);
    Assert.assertArrayEquals(coordinates, location.getCoordinates());
  }

  @Test
  public void onBeforeConvertWithNewCoordinates() {
    final Component currentComponent = Mockito.mock(Component.class);
    final LngLat point = new LngLat(41.528, 2.134);
    final LngLat[] newCoordinates = {new LngLat(41.522, 2.137)};
    location.setCoordinates(newCoordinates);
    location.setFromTsTime(System.currentTimeMillis());
    final Location currentLocation = new Location(point);
    currentLocation.setFromTsTime(System.currentTimeMillis() - 10000);

    when(componentService.findById(component.getId())).thenReturn(currentComponent);
    when(currentComponent.getLocation()).thenReturn(currentLocation);
    when(currentComponent.hasLocation()).thenReturn(true);
    when(component.hasLocation()).thenReturn(true);
    when(component.getLocation()).thenReturn(location);
    when(component.isMobileComponent()).thenReturn(false);

    listener.onBeforeConvert(eventConvert);

    Assert.assertArrayEquals(newCoordinates, location.getCoordinates());
  }

  @Test
  public void onBeforeConvertWithOldCoordinates() {
    final Component currentComponent = Mockito.mock(Component.class);
    final LngLat point = new LngLat(41.528, 2.134);
    final LngLat[] newCoordinates = {new LngLat(41.522, 2.137)};
    location.setCoordinates(newCoordinates);
    location.setFromTsTime(System.currentTimeMillis() - 20000);
    final Location currentLocation = new Location(point);
    currentLocation.setFromTsTime(System.currentTimeMillis());

    when(componentService.findById(component.getId())).thenReturn(currentComponent);
    when(currentComponent.getLocation()).thenReturn(currentLocation);
    when(currentComponent.hasLocation()).thenReturn(true);
    when(component.hasLocation()).thenReturn(true);
    when(component.getLocation()).thenReturn(location);
    when(component.isMobileComponent()).thenReturn(false);

    listener.onBeforeConvert(eventConvert);

    verify(component).setLocation(currentLocation);
  }

  @Test
  public void onBeforeConvert_with_new_mobile_component() {
    final long currentTime = System.currentTimeMillis();
    final LngLat point = new LngLat(41.528, 2.134);
    final LngLat[] coordinates = {point};
    final LngLat newPoint = new LngLat(41.522, 2.137);
    location.setCoordinates(coordinates);
    location.setFromTsTime(currentTime - 60 * 60 * 1000);

    final Set<Location> locationRoutePointsCandidates = new HashSet<>();
    final Location candidatePoint = new Location(newPoint);
    candidatePoint.setFromTsTime(currentTime);
    locationRoutePointsCandidates.add(candidatePoint);

    when(componentService.find(component)).thenReturn(null);
    when(component.hasLocation()).thenReturn(true);
    when(component.getLocation()).thenReturn(location);
    when(component.isMobileComponent()).thenReturn(true);
    when(component.getLocationCandidates()).thenReturn(locationRoutePointsCandidates);

    listener.onBeforeConvert(eventConvert);

    verify(component).setLocation(argThat(new LocationMatcher(candidatePoint)));
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
  public void onBeforeConvert_with_duplicate_consecutive_coordinates() {
    final LngLat[] coordinates = {new LngLat(41.528, 2.134), new LngLat(41.528, 2.134), new LngLat(40.123, 2.100)};
    final LngLat[] expectedCoordinates = {new LngLat(41.528, 2.134), new LngLat(40.123, 2.100)};
    location.setCoordinates(coordinates);
    when(component.hasLocation()).thenReturn(true);
    when(component.getLocation()).thenReturn(location);

    listener.onBeforeConvert(eventConvert);

    verify(location).setCoordinates(expectedCoordinates);
  }

  class LocationMatcher extends ArgumentMatcher<Location> {

    final Location expected;

    public LocationMatcher(final Location expected) {
      this.expected = expected;
    }

    @Override
    public boolean matches(final Object obj) {
      final Location actual = (Location) obj;

      return Arrays.equals(expected.getCoordinates(), actual.getCoordinates());
    }

  }
}
