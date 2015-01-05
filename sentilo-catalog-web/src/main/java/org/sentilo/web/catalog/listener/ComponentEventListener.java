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
package org.sentilo.web.catalog.listener;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.sentilo.web.catalog.domain.Component;
import org.sentilo.web.catalog.domain.LngLat;
import org.sentilo.web.catalog.domain.Location;
import org.sentilo.web.catalog.domain.RoutePoint;
import org.sentilo.web.catalog.domain.RoutePointList;
import org.sentilo.web.catalog.service.ComponentService;
import org.sentilo.web.catalog.utils.CentroidBuilder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.mongodb.core.mapping.event.AbstractMongoEventListener;

@org.springframework.stereotype.Component
public class ComponentEventListener extends AbstractMongoEventListener<Component> {

  @Autowired
  private ComponentService componentService;

  @Value("${route.max.size}")
  private int routeMaxSize;

  @Override
  public void onBeforeConvert(final Component component) {
    // Before convert component to be persisted, we must do the following steps if location is not
    // null:
    // 1. Remove duplicate consecutive coordinates from the location (does not make sense to
    // duplicate consecutive coordinates)
    // 2. Build(or rebuild) the centroid attribute with the location info
    // 3. Verify that location fromTsTime is fill in
    // 4. Likewise, if location has changed and component is mobile, update route locations with
    // this new one
    if (component.getLocation() != null) {
      removeDuplicateConsecutiveCoords(component.getLocation());
      CentroidBuilder.build(component.getLocation());
      validateLocationFromTsTime(component);
      if (component.isMobileComponent()) {
        storeRouteIfNecessary(component);
      }
    }
  }

  private void validateLocationFromTsTime(final Component component) {
    // If location has changed and fromTsTime is null, initialize it with component update time
    if (component.getLocation().getFromTsTime() == null) {
      final Component aux = componentService.find(component);
      if (aux == null || aux.getLocation() == null || !Arrays.equals(aux.getLocation().getCoordinates(), component.getLocation().getCoordinates())) {
        component.getLocation().setFromTsTime(component.getUpdateAt().getTime());
      }
    }

  }

  private void removeDuplicateConsecutiveCoords(final Location location) {
    if (location.getNumberOfCoordinates() != 1) {
      final List<LngLat> newCoordinates = new ArrayList<LngLat>();
      LngLat previousCoords = null;
      boolean existDuplicates = false;
      for (final LngLat currentCoordinates : location.getCoordinates()) {
        if (currentCoordinates.equals(previousCoords)) {
          existDuplicates = true;
          continue;
        }

        newCoordinates.add(currentCoordinates);
        previousCoords = currentCoordinates;
      }

      if (existDuplicates) {
        location.setCoordinates(newCoordinates.toArray(new LngLat[0]));
      }
    }
  }

  private void storeRouteIfNecessary(final Component component) {
    // Routes not have sense for components with a set of coordinates
    if (component.getLocation().getNumberOfCoordinates() != 1) {
      return;
    }

    RoutePointList route = component.getRoutePointList();

    if (route == null) {
      // Try to retrieve routePointList from the backend or to set new one if not exists yet
      final Component aux = componentService.find(component);
      route = (aux != null && aux.getRoutePointList() != null ? aux.getRoutePointList() : new RoutePointList(routeMaxSize));
      component.setRoutePointList(route);
    }

    // Retrieve the more recent position from the route
    final RoutePoint lastLocation = route.peek();

    final LngLat currentLocation = component.getLocation().getCoordinates()[0];
    if (lastLocation == null || !currentLocation.equals(lastLocation.getLocation())) {
      route.add(new RoutePoint(currentLocation, component.getLocation().getFromTsTime()));
    }
  }

}
