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
package org.sentilo.web.catalog.listener;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
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
import org.springframework.data.mongodb.core.mapping.event.BeforeConvertEvent;

@org.springframework.stereotype.Component
public class ComponentEventListener extends AbstractMongoEventListener<Component> {

  @Autowired
  private ComponentService componentService;

  @Value("${route.max.size}")
  private int routeMaxSize;

  @Override
  public void onBeforeConvert(final BeforeConvertEvent<Component> event) {
    changeLocationAndRouteIfNeeded(event.getSource());
  }

  /**
   * Component location in backend must be changed if, and only if, new location is different from
   * existing location and its timestamp is greater than existing location timestamp. Additionally,
   * new location could be inserted into route list if need be.
   *
   * @param newComponent
   */
  private void changeLocationAndRouteIfNeeded(final Component newComponent) {
    // Before convert component to be persisted, we must do the following steps if new location is
    // not null:
    // 1. Remove duplicate consecutive coordinates from the location (does not make sense to
    // duplicate consecutive coordinates)
    // 2. Verify that new location changes old location (i.e has different value and its timestamp
    // is greater than old location timestamp). If so, then build (or rebuild) the centroid
    // attribute with the new location info
    // Likewise, if component is mobile and has candidates to be route members, update route list if
    // need be

    if (newComponent.getLocation() != null) {
      removeDuplicateConsecutiveCoords(newComponent);
      updateLocationIfNeedBe(newComponent);

      if (newComponent.isMobileComponent()) {
        updateRouteIfNeedBe(newComponent);
      }
    }
  }

  /**
   * Verifies if current component location saved into backend must be changed for the new component
   * location. If so, then component location centroid must be rebuild. Otherwise, new component
   * location is replaced with the current location saved into backend
   */
  private void updateLocationIfNeedBe(final Component newComponent) {
    boolean locationMustBeUpdated = true;
    final Location newLocation = newComponent.getLocation();

    // If newLocation timestamp is not filled in, it takes its value from the component update
    // time field
    if (newLocation.getFromTsTime() == null) {
      newLocation.setFromTsTime(newComponent.getUpdatedAt().getTime());
    }

    // Get the current component info from backend if it exists
    final Component currentComponent = componentService.find(newComponent);

    if (currentComponent != null && currentComponent.getLocation() != null) {
      final Location currentLocation = currentComponent.getLocation();
      final Long newLocationTs = newLocation.getFromTsTime();
      final Long currentLocationTs =
          currentLocation.getFromTsTime() != null ? currentLocation.getFromTsTime() : currentComponent.getUpdatedAt().getTime();

      if (currentLocation.getFromTsTime() == null) {
        currentLocation.setFromTsTime(currentComponent.getUpdatedAt().getTime());
      }

      // If coordinates are equals or new location hasn't a timestamp greater than current location
      // timestamp, location saved into backend mustn't be updated when component will be saved
      if (Arrays.equals(currentLocation.getCoordinates(), newLocation.getCoordinates()) || newLocationTs <= currentLocationTs) {
        // Changed new component location for current location
        newComponent.setLocation(currentLocation);
        locationMustBeUpdated = false;
      }
    }

    if (locationMustBeUpdated) {
      CentroidBuilder.build(newLocation);
    }
  }

  private void removeDuplicateConsecutiveCoords(final Component component) {
    final Location location = component.getLocation();
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

  private void updateRouteIfNeedBe(final Component component) {
    // Routes not have sense for components with a set of coordinates
    if (component.getLocation().getNumberOfCoordinates() != 1) {
      return;
    }

    RoutePointList route = component.getRoutePointList();

    if (route == null) {
      // Try to retrieve routePointList from the backend or to set new one if not exists yet
      final Component aux = componentService.find(component);
      route = aux != null && aux.getRoutePointList() != null ? aux.getRoutePointList() : new RoutePointList(routeMaxSize);
      component.setRoutePointList(route);
    }

    // If it's not member yet, the current location always is a candidate to be a route member (is
    // candidate to be the route head)
    component.addLocationCandidate(component.getLocation());

    // For each location route point candidate, validates if must be a route member
    final Iterator<Location> it = component.getLocationCandidates().iterator();
    while (it.hasNext()) {
      route.add(new RoutePoint(it.next()));
    }
  }

}
