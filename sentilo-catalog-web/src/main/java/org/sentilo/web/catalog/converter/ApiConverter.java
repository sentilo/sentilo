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
package org.sentilo.web.catalog.converter;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.sentilo.common.domain.CatalogComponent;
import org.sentilo.common.domain.CatalogElement;
import org.sentilo.common.domain.CatalogSensor;
import org.sentilo.common.domain.SensorMessageLocation;
import org.sentilo.common.enums.SensorState;
import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.web.catalog.domain.Component;
import org.sentilo.web.catalog.domain.Location;
import org.sentilo.web.catalog.domain.RoutePointCandidate;
import org.sentilo.web.catalog.domain.Sensor;
import org.sentilo.web.catalog.domain.Sensor.DataType;
import org.sentilo.web.catalog.utils.LocationUtils;
import org.springframework.util.CollectionUtils;

public final class ApiConverter {

  private ApiConverter() {
    // this prevents even the native class from calling this ctor as well :
    throw new AssertionError();
  }

  public static final List<CatalogSensor> convertToCatalogSensorList(final List<Sensor> sensors, final List<Component> components) {
    final List<CatalogSensor> catalogSensors = new ArrayList<CatalogSensor>();

    for (final Sensor sensor : sensors) {

      final int pos = components.indexOf(new Component(sensor.getComponentId()));
      if (pos != -1) {
        final Component component = components.get(pos);
        final CatalogSensor catalogSensor = convertToCatalogSensor(sensor);
        final CatalogComponent catalogComponent = convertToCatalogComponent(component);
        CatalogDocumentConverter.copyProperties(catalogComponent, catalogSensor);
        catalogSensors.add(catalogSensor);
      }
    }

    return catalogSensors;
  }

  public static final List<Sensor> buildSensorsFromCatalogSensors(final ApiConverterContext context) {
    final List<Sensor> sensors = new ArrayList<Sensor>();
    final List<CatalogSensor> catalogSensors = context.getMessage().getSensors();

    if (!CollectionUtils.isEmpty(catalogSensors)) {
      for (final CatalogSensor catalogSensor : catalogSensors) {
        final Sensor sensor = buildSensor(catalogSensor, context);
        if (sensor != null) {
          sensors.add(sensor);
        }
      }
    }

    return sensors;
  }

  public static final List<Component> buildComponentsFromCatalogComponents(final ApiConverterContext context) {
    final List<Component> components = new ArrayList<Component>();
    final boolean isUpdateAction = context.isUpdateAction();
    final List<? extends CatalogElement> resources = isUpdateAction ? context.getMessage().getComponents() : context.getMessage().getSensors();

    if (!CollectionUtils.isEmpty(resources)) {
      for (final CatalogElement resource : resources) {
        final Component component = isUpdateAction ? buildComponentToUpdate(resource, context) : buildNewComponent(resource, context);
        if (component != null && !components.contains(component)) {
          components.add(component);
        }
      }
    }

    return components;
  }

  /**
   * Build a new RoutePointCandidate from each SensorMessageLocation and add it to list of route
   * points candidates of its related component.
   *
   * @param context
   * @return
   */
  public static final List<Component> addRoutePointsCandidates(final ApiConverterContext context) {
    final Map<Component, List<RoutePointCandidate>> rpCandidates = buildComponentRoutePointCandidates(context);

    // Finally, translate each component's route point list candidate into
    // locationRoutePointsCandidates field of each component instance.
    final List<Component> components = rpCandidates.entrySet().stream().map(e ->
      {
        e.getValue().forEach(rpc -> e.getKey().addLocationCandidate(rpc.getLocationCandidate()));
        return e.getKey();
      }).collect(Collectors.toList());
    return components;
  }

  /**
   * This method returns a list of location candidates to be part of a component route. Result is
   * build following these steps:
   * <li>Step 1: Remove wrong locations values and duplicates tuples (provider,sensor,ts)
   * <li>Step 2: Add component to each element
   * <li>Step 3: and remove duplicates tuples (component,ts) because a component only can be in one
   * place in a given instant
   * <li>Step 4: Finally, candidates are grouped by component.
   *
   *
   * @param context
   * @return
   */
  private static Map<Component, List<RoutePointCandidate>> buildComponentRoutePointCandidates(final ApiConverterContext context) {

    final List<SensorMessageLocation> sensorLocationsSource = context.getMessage().getLocations();
    final String smlKeyTemplate = "%s" + SentiloConstants.SENTILO_INTERNAL_TOKEN + "%s" + SentiloConstants.SENTILO_INTERNAL_TOKEN + "%s";

    final Set<String> aux = new HashSet<String>();

    // Step 1: filter sensorLocationsSource list removing wrong locations values and duplicates
    // tuples (provider,sensor,ts)
    final List<SensorMessageLocation> stp1List = sensorLocationsSource.stream().filter(e ->
      {
        final Location location = LocationUtils.parse(e.getLocation());
        final String eKey = String.format(smlKeyTemplate, e.getProvider(), e.getSensor(), e.getFromTsTime());

        final boolean exists = aux.contains(eKey);
        if (!exists) {
          aux.add(eKey);
        }
        // element should be rejected if it already exists or its location isn't valid
        return !exists && LocationUtils.isValid(location);
      }).collect(Collectors.toList());

    // Step 2: transform list to Map of type <RoutePointCandidate,Component>
    // final Map<RoutePointCandidate, Component> stp2Map =
    // stp1List.stream().collect(Collectors.toMap(e -> e, e -> getComponent(context, e)));
    final Map<RoutePointCandidate, Component> stp2Map = stp1List.stream()
        .collect(Collectors.toMap(e -> new RoutePointCandidate(getComponent(context, e), LocationUtils.parse(e.getLocation(), e.getFromTsTime())),
            e -> getComponent(context, e)));

    // Step 3: filter duplicates component's locations
    aux.clear();
    final Map<RoutePointCandidate, Component> stp3Map = stp2Map.entrySet().stream().filter(e ->
      {
        final String eKey = String.format(smlKeyTemplate, e.getValue().getProviderId(), e.getValue().getId(), e.getKey().getFromTsTime());
        final boolean exists = aux.contains(eKey);
        if (!exists) {
          aux.add(eKey);
        }
        return !exists;
      }).collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));

    // Step 4: group by component
    final Map<Component, List<RoutePointCandidate>> stp4Map =
        stp3Map.entrySet().stream().collect(Collectors.groupingBy(Map.Entry<RoutePointCandidate, Component>::getValue,
            Collectors.mapping(Map.Entry<RoutePointCandidate, Component>::getKey, Collectors.toList())));

    return stp4Map;
  }

  private static final Component getComponent(final ApiConverterContext context, final SensorMessageLocation resource) {
    Component component = null;

    final Sensor sensor = context.getSensorService().findByName(resource.getProvider(), resource.getSensor());
    if (sensor != null) {
      component = context.getComponentService().findById(sensor.getComponentId());
    }

    return component;
  }

  private static final Sensor buildSensor(final CatalogSensor catalogSensor, final ApiConverterContext context) {
    Sensor sensor;
    final String providerId = context.getProviderId();

    if ((sensor = context.getSensorService().findByName(providerId, catalogSensor.getSensor())) == null) {
      // create new sensor case
      final CatalogComponent catalogComponent = CatalogDocumentConverter.extractCatalogComponent(catalogSensor);
      final String componentId = Component.buildId(providerId, catalogComponent.getComponent());
      sensor = new Sensor(providerId, componentId, catalogSensor.getSensor());

      // By default, sensor's dataType is NUMBER and its state is online
      sensor.setDataType(DataType.NUMBER);
      sensor.setState(SensorState.online);

      // Copy properties from catalogSensor to sensor
      CatalogDocumentConverter.copyProperties(catalogSensor, sensor);
    } else { // update sensor case
      // Copy properties from catalogSensor to sensor
      CatalogDocumentConverter.copyProperties(catalogSensor, sensor);
    }

    return sensor;
  }

  private static final CatalogSensor convertToCatalogSensor(final Sensor sensor) {
    final CatalogSensor catalogSensor = new CatalogSensor();
    CatalogDocumentConverter.copyProperties(sensor, catalogSensor);
    return catalogSensor;
  }

  private static final CatalogComponent convertToCatalogComponent(final Component component) {
    final CatalogComponent catalogComponent = new CatalogComponent();
    CatalogDocumentConverter.copyProperties(component, catalogComponent);
    return catalogComponent;
  }

  private static final Component buildNewComponent(final CatalogElement resource, final ApiConverterContext context) {
    Component component = null;
    final CatalogComponent catalogComponent = CatalogDocumentConverter.extractCatalogComponent((CatalogSensor) resource);
    final String providerId = context.getProviderId();
    final String name = catalogComponent.getComponent();

    if (context.getComponentService().findByName(providerId, name) == null) {
      final String componentId = Component.buildId(providerId, name);
      component = new Component(componentId);

      CatalogDocumentConverter.copyProperties(catalogComponent, component);
    }

    return component;
  }

  private static final Component buildComponentToUpdate(final CatalogElement resource, final ApiConverterContext context) {
    final CatalogComponent catalogComponent = (CatalogComponent) resource;
    final Component component = context.getComponentService().findByName(context.getProviderId(), catalogComponent.getComponent());

    if (component != null) {
      CatalogDocumentConverter.copyProperties(catalogComponent, component);
    }

    return component;
  }

}
