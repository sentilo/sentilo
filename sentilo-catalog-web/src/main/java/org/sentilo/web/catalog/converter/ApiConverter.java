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
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import org.sentilo.common.domain.CatalogComponent;
import org.sentilo.common.domain.CatalogElement;
import org.sentilo.common.domain.CatalogSensor;
import org.sentilo.common.domain.SensorLocationElement;
import org.sentilo.common.enums.SensorState;
import org.sentilo.web.catalog.domain.Component;
import org.sentilo.web.catalog.domain.Location;
import org.sentilo.web.catalog.domain.Sensor;
import org.sentilo.web.catalog.domain.Sensor.DataType;
import org.sentilo.web.catalog.utils.CatalogUtils;
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
    final boolean isUpdateAction = context.isUpdateAction();

    if (!CollectionUtils.isEmpty(catalogSensors)) {
      for (final CatalogSensor catalogSensor : catalogSensors) {
        final Sensor sensor = isUpdateAction ? buildSensorToUpdate(catalogSensor, context) : buildNewSensor(catalogSensor, context);
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
   * Translate SensorLocationElement list into a Component list where each component will have a
   * list of location route point candidates. Every SensorLocationElement is parsed into a new
   * location candidate
   *
   * @param context
   * @return
   */
  public static final List<Component> buildMobileComponentsFromSensorLocationElements(final ApiConverterContext context) {
    // Each component in components list will have a list of location route point candidates.
    final List<Component> components = new ArrayList<Component>();
    final List<SensorLocationElement> resources = context.getMessage().getLocations();

    if (!CollectionUtils.isEmpty(resources)) {
      for (final SensorLocationElement resource : resources) {
        final Component auxComponent = getComponent(context, resource);
        final Location locationCandidate = buildLocationCandidate(resource);
        if (auxComponent != null) {
          if (!components.contains(auxComponent)) {
            components.add(auxComponent);
          }

          final Component component = components.get(components.indexOf(auxComponent));
          // add candidate to list only if component contains no element location such that
          // location.equals(locationCandidate)
          component.addLocationCandidate(locationCandidate);
        }
      }
    }

    // Finally, the location candidate with the maximum timestamp (i.e. the last location published)
    // will be the candidate to be the new component location
    for (final Component component : components) {
      final List<Location> locationCandidates = new ArrayList<Location>(component.getLocationCandidates());
      if (!CollectionUtils.isEmpty(locationCandidates)) {
        Collections.sort(locationCandidates, new Comparator<Location>() {

          @Override
          public int compare(final Location o1, final Location o2) {
            return o1.getFromTsTime().compareTo(o2.getFromTsTime());
          }
        });

        component.setLocation(locationCandidates.get(locationCandidates.size() - 1));
      }
    }

    return components;
  }

  private static final Component getComponent(final ApiConverterContext context, final SensorLocationElement resource) {
    Component component = null;

    final Sensor sensor = context.getSensorService().findByName(resource.getProvider(), resource.getSensor());
    if (sensor != null) {
      component = context.getComponentService().find(new Component(sensor.getComponentId()));
    }

    return component;
  }

  private static final Location buildLocationCandidate(final SensorLocationElement resource) {
    final Location newLocation = CatalogUtils.convertStringLocation(resource.getLocation());
    newLocation.setFromTsTime(resource.getFromTsTime());
    return newLocation;
  }

  private static final Sensor buildNewSensor(final CatalogSensor catalogSensor, final ApiConverterContext context) {
    final String providerId = context.getProviderId();
    final CatalogComponent catalogComponent = CatalogDocumentConverter.extractCatalogComponent(catalogSensor);
    final String componentId = Component.buildId(providerId, catalogComponent.getComponent());
    Sensor sensor = null;

    if (context.getSensorService().findByName(providerId, catalogSensor.getSensor()) == null) {
      sensor = new Sensor(providerId, componentId, catalogSensor.getSensor());

      // By default, sensor's dataType is NUMBER and its state is online
      sensor.setDataType(DataType.NUMBER);
      sensor.setState(SensorState.online);

      // Copy properties from catalogSensor to sensor
      CatalogDocumentConverter.copyProperties(catalogSensor, sensor);
    }
    return sensor;
  }

  private static final Sensor buildSensorToUpdate(final CatalogSensor catalogSensor, final ApiConverterContext context) {
    final String providerId = context.getProviderId();
    final Sensor sensor = context.getSensorService().findByName(providerId, catalogSensor.getSensor());

    if (sensor != null) {
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
