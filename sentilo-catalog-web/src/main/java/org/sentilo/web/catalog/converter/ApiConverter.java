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
package org.sentilo.web.catalog.converter;

import java.beans.PropertyDescriptor;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.sentilo.common.domain.CatalogComponent;
import org.sentilo.common.domain.CatalogElement;
import org.sentilo.common.domain.CatalogSensor;
import org.sentilo.common.domain.SensorLocationElement;
import org.sentilo.common.domain.TechnicalDetails;
import org.sentilo.common.utils.SentiloUtils;
import org.sentilo.web.catalog.domain.Component;
import org.sentilo.web.catalog.domain.LngLat;
import org.sentilo.web.catalog.domain.Location;
import org.sentilo.web.catalog.domain.Sensor;
import org.sentilo.web.catalog.domain.Sensor.DataType;
import org.sentilo.web.catalog.utils.CatalogUtils;
import org.sentilo.web.catalog.utils.Constants;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.BeanWrapper;
import org.springframework.beans.BeanWrapperImpl;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

public abstract class ApiConverter {

  public static List<CatalogSensor> convertToCatalogSensorList(final List<Sensor> sensors, final List<Component> components) {
    final List<CatalogSensor> catalogSensors = new ArrayList<CatalogSensor>();

    for (final Sensor sensor : sensors) {

      final int pos = components.indexOf(new Component(sensor.getComponentId()));
      if (pos != -1) {
        final CatalogSensor catalogSensor = convertToCatalogSensor(sensor);
        final Component component = components.get(pos);
        catalogSensor.setComponent(component.getName());
        if (component.getLocation() != null) {
          catalogSensor.setLocation(CatalogUtils.locationToString(component.getLocation()));
        }
        if (StringUtils.hasText(component.getDescription())) {
          catalogSensor.setComponentDesc(component.getDescription());
        }
        if (StringUtils.hasText(component.getComponentType())) {
          catalogSensor.setComponentType(component.getComponentType());
        }
        if (component.getPublicAccess() != null) {
          catalogSensor.setComponentPublicAccess(component.getPublicAccess());
        }

        if (!CollectionUtils.isEmpty(component.getAdditionalInfo())) {
          catalogSensor.setComponentAdditionalInfo(component.getAdditionalInfo());
        }

        if (component.getTechnicalDetails() != null) {
          catalogSensor.setComponentTechnicalDetails(component.getTechnicalDetails());
        }

        catalogSensors.add(catalogSensor);
      }
    }

    return catalogSensors;
  }

  public static List<Sensor> buildSensorsFromCatalogSensors(final ApiConverterContext context) {
    final List<Sensor> sensors = new ArrayList<Sensor>();

    final List<CatalogSensor> catalogSensors = context.getMessage().getSensors();
    final boolean isUpdateAction = context.isUpdateAction();

    if (!CollectionUtils.isEmpty(catalogSensors)) {
      for (final CatalogSensor catalogSensor : catalogSensors) {
        final Sensor sensor = (isUpdateAction ? buildSensorToUpdate(catalogSensor, context) : buildNewSensor(catalogSensor, context));
        if (sensor != null) {
          sensors.add(sensor);
        }
      }
    }

    return sensors;
  }

  public static List<Component> buildComponentsFromCatalogComponents(final ApiConverterContext context) {
    final List<Component> components = new ArrayList<Component>();
    final boolean isUpdateAction = context.isUpdateAction();
    final List<? extends CatalogElement> resources = (isUpdateAction ? context.getMessage().getComponents() : context.getMessage().getSensors());

    if (!CollectionUtils.isEmpty(resources)) {
      for (final CatalogElement resource : resources) {
        final Component component = (isUpdateAction ? buildComponentToUpdate(resource, context) : buildNewComponent(resource, context));
        if (component != null && !components.contains(component)) {
          components.add(component);
        }
      }
    }

    return components;
  }

  /**
   * Build a ordered list of components that need update its location and/or its routeList (if
   * component is mobile) Order is fixed by the location timestamp. Older timestamps are first on
   * the list
   * 
   * @param context
   * @return
   */
  public static List<Component> buildMobileComponentsFromSensorLocationElements(final ApiConverterContext context) {
    final List<Component> components = new ArrayList<Component>();
    // Every entry from this set has the format <componentId>+<location Ts>
    final HashSet<String> componentsAddedKeys = new HashSet<String>();
    final List<SensorLocationElement> resources = context.getMessage().getLocations();

    // The resources list contains sensor locations, but as two sensors from the same component have
    // the same location, the list could contains different sensors with the same location and
    // timestamp related to the same component. The first thing to do is to remove these
    // "duplicate" sensors from the resources list (it is controlled by the componentsAddedKeys
    // Set).
    final List<Component> auxComponents = new ArrayList<Component>();
    if (!CollectionUtils.isEmpty(resources)) {
      for (final SensorLocationElement resource : resources) {
        final Component component = buildComponentWithLocationUpdated(resource, context);
        // component is added to the components lists if and only if is not null
        // and not exists another entry from the same component with equals location timestamp
        if (component != null && componentsAddedKeys.add(component.getId() + "." + component.getLocation().getFromTsTime().toString())) {
          auxComponents.add(component);
        }
      }
    }

    // Finally, only locations changes must be returned so entries that not modify the previous
    // location must be remove from the list
    final Map<String, LngLat> componentLastLocations = new HashMap<String, LngLat>();
    for (final Component component : auxComponents) {
      final LngLat previousComponentLocation = componentLastLocations.get(component.getId());
      if (!component.getLocation().getCoordinates()[0].equals(previousComponentLocation)) {
        componentLastLocations.put(component.getId(), component.getLocation().getCoordinates()[0]);
        components.add(component);
      }
    }

    return components;
  }

  private static Sensor buildNewSensor(final CatalogSensor catalogSensor, final ApiConverterContext context) {
    final String providerId = context.getProviderId();
    final String componentId = Component.buildId(providerId, catalogSensor.getComponent());
    final Sensor sensor = new Sensor(providerId, componentId, catalogSensor.getSensor());
    sensor.setDescription(catalogSensor.getDescription());
    sensor.setType(catalogSensor.getType());
    sensor.setUnit(catalogSensor.getUnit());
    sensor.setDataType(parseDataTypeValue(catalogSensor.getDataType()));

    if (catalogSensor.getPublicAccess() != null) {
      sensor.setPublicAccess(catalogSensor.getPublicAccess());
    }

    if (catalogSensor.getTimeZone() != null) {
      sensor.setTimeZone(catalogSensor.getTimeZone());
    }

    if (!CollectionUtils.isEmpty(catalogSensor.getAdditionalInfo())) {
      sensor.setAdditionalInfo(catalogSensor.getAdditionalInfo());
    }

    if (catalogSensor.getTechnicalDetails() != null) {
      sensor.setTechnicalDetails(catalogSensor.getTechnicalDetails());
    }

    sensor.setCreatedAt(new Date());
    sensor.setUpdateAt(new Date());
    return sensor;
  }

  private static Sensor buildSensorToUpdate(final CatalogSensor catalogSensor, final ApiConverterContext context) {
    final String providerId = context.getProviderId();
    final Sensor sensor = context.getSensorService().findByName(providerId, catalogSensor.getSensor());

    if (sensor != null) {
      if (SentiloUtils.stringIsNotEmptyOrNull(catalogSensor.getDataType())) {
        sensor.setDataType(parseDataTypeValue(catalogSensor.getDataType()));
      }

      if (SentiloUtils.stringIsNotEmptyOrNull(catalogSensor.getDescription())) {
        sensor.setDescription(catalogSensor.getDescription());
      }

      if (SentiloUtils.stringIsNotEmptyOrNull(catalogSensor.getType())) {
        sensor.setType(catalogSensor.getType());
      }

      if (SentiloUtils.stringIsNotEmptyOrNull(catalogSensor.getUnit())) {
        sensor.setUnit(catalogSensor.getUnit());
      }

      if (SentiloUtils.stringIsNotEmptyOrNull(catalogSensor.getTimeZone())) {
        sensor.setTimeZone(catalogSensor.getTimeZone());
      }

      if (catalogSensor.getPublicAccess() != null) {
        sensor.setPublicAccess(catalogSensor.getPublicAccess());
      }

      if (!CollectionUtils.isEmpty(catalogSensor.getAdditionalInfo())) {
        Map<String, String> sensorAdditionalInfoToUpdate = sensor.getAdditionalInfo();

        if (CollectionUtils.isEmpty(sensorAdditionalInfoToUpdate)) {
          sensorAdditionalInfoToUpdate = new HashMap<String, String>();
          sensor.setAdditionalInfo(sensorAdditionalInfoToUpdate);
        }

        sensorAdditionalInfoToUpdate.putAll(catalogSensor.getAdditionalInfo());
      }

      final TechnicalDetails srcTechDetails = catalogSensor.getTechnicalDetails();
      if (srcTechDetails != null) {
        if (sensor.getTechnicalDetails() == null) {
          sensor.setTechnicalDetails(srcTechDetails);
        } else {
          BeanUtils.copyProperties(srcTechDetails, sensor.getTechnicalDetails(), getNullPropertyNames(srcTechDetails));
        }
      }

      sensor.setUpdateAt(new Date());
    }

    return sensor;
  }

  private static CatalogSensor convertToCatalogSensor(final Sensor sensor) {
    final CatalogSensor catalogSensor = new CatalogSensor();
    catalogSensor.setSensor(sensor.getSensorId());
    catalogSensor.setType(sensor.getType());
    catalogSensor.setDataType(sensor.getDataType().name());
    catalogSensor.setUnit(sensor.getUnit());
    catalogSensor.setTimeZone(sensor.getTimeZone());
    catalogSensor.setPublicAccess(sensor.getPublicAccess());

    if (StringUtils.hasText(sensor.getDescription())) {
      catalogSensor.setDescription(sensor.getDescription());
    }

    if (!CollectionUtils.isEmpty(sensor.getAdditionalInfo())) {
      catalogSensor.setAdditionalInfo(sensor.getAdditionalInfo());
    }

    if (sensor.getTechnicalDetails() != null) {
      catalogSensor.setTechnicalDetails(sensor.getTechnicalDetails());
    }

    return catalogSensor;
  }

  private static DataType parseDataTypeValue(final String dataTypeValue) {
    DataType dataType = null;
    try {
      if (StringUtils.hasText(dataTypeValue)) {
        dataType = DataType.valueOf(dataTypeValue.toUpperCase());
      } else {
        // By default, sensor data type is set to NUMBER.
        dataType = DataType.NUMBER;
      }
    } catch (final IllegalArgumentException e) {
    }

    return dataType;
  }

  private static Component buildNewComponent(final CatalogElement resource, final ApiConverterContext context) {

    final CatalogSensor catalogSensor = (CatalogSensor) resource;

    // Convenciones para el registro de los componentes:
    // 1. En caso de que el nombre del componente no venga informado, por defecto se considera que
    // el componente tiene el mismo nombre que el sensor
    // De esta manera se cubre el caso de componentes con un único sensor.
    // 2. Ídem para el caso del tipo del componente: si no viene informado se fija el tipo a GENERIC
    // 3. Sólo se creará un componente en caso de que no exista previamente.

    if (!SentiloUtils.stringIsNotEmptyOrNull(catalogSensor.getComponent())) {
      catalogSensor.setComponent(catalogSensor.getSensor());
    }

    if (!SentiloUtils.stringIsNotEmptyOrNull(catalogSensor.getComponentType())) {
      catalogSensor.setComponentType(Constants.DEFAULT_COMPONENT_TYPE);
    }

    final String providerId = context.getProviderId();
    final String name = catalogSensor.getComponent();

    if (context.getComponentService().findByName(providerId, name) == null) {
      final Component component = new Component();
      component.setProviderId(context.getProviderId());
      component.setName(catalogSensor.getComponent());
      component.setComponentType(catalogSensor.getComponentType());

      if (StringUtils.hasText(catalogSensor.getLocation())) {
        component.setLocation(CatalogUtils.convertStringLocation(catalogSensor.getLocation()));
        component.setMobile(Constants.STATIC);
      } else {
        component.setMobile(Constants.MOBILE);
      }

      if (SentiloUtils.stringIsNotEmptyOrNull(catalogSensor.getComponentDesc())) {
        component.setDescription(catalogSensor.getComponentDesc());
      }

      if (catalogSensor.getComponentPublicAccess() != null) {
        component.setPublicAccess(catalogSensor.getComponentPublicAccess());
      }

      if (!CollectionUtils.isEmpty(catalogSensor.getComponentAdditionalInfo())) {
        component.setAdditionalInfo(catalogSensor.getComponentAdditionalInfo());
      }

      if (catalogSensor.getComponentTechnicalDetails() != null) {
        component.setTechnicalDetails(catalogSensor.getComponentTechnicalDetails());
      }

      component.setId(Component.buildId(context.getProviderId(), component.getName()));
      component.setCreatedAt(new Date());
      component.setUpdateAt(new Date());

      return component;
    } else {
      return null;
    }
  }

  private static Component buildComponentToUpdate(final CatalogElement resource, final ApiConverterContext context) {
    final CatalogComponent catalogComponent = (CatalogComponent) resource;

    final Component component = context.getComponentService().findByName(context.getProviderId(), catalogComponent.getComponent());

    if (component != null) {
      if (SentiloUtils.stringIsNotEmptyOrNull(catalogComponent.getComponentDesc())) {
        component.setDescription(catalogComponent.getComponentDesc());
      }

      if (SentiloUtils.stringIsNotEmptyOrNull(catalogComponent.getComponentType())) {
        component.setComponentType(catalogComponent.getComponentType());
      }

      if (SentiloUtils.stringIsNotEmptyOrNull(catalogComponent.getLocation())) {
        component.setLocation(CatalogUtils.convertStringLocation(catalogComponent.getLocation()));
      }

      if (catalogComponent.getComponentPublicAccess() != null) {
        component.setPublicAccess(catalogComponent.getComponentPublicAccess());
      }

      if (!CollectionUtils.isEmpty(catalogComponent.getComponentAdditionalInfo())) {
        Map<String, String> componentAdditionalInfoToUpdate = component.getAdditionalInfo();

        if (CollectionUtils.isEmpty(componentAdditionalInfoToUpdate)) {
          componentAdditionalInfoToUpdate = new HashMap<String, String>();
          component.setAdditionalInfo(componentAdditionalInfoToUpdate);
        }

        componentAdditionalInfoToUpdate.putAll(catalogComponent.getComponentAdditionalInfo());
      }

      final TechnicalDetails srcTechDetails = catalogComponent.getComponentTechnicalDetails();
      if (srcTechDetails != null) {
        if (component.getTechnicalDetails() == null) {
          component.setTechnicalDetails(srcTechDetails);
        } else {
          BeanUtils.copyProperties(srcTechDetails, component.getTechnicalDetails(), getNullPropertyNames(srcTechDetails));
        }
      }

      component.setUpdateAt(new Date());
    }

    return component;
  }

  private static Component buildComponentWithLocationUpdated(final SensorLocationElement resource, final ApiConverterContext context) {
    Component component = null;
    boolean locationUpdated = false;
    final Sensor sensor = context.getSensorService().findByName(resource.getProvider(), resource.getSensor());
    if (sensor != null) {
      component = context.getComponentService().find(new Component(sensor.getComponentId()));
      if (locationMustToBeUpdated(component, resource)) {
        final Location newLocation = CatalogUtils.convertStringLocation(resource.getLocation());
        newLocation.setFromTsTime(resource.getFromTsTime());
        component.setLocation(newLocation);
        component.setUpdateAt(new Date());
        locationUpdated = true;
      }
    }

    return (locationUpdated ? component : null);
  }

  /**
   * Component location must be updated if and only if it is mobile and its timestamp is older than
   * resource timestamp
   * 
   * @param component
   * @param resource
   * @return
   */
  private static boolean locationMustToBeUpdated(final Component component, final SensorLocationElement resource) {
    boolean locationMustToBeUpdated = false;
    if (component != null && component.isMobileComponent() && resource.getFromTsTime() != null) {
      final Long newLocTs = resource.getFromTsTime();
      final Location currentLocation = component.getLocation();
      if (currentLocation == null || currentLocation.getFromTsTime() == null || currentLocation.getFromTsTime().compareTo(newLocTs) < 0) {
        locationMustToBeUpdated = true;
      }
    }

    return locationMustToBeUpdated;
  }

  private static String[] getNullPropertyNames(final Object source) {
    final BeanWrapper src = new BeanWrapperImpl(source);
    final PropertyDescriptor[] pds = src.getPropertyDescriptors();

    final Set<String> emptyNames = new HashSet<String>();
    for (final PropertyDescriptor pd : pds) {
      final Object srcValue = src.getPropertyValue(pd.getName());
      if (srcValue == null) {
        emptyNames.add(pd.getName());
      }
    }
    final String[] result = new String[emptyNames.size()];
    return emptyNames.toArray(result);
  }

  private ApiConverter() {
    // this prevents even the native class from calling this ctor as well :
    throw new AssertionError();
  }
}
