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

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.sentilo.common.domain.CatalogComponent;
import org.sentilo.common.domain.CatalogElement;
import org.sentilo.common.domain.CatalogSensor;
import org.sentilo.web.catalog.domain.Component;
import org.sentilo.web.catalog.domain.Sensor;
import org.sentilo.web.catalog.domain.Sensor.DataType;
import org.sentilo.web.catalog.utils.CatalogUtils;
import org.sentilo.web.catalog.utils.Constants;
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

    sensor.setCreatedAt(new Date());
    sensor.setUpdateAt(new Date());
    return sensor;
  }

  private static Sensor buildSensorToUpdate(final CatalogSensor catalogSensor, final ApiConverterContext context) {
    final String providerId = context.getProviderId();
    final Sensor sensor = context.getSensorService().findByName(providerId, catalogSensor.getSensor());

    if (sensor != null) {
      if (CatalogUtils.stringIsNotEmptyOrNull(catalogSensor.getDataType())) {
        sensor.setDataType(parseDataTypeValue(catalogSensor.getDataType()));
      }

      if (CatalogUtils.stringIsNotEmptyOrNull(catalogSensor.getDescription())) {
        sensor.setDescription(catalogSensor.getDescription());
      }

      if (CatalogUtils.stringIsNotEmptyOrNull(catalogSensor.getType())) {
        sensor.setType(catalogSensor.getType());
      }

      if (CatalogUtils.stringIsNotEmptyOrNull(catalogSensor.getUnit())) {
        sensor.setUnit(catalogSensor.getUnit());
      }

      if (CatalogUtils.stringIsNotEmptyOrNull(catalogSensor.getTimeZone())) {
        sensor.setTimeZone(catalogSensor.getTimeZone());
      }

      if (catalogSensor.getPublicAccess() != null) {
        sensor.setPublicAccess(catalogSensor.getPublicAccess());
      }

      if (!CollectionUtils.isEmpty(catalogSensor.getAdditionalInfo())) {
        Map<String, String> sensorAdditionalInfoToUpdate = sensor.getAdditionalInfo();

        if (CollectionUtils.isEmpty(sensorAdditionalInfoToUpdate)) {
          sensorAdditionalInfoToUpdate = new HashMap<String, String>();
        }

        sensorAdditionalInfoToUpdate.putAll(catalogSensor.getAdditionalInfo());
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

    if (!CatalogUtils.stringIsNotEmptyOrNull(catalogSensor.getComponent())) {
      catalogSensor.setComponent(catalogSensor.getSensor());
    }

    if (!CatalogUtils.stringIsNotEmptyOrNull(catalogSensor.getComponentType())) {
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

      if (CatalogUtils.stringIsNotEmptyOrNull(catalogSensor.getComponentDesc())) {
        component.setDescription(catalogSensor.getComponentDesc());
      }

      if (catalogSensor.getComponentPublicAccess() != null) {
        component.setPublicAccess(catalogSensor.getComponentPublicAccess());
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
      if (CatalogUtils.stringIsNotEmptyOrNull(catalogComponent.getComponentDesc())) {
        component.setDescription(catalogComponent.getComponentDesc());
      }

      if (CatalogUtils.stringIsNotEmptyOrNull(catalogComponent.getComponentType())) {
        component.setComponentType(catalogComponent.getComponentType());
      }

      if (CatalogUtils.stringIsNotEmptyOrNull(catalogComponent.getLocation())) {
        component.setLocation(CatalogUtils.convertStringLocation(catalogComponent.getLocation()));
        component.setMobile(Constants.STATIC);
      }

      if (catalogComponent.getComponentPublicAccess() != null) {
        component.setPublicAccess(catalogComponent.getComponentPublicAccess());
      }

      component.setUpdateAt(new Date());
    }

    return component;
  }

  private ApiConverter() {
    // this prevents even the native class from calling this ctor as well :
    throw new AssertionError();
  }
}
