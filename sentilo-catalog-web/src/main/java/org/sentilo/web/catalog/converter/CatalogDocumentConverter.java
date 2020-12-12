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

import java.beans.PropertyDescriptor;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.sentilo.common.domain.CatalogComponent;
import org.sentilo.common.domain.CatalogSensor;
import org.sentilo.common.domain.MutableCatalogElement;
import org.sentilo.common.utils.SentiloUtils;
import org.sentilo.web.catalog.domain.CatalogDocument;
import org.sentilo.web.catalog.domain.Component;
import org.sentilo.web.catalog.domain.Sensor;
import org.sentilo.web.catalog.domain.Sensor.DataType;
import org.sentilo.web.catalog.utils.CatalogUtils;
import org.sentilo.web.catalog.utils.Constants;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.BeanWrapper;
import org.springframework.beans.BeanWrapperImpl;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

public final class CatalogDocumentConverter {

  private CatalogDocumentConverter() {
    // this prevents even the native class from calling this ctor as well :
    throw new AssertionError();
  }

  public static final void copyProperties(final MutableCatalogElement catalogResource, final CatalogDocument resource) {
    if (catalogResource instanceof CatalogSensor && resource instanceof Sensor) {
      copyProperties((CatalogSensor) catalogResource, (Sensor) resource);
    } else if (catalogResource instanceof CatalogComponent && resource instanceof Component) {
      copyProperties((CatalogComponent) catalogResource, (Component) resource);
    }
  }

  public static final void copyProperties(final CatalogSensor catalogSensor, final Sensor sensor) {

    if (SentiloUtils.stringIsNotEmptyOrNull(catalogSensor.getDescription())) {
      sensor.setDescription(catalogSensor.getDescription());
    }

    if (SentiloUtils.stringIsNotEmptyOrNull(catalogSensor.getType())) {
      sensor.setType(catalogSensor.getType());
    }

    if (SentiloUtils.stringIsNotEmptyOrNull(catalogSensor.getUnit())) {
      sensor.setUnit(catalogSensor.getUnit());
    }

    if (SentiloUtils.stringIsNotEmptyOrNull(catalogSensor.getDataType())) {
      sensor.setDataType(parseDataTypeValue(catalogSensor.getDataType()));
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

    if (catalogSensor.getTechnicalDetails() != null) {
      if (sensor.getTechnicalDetails() == null) {
        sensor.setTechnicalDetails(catalogSensor.getTechnicalDetails());
      } else {
        BeanUtils.copyProperties(catalogSensor.getTechnicalDetails(), sensor.getTechnicalDetails(),
            getNullPropertyNames(catalogSensor.getTechnicalDetails()));
      }
    }

  }

  public static final void copyProperties(final CatalogComponent catalogComponent, final Component component) {

    if (SentiloUtils.stringIsNotEmptyOrNull(catalogComponent.getComponent())) {
      component.setName(catalogComponent.getComponent());
    }

    if (SentiloUtils.stringIsNotEmptyOrNull(catalogComponent.getComponentType())) {
      component.setComponentType(catalogComponent.getComponentType());
    }

    if (StringUtils.hasText(catalogComponent.getLocation())) {
      component.setLocation(CatalogUtils.convertStringLocation(catalogComponent.getLocation()));
      component.setMobile(Constants.STATIC);
    } else {
      component.setMobile(Constants.MOBILE);
    }

    if (SentiloUtils.stringIsNotEmptyOrNull(catalogComponent.getComponentDesc())) {
      component.setDescription(catalogComponent.getComponentDesc());
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

    if (catalogComponent.getComponentTechnicalDetails() != null) {
      if (component.getTechnicalDetails() == null) {
        component.setTechnicalDetails(catalogComponent.getComponentTechnicalDetails());
      } else {
        BeanUtils.copyProperties(catalogComponent.getComponentTechnicalDetails(), component.getTechnicalDetails(),
            getNullPropertyNames(catalogComponent.getComponentTechnicalDetails()));
      }
    }
  }

  public static final CatalogComponent extractCatalogComponent(final CatalogSensor catalogSensor) {
    // This method extracts internal catalogComponent from the catalogSensor wrapper
    final CatalogComponent catalogComponent = new CatalogComponent();
    BeanUtils.copyProperties(catalogSensor, catalogComponent);

    // Set default field values it them are empty
    // 1. Component name by default is equal to sensor name
    if (!SentiloUtils.stringIsNotEmptyOrNull(catalogComponent.getComponent())) {
      catalogComponent.setComponent(catalogSensor.getSensor());
    }

    // 2. Component type by default is equal to generic
    if (!SentiloUtils.stringIsNotEmptyOrNull(catalogComponent.getComponentType())) {
      catalogComponent.setComponentType(Constants.DEFAULT_COMPONENT_TYPE);
    }

    return catalogComponent;
  }

  public static final void copyProperties(final Sensor sensor, final CatalogSensor catalogSensor) {
    catalogSensor.setSensor(sensor.getSensorId());
    catalogSensor.setType(sensor.getType());
    catalogSensor.setDataType(sensor.getDataType().name());
    catalogSensor.setUnit(sensor.getUnit());
    catalogSensor.setTimeZone(sensor.getTimeZone());
    catalogSensor.setPublicAccess(sensor.getPublicAccess());
    catalogSensor.setState(sensor.getState());
    catalogSensor.setCreatedAt(sensor.getCreatedAt().getTime());
    catalogSensor.setUpdatedAt(sensor.getUpdatedAt().getTime());

    if (StringUtils.hasText(sensor.getDescription())) {
      catalogSensor.setDescription(sensor.getDescription());
    }

    if (!CollectionUtils.isEmpty(sensor.getAdditionalInfo())) {
      catalogSensor.setAdditionalInfo(sensor.getAdditionalInfo());
    }

    if (sensor.getTechnicalDetails() != null) {
      catalogSensor.setTechnicalDetails(sensor.getTechnicalDetails());
    }
  }

  public static final void copyProperties(final Component component, final CatalogComponent catalogComponent) {
    catalogComponent.setComponent(component.getName());
    catalogComponent.setComponentCreatedAt(component.getCreatedAt().getTime());
    catalogComponent.setComponentUpdatedAt(component.getUpdatedAt().getTime());

    if (component.getLocation() != null) {
      catalogComponent.setLocation(CatalogUtils.locationToString(component.getLocation()));
    }
    if (StringUtils.hasText(component.getDescription())) {
      catalogComponent.setComponentDesc(component.getDescription());
    }
    if (StringUtils.hasText(component.getComponentType())) {
      catalogComponent.setComponentType(component.getComponentType());
    }
    if (component.getPublicAccess() != null) {
      catalogComponent.setComponentPublicAccess(component.getPublicAccess());
    }

    if (!CollectionUtils.isEmpty(component.getAdditionalInfo())) {
      catalogComponent.setComponentAdditionalInfo(component.getAdditionalInfo());
    }

    if (component.getTechnicalDetails() != null) {
      catalogComponent.setComponentTechnicalDetails(component.getTechnicalDetails());
    }
  }

  public static final void copyProperties(final CatalogComponent catalogComponent, final CatalogSensor catalogSensor) {
    // CatalogComponent implements method getUpdatedAt that returns its internal attribute
    // componentUpdatedAt.
    // If it isn't ignored then copyProperties override updatedAt sensorAttribute with the value
    // returned by getUpdatedAt of CatalogComponent
    BeanUtils.copyProperties(catalogComponent, catalogSensor, "updatedAt");
  }

  public static final DataType parseDataTypeValue(final String dataTypeValue) {
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

  private static final String[] getNullPropertyNames(final Object source) {
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

}
