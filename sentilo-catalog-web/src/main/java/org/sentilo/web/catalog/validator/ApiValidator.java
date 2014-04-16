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
package org.sentilo.web.catalog.validator;

import java.util.List;

import org.sentilo.web.catalog.domain.Component;
import org.sentilo.web.catalog.domain.ComponentType;
import org.sentilo.web.catalog.domain.Sensor;
import org.sentilo.web.catalog.domain.SensorType;
import org.sentilo.web.catalog.exception.builder.CompoundDuplicateKeyExceptionBuilder;
import org.sentilo.web.catalog.service.ComponentTypesService;
import org.sentilo.web.catalog.service.SensorService;
import org.sentilo.web.catalog.service.SensorTypesService;
import org.sentilo.web.catalog.utils.ApiTranslator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.CollectionUtils;
import org.springframework.validation.Validator;

@org.springframework.stereotype.Component
public class ApiValidator extends ApiBaseValidator<Sensor> {

  @Autowired
  private SensorTypesService sensorTypesService;

  @Autowired
  private ComponentTypesService componentTypesService;

  @Autowired
  private SensorService sensorService;

  @Autowired
  private Validator validator;

  public ApiValidationResults
      validateSensorsAndComponents(final List<Sensor> sensors, final List<Component> components, final boolean isUpdateAction) {
    final ApiValidationResults results = new ApiValidationResults();
    final List<SensorType> sensorTypes = sensorTypesService.findAll();
    final List<ComponentType> componentTypes = componentTypesService.findAll();

    if (!CollectionUtils.isEmpty(sensors)) {
      for (final Sensor sensor : sensors) {
        validate(results, sensor, sensor.getSensorId(), "Sensor", ApiTranslator.SENSOR_DOMAIN_FIELDS);
        validateSensorTypes(results, sensor, sensorTypes);
      }

      if (!isUpdateAction && !results.hasErrors()) {
        validateKeys(results, sensors);
      }
    }

    if (!CollectionUtils.isEmpty(components)) {
      for (final Component component : components) {
        validate(results, component, component.getName(), "Component", ApiTranslator.COMPONENT_DOMAIN_FIELDS);
        validateComponentTypes(results, component, componentTypes);
      }
    }

    return results;
  }

  private void validateSensorTypes(final ApiValidationResults results, final Sensor sensor, final List<SensorType> sensorTypes) {
    if (!sensorTypes.contains(new SensorType(sensor.getType()))) {
      final String errorMessage = String.format("Sensor %s : an invalid value was specified for type field.", sensor.getSensorId());
      results.addErrorMessage(errorMessage);
    }
  }

  private void validateComponentTypes(final ApiValidationResults results, final Component component, final List<ComponentType> componentTypes) {
    if (!componentTypes.contains(new ComponentType(component.getComponentType()))) {
      final String errorMessage = String.format("Component %s : an invalid value was specified for componentType field.", component.getName());
      results.addErrorMessage(errorMessage);
    }
  }

  protected EntityKeyValidator buildEntityKeyValidator() {
    return new SensorEntityKeyValidatorImpl(sensorService, new CompoundDuplicateKeyExceptionBuilder("error.sensor.duplicate.key"));
  }

  protected String buildIntegrityKeyErrorMessage(final Sensor sensor) {
    return String.format("Sensor %s : sensor with the same id already exists.", sensor.getSensorId());
  }

  protected String getGroupKey(final Sensor sensor) {
    return sensor.getSensorId();
  }

  protected Validator getValidator() {
    return this.validator;
  }
}
