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
package org.sentilo.web.catalog.validator;

import static org.sentilo.common.utils.SentiloUtils.arrayContainsValue;

import java.util.Arrays;
import java.util.List;

import org.sentilo.common.domain.CatalogComponent;
import org.sentilo.common.domain.CatalogElement;
import org.sentilo.common.domain.CatalogSensor;
import org.sentilo.common.domain.TechnicalDetails;
import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.common.utils.SentiloUtils;
import org.sentilo.web.catalog.converter.ApiConverterContext;
import org.sentilo.web.catalog.domain.CatalogDocument;
import org.sentilo.web.catalog.domain.Component;
import org.sentilo.web.catalog.domain.ComponentType;
import org.sentilo.web.catalog.domain.LngLat;
import org.sentilo.web.catalog.domain.Sensor;
import org.sentilo.web.catalog.domain.Sensor.DataType;
import org.sentilo.web.catalog.domain.SensorType;
import org.sentilo.web.catalog.exception.builder.CompoundDuplicateKeyExceptionBuilder;
import org.sentilo.web.catalog.service.ComponentTypesService;
import org.sentilo.web.catalog.service.SensorService;
import org.sentilo.web.catalog.service.SensorTypesService;
import org.sentilo.web.catalog.utils.ApiTranslator;
import org.sentilo.web.catalog.utils.Constants;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.MessageSource;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;
import org.springframework.validation.DataBinder;
import org.springframework.validation.Validator;

@org.springframework.stereotype.Component
public class ApiValidator extends ApiBaseValidator<Sensor> {

  private static final String SENSOR = "Sensor";
  private static final String COMPONENT = "Component";

  @Autowired
  private SensorTypesService sensorTypesService;

  @Autowired
  private ComponentTypesService componentTypesService;

  @Autowired
  private SensorService sensorService;

  @Autowired
  private MessageSource messageSource;

  @Autowired
  private Validator validator;

  public ApiValidationResults validateFieldFormatValues(final ApiConverterContext context) {
    // For each field filled in, validates that it has the right format (number, date, coordinates,
    // ... )
    final ApiValidationResults results = new ApiValidationResults();

    validateSensorsDataType(results, context);
    validateComponentsLocations(results, context);

    return results;
  }

  public ApiValidationResults validateSensorsAndComponents(final List<Sensor> sensors, final List<Component> components,
      final boolean isUpdateAction) {
    final ApiValidationResults results = new ApiValidationResults();
    final List<SensorType> sensorTypes = sensorTypesService.findAll();
    final List<ComponentType> componentTypes = componentTypesService.findAll();
    final String connectivityTypes = messageSource.getMessage(Constants.CONNECTIVITY_TYPES_KEY, null, LocaleContextHolder.getLocale());
    final String energyTypes = messageSource.getMessage(Constants.ENERGY_TYPES_KEY, null, LocaleContextHolder.getLocale());
    final String[] connectivityTypesList =
        StringUtils.hasText(connectivityTypes) ? connectivityTypes.split(SentiloConstants.COMMA_TOKEN_SPLITTER) : new String[] {};
    final String[] energyTypesList = StringUtils.hasText(energyTypes) ? energyTypes.split(SentiloConstants.COMMA_TOKEN_SPLITTER) : new String[] {};

    if (!CollectionUtils.isEmpty(sensors)) {
      for (final Sensor sensor : sensors) {
        validate(results, sensor, sensor.getSensorId(), SENSOR, ApiTranslator.SENSOR_DOMAIN_FIELDS);
        validateSensorTypes(results, sensor, sensorTypes);
        validateTechnicalDetails(results, sensor.getTechnicalDetails(), sensor.getSensorId(), sensor, connectivityTypesList, energyTypesList);
      }

      if (!isUpdateAction && !results.hasErrors()) {
        validateKeys(results, sensors);
      }
    }

    if (!CollectionUtils.isEmpty(components)) {
      for (final Component component : components) {
        validate(results, component, component.getName(), COMPONENT, ApiTranslator.COMPONENT_DOMAIN_FIELDS);
        validateLocationContent(results, component);
        validateComponentTypes(results, component, componentTypes);
        validateTechnicalDetails(results, component.getTechnicalDetails(), component.getName(), component, connectivityTypesList, energyTypesList);
      }
    }

    return results;
  }

  protected ResourceKeyValidator buildResourceKeyValidator() {
    return new SensorKeyValidatorImpl(sensorService, new CompoundDuplicateKeyExceptionBuilder("error.sensor.duplicate.key"));
  }

  protected String buildIntegrityKeyErrorMessage(final Sensor sensor) {
    return String.format("Sensor %s : sensor with the same id already exists.", sensor.getSensorId());
  }

  protected String getGroupKey(final Sensor sensor) {
    return sensor.getSensorId();
  }

  protected Validator getValidator() {
    return validator;
  }

  private void validateSensorTypes(final ApiValidationResults results, final Sensor sensor, final List<SensorType> sensorTypes) {
    if (!sensorTypes.contains(new SensorType(sensor.getType()))) {
      final String errorMessage = buildErrorMessage(SENSOR, sensor.getSensorId(), "type", sensor.getType());
      results.addErrorMessage(errorMessage);
    }
  }

  private void validateComponentTypes(final ApiValidationResults results, final Component component, final List<ComponentType> componentTypes) {
    if (!componentTypes.contains(new ComponentType(component.getComponentType()))) {
      final String errorMessage = buildErrorMessage(COMPONENT, component.getName(), "componentType", component.getComponentType());
      results.addErrorMessage(errorMessage);
    }
  }

  private void validateTechnicalDetails(final ApiValidationResults results, final TechnicalDetails technicalDetails, final String resourceId,
      final CatalogDocument resource, final String[] connectivityTypesList, final String[] energyTypesList) {
    final String resourceType = resource instanceof Component ? COMPONENT : SENSOR;
    if (technicalDetails != null) {
      final String connectivity = technicalDetails.getConnectivity();
      final String energy = technicalDetails.getEnergy();

      if (StringUtils.hasText(energy) && !arrayContainsValue(energyTypesList, energy)) {
        final String errorMessage = buildErrorMessage(resourceType, resourceId, "energy", energy);
        results.addErrorMessage(errorMessage);
      }

      if (StringUtils.hasText(connectivity) && !arrayContainsValue(connectivityTypesList, connectivity)) {
        final String errorMessage = buildErrorMessage(resourceType, resourceId, "connectivity", connectivity);
        results.addErrorMessage(errorMessage);
      }
    }

  }

  private void validateComponentsLocations(final ApiValidationResults results, final ApiConverterContext context) {
    final boolean isUpdateAction = context.isUpdateAction();
    final List<? extends CatalogElement> resources = isUpdateAction ? context.getMessage().getComponents() : context.getMessage().getSensors();

    if (!CollectionUtils.isEmpty(resources)) {
      for (final CatalogElement resource : resources) {
        final String location = isUpdateAction ? ((CatalogComponent) resource).getLocation() : ((CatalogSensor) resource).getLocation();
        if (!SentiloUtils.isValidLocationFormat(location)) {
          final String resourceType = isUpdateAction ? COMPONENT : SENSOR;
          final String resourceName = isUpdateAction ? ((CatalogComponent) resource).getComponent() : ((CatalogSensor) resource).getSensor();
          final String errorMessage = buildErrorMessage(resourceType, resourceName, "location", location);
          results.addErrorMessage(errorMessage);
        }
      }
    }
  }

  private void validateLocationContent(final ApiValidationResults results, final Component component) {
    if (component.getLocation() != null && !SentiloUtils.arrayIsEmpty(component.getLocation().getCoordinates())) {
      final LngLat[] coordinates = component.getLocation().getCoordinates();
      boolean valid = true;
      for (int i = 0; i < coordinates.length && valid; i++) {
        final DataBinder binder = new DataBinder(coordinates[i]);
        binder.setValidator(getValidator());
        binder.validate();

        valid = binder.getBindingResult().hasErrors() ? false : true;
      }

      if (!valid) {
        final String errorMessage = buildErrorMessage(COMPONENT, component.getName(), "location", Arrays.toString(coordinates));
        results.addErrorMessage(errorMessage);
      }
    }
  }

  private void validateSensorsDataType(final ApiValidationResults results, final ApiConverterContext context) {
    final List<CatalogSensor> resources = context.getMessage().getSensors();

    if (!CollectionUtils.isEmpty(resources)) {
      for (final CatalogSensor resource : resources) {
        final String dataType = resource.getDataType();
        if (StringUtils.hasText(dataType) && !isValidDataType(dataType)) {
          final String resourceName = resource.getSensor();
          final String errorMessage = buildErrorMessage(SENSOR, resourceName, "dataType", dataType);
          results.addErrorMessage(errorMessage);
        }
      }
    }
  }

  private boolean isValidDataType(final String value) {
    boolean valid = true;

    try {
      DataType.valueOf(value.toUpperCase());
    } catch (final IllegalArgumentException iae) {
      valid = false;
    }

    return valid;
  }

  private String buildErrorMessage(final String resourceType, final String resourceName, final String fieldName, final String fieldValue) {
    final String errorTemplate = "%s %s: invalid value for field %s (%s).";
    return String.format(errorTemplate, resourceType, resourceName, fieldName, fieldValue);
  }
}
