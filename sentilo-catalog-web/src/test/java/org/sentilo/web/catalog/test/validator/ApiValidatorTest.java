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
package org.sentilo.web.catalog.test.validator;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Set;

import javax.validation.ConstraintViolation;

import org.hibernate.validator.HibernateValidator;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.domain.CatalogComponent;
import org.sentilo.common.domain.CatalogInputMessage;
import org.sentilo.common.domain.CatalogSensor;
import org.sentilo.common.domain.TechnicalDetails;
import org.sentilo.common.test.AbstractBaseTest;
import org.sentilo.web.catalog.converter.ApiConverterContext;
import org.sentilo.web.catalog.domain.Component;
import org.sentilo.web.catalog.domain.ComponentType;
import org.sentilo.web.catalog.domain.LngLat;
import org.sentilo.web.catalog.domain.Location;
import org.sentilo.web.catalog.domain.Sensor;
import org.sentilo.web.catalog.domain.SensorType;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.search.SearchFilterResult;
import org.sentilo.web.catalog.service.ComponentTypesService;
import org.sentilo.web.catalog.service.SensorService;
import org.sentilo.web.catalog.service.SensorTypesService;
import org.sentilo.web.catalog.utils.Constants;
import org.sentilo.web.catalog.validator.ApiValidationResults;
import org.sentilo.web.catalog.validator.ApiValidator;
import org.sentilo.web.catalog.validator.LngLatValidator;
import org.springframework.context.MessageSource;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.validation.Validator;
import org.springframework.validation.beanvalidation.LocalValidatorFactoryBean;

public class ApiValidatorTest extends AbstractBaseTest {

  private final String providerId = "provider1";
  private final String componentName = "component1";
  private final String sensor1Id = "sensor1";
  private final String sensor2Id = "sensor2";
  private final String invalidSensorId = "sensor.invalid";
  private final String validSensorId = "sen2or-valid_id-Ok";
  private final String temperatureType = "temperature";

  @Mock
  private SensorTypesService sensorTypesService;

  @Mock
  private ComponentTypesService componentTypesService;

  @Mock
  private SensorService sensorService;

  @Mock
  private Validator validator;

  @Mock
  private MessageSource messageSource;

  @Mock
  private TechnicalDetails technicalDetails;

  @Mock
  private CatalogComponent catalogComponent;

  @Mock
  private Component component;

  @Mock
  private Location location;

  @Mock
  private ApiConverterContext context;

  @InjectMocks
  private ApiValidator apiValidator;

  private LocalValidatorFactoryBean validatorFactory;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);

    when(sensorTypesService.findAll()).thenReturn(getSensorTypes());
    when(componentTypesService.findAll()).thenReturn(Collections.<ComponentType>emptyList());
    when(validator.supports(Sensor.class)).thenReturn(true);
    when(sensorService.search(any(SearchFilter.class))).thenReturn(new SearchFilterResult<Sensor>(Collections.<Sensor>emptyList()));
  }

  @After
  public void tearDown() throws Exception {
    if (validatorFactory != null) {
      validatorFactory.close();
    }
  }

  @Test
  public void validateKoSensors() {
    final List<Sensor> sensors = getSensorsWithErrors();
    final ApiValidationResults result = apiValidator.validateSensorsAndComponents(sensors, null, false);
    assertTrue(result.hasErrors());
  }

  @Test
  public void validateKoTechnicalDetails() {
    when(technicalDetails.getConnectivity()).thenReturn("ABC");
    when(messageSource.getMessage(Constants.CONNECTIVITY_TYPES_KEY, null, LocaleContextHolder.getLocale())).thenReturn("ET_RJ45, ET_POE,3G,WIFI");
    final List<Sensor> sensors = getSensors();
    final ApiValidationResults result = apiValidator.validateSensorsAndComponents(sensors, null, false);
    assertTrue(result.hasErrors());
  }

  @Test
  public void validateOkSensors() {
    final List<Sensor> sensors = getSensors();
    final ApiValidationResults result = apiValidator.validateSensorsAndComponents(sensors, null, false);
    assertTrue(!result.hasErrors());
  }

  @Test
  public void validateKoSensorName() {
    validatorFactory = new LocalValidatorFactoryBean();
    validatorFactory.setProviderClass(HibernateValidator.class);
    validatorFactory.afterPropertiesSet();
    final Sensor sensor = getSensor(invalidSensorId);
    final Set<ConstraintViolation<Sensor>> result = validatorFactory.validate(sensor);

    assertTrue(result.size() == 1);
  }

  @Test
  public void validateOkSensorName() {
    validatorFactory = new LocalValidatorFactoryBean();
    validatorFactory.setProviderClass(HibernateValidator.class);
    validatorFactory.afterPropertiesSet();
    final Sensor sensor = getSensor(validSensorId);
    final Set<ConstraintViolation<Sensor>> result = validatorFactory.validate(sensor);

    assertTrue(result.size() == 0);
  }

  @Test
  public void validateLocationContent() {
    final LngLat[] lngLat1 = {new LngLat(1.2345, 43.23456)};
    final LngLat[] lngLat2 = {new LngLat(1.2345, 43.23456), new LngLat(-198.2345, 99.23456)};

    ReflectionTestUtils.setField(apiValidator, "validator", new LngLatValidator());

    when(component.getLocation()).thenReturn(location);
    // Each invoke to validateLocationContent does two calls to location.getCoordinates()
    when(location.getCoordinates()).thenReturn(lngLat1, lngLat1, lngLat2, lngLat2);

    final ApiValidationResults results = new ApiValidationResults();
    final ApiValidationResults results2 = new ApiValidationResults();

    ReflectionTestUtils.invokeMethod(apiValidator, "validateLocationContent", results, component);
    ReflectionTestUtils.invokeMethod(apiValidator, "validateLocationContent", results2, component);

    assertFalse(results.hasErrors());
    assertTrue(results2.hasErrors() && results2.getErrorsCount() == 1);
  }

  @Test
  public void validateSensorsDataType() {
    final String[] dataTypes = {"NUMBER", "nuMBeR", "BOOLEAN", "text", "wrongDataType"};
    final List<CatalogSensor> sensors = new ArrayList<CatalogSensor>();
    for (int i = 0; i < dataTypes.length; i++) {
      final CatalogSensor sensor = new CatalogSensor();
      sensor.setDataType(dataTypes[i]);
      sensor.setSensor("sensor-" + i);
      sensors.add(sensor);
    }
    final CatalogInputMessage cim = new CatalogInputMessage();
    cim.setSensors(sensors);

    when(context.isUpdateAction()).thenReturn(false);
    when(context.getMessage()).thenReturn(cim);

    final ApiValidationResults results = apiValidator.validateFieldFormatValues(context);

    assertTrue(results.hasErrors() && results.getErrorsCount() == 1);
    assertTrue(results.getErrors().get(0).contains("wrongDataType"));

  }

  private List<Sensor> getSensorsWithErrors() {
    final List<Sensor> sensors = new ArrayList<Sensor>();

    final String componentId = Component.buildId(providerId, componentName);

    final Sensor sensor = new Sensor(providerId, componentId, sensor1Id);
    sensor.setDescription("Lorem ipsum dolor sit amet");
    sensor.setUnit("C");
    sensor.setDataType(Sensor.DataType.BOOLEAN);
    sensor.setCreatedAt(new Date());
    sensor.setTechnicalDetails(technicalDetails);
    sensors.add(sensor);

    final Sensor sensor2 = new Sensor(providerId, componentId, sensor2Id);
    sensor2.setDescription("Sed ut perspiciatis unde omnis");
    sensor2.setType(temperatureType);
    sensor2.setDataType(Sensor.DataType.BOOLEAN);
    sensor2.setCreatedAt(new Date());
    sensor2.setTechnicalDetails(technicalDetails);
    sensors.add(sensor2);

    return sensors;
  }

  private Sensor getSensor(final String sensorId) {

    final String componentId = Component.buildId(providerId, componentName);

    final Sensor sensor = new Sensor(providerId, componentId, sensorId);
    sensor.setDescription("Lorem ipsum dolor sit amet");
    sensor.setUnit("C");
    sensor.setType(temperatureType);
    sensor.setDataType(Sensor.DataType.BOOLEAN);
    sensor.setCreatedAt(new Date());
    sensor.setTenantId("tenantId");

    return sensor;
  }

  private List<Sensor> getSensors() {
    final List<Sensor> sensors = new ArrayList<Sensor>();

    final String componentId = Component.buildId(providerId, componentName);

    final Sensor sensor = new Sensor(providerId, componentId, sensor1Id);
    sensor.setDescription("Lorem ipsum dolor sit amet");
    sensor.setUnit("C");
    sensor.setType(temperatureType);
    sensor.setDataType(Sensor.DataType.BOOLEAN);
    sensor.setCreatedAt(new Date());
    sensor.setTechnicalDetails(technicalDetails);
    sensors.add(sensor);

    return sensors;
  }

  private List<SensorType> getSensorTypes() {
    final SensorType[] sensorTypes = {new SensorType(temperatureType)};
    return Arrays.asList(sensorTypes);
  }

}
