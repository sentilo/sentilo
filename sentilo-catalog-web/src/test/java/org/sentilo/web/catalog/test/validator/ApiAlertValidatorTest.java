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
package org.sentilo.web.catalog.test.validator;

import static org.mockito.Matchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.List;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.web.catalog.domain.Alert;
import org.sentilo.web.catalog.domain.Alert.Trigger;
import org.sentilo.web.catalog.domain.Alert.Type;
import org.sentilo.web.catalog.domain.Sensor;
import org.sentilo.web.catalog.repository.AlertRepository;
import org.sentilo.web.catalog.service.SensorService;
import org.sentilo.web.catalog.test.AbstractBaseTest;
import org.sentilo.web.catalog.validator.AlertValidator;
import org.sentilo.web.catalog.validator.ApiAlertValidator;
import org.sentilo.web.catalog.validator.ApiValidationResults;

public class ApiAlertValidatorTest extends AbstractBaseTest {

  private static final String MOCK_VALUE = "mockValue";

  @Mock
  private SensorService sensorService;

  @Mock
  private AlertRepository repository;

  private AlertValidator validator;

  private ApiAlertValidator apiAlertValidator;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    apiAlertValidator = new ApiAlertValidator();
    validator = new AlertValidator();
    injectMocks();
  }

  @Test
  public void validateEmptyAlerts() throws Exception {
    final List<Alert> alerts = generateRandomList(Alert.class);
    ApiValidationResults results = new ApiValidationResults();
    apiAlertValidator.validate(alerts, results, false);

    Assert.assertTrue(results.hasErrors());
  }

  @Test
  public void validateInvalidExternalAlerts() throws Exception {
    final List<Alert> alerts = generateRandomAlerts(true, Type.EXTERNAL);
    ApiValidationResults results = new ApiValidationResults();
    apiAlertValidator.validate(alerts, results, false);

    Assert.assertTrue(results.hasErrors());
  }

  @Test
  public void validateExternalAlerts() throws Exception {
    final List<Alert> alerts = generateRandomAlerts(false, Type.EXTERNAL);
    ApiValidationResults results = new ApiValidationResults();
    apiAlertValidator.validate(alerts, results, false);

    Assert.assertTrue(!results.hasErrors());
  }

  @Test
  public void validateInvalidInternalAlerts() throws Exception {
    final List<Alert> alerts = generateRandomAlerts(true, Type.INTERNAL);
    ApiValidationResults results = new ApiValidationResults();
    apiAlertValidator.validate(alerts, results, false);

    Assert.assertTrue(results.hasErrors());
  }

  @Test
  public void validateInternalAlertsWithUnknownSensor() throws Exception {
    final List<Alert> alerts = generateRandomAlerts(true, Type.INTERNAL);
    ApiValidationResults results = new ApiValidationResults();
    apiAlertValidator.validate(alerts, results, false);

    Assert.assertTrue(results.hasErrors());
    verify(sensorService, times(alerts.size())).find(any(Sensor.class));
  }

  @Test
  public void validateInternalAlerts() throws Exception {
    when(sensorService.find(any(Sensor.class))).thenReturn(new Sensor());
    final List<Alert> alerts = generateRandomAlerts(false, Type.INTERNAL);
    ApiValidationResults results = new ApiValidationResults();
    apiAlertValidator.validate(alerts, results, false);

    Assert.assertTrue(!results.hasErrors());
  }

  private void injectMocks() throws Exception {
    injectField("repository", repository, apiAlertValidator, ApiAlertValidator.class);
    injectField("validator", validator, apiAlertValidator, ApiAlertValidator.class);
    injectField("sensorService", sensorService, validator, AlertValidator.class);
  }

  private List<Alert> generateRandomAlerts(boolean withErrors, Type type) throws Exception {
    final List<Alert> alerts = generateRandomList(Alert.class);

    for (Alert alert : alerts) {
      String randomToken = Double.toString(Math.random());
      alert.setType(type);
      alert.setId(randomToken);
      alert.setName(randomToken);

      switch (type) {
        case EXTERNAL:
          alert.setApplicationId(MOCK_VALUE);
          if (withErrors) {
            alert.setProviderId(MOCK_VALUE);
          }
          break;
        case INTERNAL:
          alert.setProviderId(MOCK_VALUE);
          alert.setComponentId(MOCK_VALUE);
          alert.setSensorId(MOCK_VALUE);
          alert.setExpression(MOCK_VALUE);
          if (!withErrors) {
            alert.setTrigger(Trigger.EQ);
          }
        default:
          break;
      }
    }

    return alerts;
  }
}
