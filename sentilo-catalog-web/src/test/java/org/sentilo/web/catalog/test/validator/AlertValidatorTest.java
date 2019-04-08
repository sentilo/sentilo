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

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.enums.AlertTriggerType;
import org.sentilo.common.test.AbstractBaseTest;
import org.sentilo.web.catalog.domain.Alert;
import org.sentilo.web.catalog.domain.Sensor;
import org.sentilo.web.catalog.service.SensorService;
import org.sentilo.web.catalog.validator.AlertTriggerValidatorComponent;
import org.sentilo.web.catalog.validator.AlertValidator;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.validation.Errors;

public class AlertValidatorTest extends AbstractBaseTest {

  private static final String MOCK_TEST = "mockTest";

  @Mock
  private SensorService sensorService;

  @Mock
  private Alert alert;

  @Mock
  private Errors errors;

  @InjectMocks
  private AlertValidator alertValidator;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    final AlertTriggerValidatorComponent alertTriggerValidatorComponent = new AlertTriggerValidatorComponent();
    ReflectionTestUtils.setField(alertValidator, "alertTriggerValidatorComponent", alertTriggerValidatorComponent);
  }

  @Test
  public void supports() {
    Assert.assertTrue(alertValidator.supports(Alert.class));
    Assert.assertFalse(alertValidator.supports(Sensor.class));
  }

  @Test
  public void validateValidInternalAlert() {
    final Sensor mockSensor = new Sensor(MOCK_TEST, MOCK_TEST, MOCK_TEST);
    when(alert.getType()).thenReturn(Alert.Type.INTERNAL);
    when(alert.getSensorId()).thenReturn(MOCK_TEST);
    when(alert.getComponentId()).thenReturn(MOCK_TEST);
    when(alert.getProviderId()).thenReturn(MOCK_TEST);
    when(alert.getTrigger()).thenReturn(AlertTriggerType.EQ);
    when(alert.getExpression()).thenReturn(MOCK_TEST);
    when(sensorService.find(mockSensor)).thenReturn(mockSensor);

    alertValidator.validate(alert, errors);

    verify(errors, times(0)).rejectValue(anyString(), anyString());
    verify(sensorService).find(mockSensor);
  }

  @Test
  public void validateInternalAlertWithErrors() {
    when(alert.getType()).thenReturn(Alert.Type.INTERNAL);
    when(alert.getSensorId()).thenReturn(MOCK_TEST);
    when(alert.getComponentId()).thenReturn(MOCK_TEST);
    when(alert.getTrigger()).thenReturn(AlertTriggerType.EQ);

    alertValidator.validate(alert, errors);

    verify(errors, times(2)).rejectValue(anyString(), anyString());
    verify(sensorService, times(0)).find(any(Sensor.class));
  }

  @Test
  public void validateInternalAlertWithUnknownSensor() {
    final Sensor mockSensor = new Sensor(MOCK_TEST, MOCK_TEST, MOCK_TEST);
    when(alert.getType()).thenReturn(Alert.Type.INTERNAL);
    when(alert.getSensorId()).thenReturn(MOCK_TEST);
    when(alert.getComponentId()).thenReturn(MOCK_TEST);
    when(alert.getProviderId()).thenReturn(MOCK_TEST);
    when(alert.getTrigger()).thenReturn(AlertTriggerType.EQ);
    when(alert.getExpression()).thenReturn(MOCK_TEST);
    when(sensorService.find(mockSensor)).thenReturn(null);

    alertValidator.validate(alert, errors);

    verify(errors, times(1)).rejectValue(anyString(), anyString());
    verify(sensorService).find(mockSensor);
  }

  @Test
  public void validateValidExternalAlert() {
    when(alert.getType()).thenReturn(Alert.Type.EXTERNAL);
    when(alert.getProviderId()).thenReturn(MOCK_TEST);

    alertValidator.validate(alert, errors);

    verify(errors, times(0)).rejectValue(anyString(), anyString());
  }

  @Test
  public void validateExternalAlertWithoutEntity() {
    when(alert.getType()).thenReturn(Alert.Type.EXTERNAL);
    alertValidator.validate(alert, errors);

    verify(errors).reject(anyString());
  }

  @Test
  public void validateExternalAlertWithTwoEntities() {
    when(alert.getType()).thenReturn(Alert.Type.EXTERNAL);
    when(alert.getProviderId()).thenReturn(MOCK_TEST);
    when(alert.getApplicationId()).thenReturn(MOCK_TEST);
    alertValidator.validate(alert, errors);

    verify(errors).reject(anyString());
  }
}
