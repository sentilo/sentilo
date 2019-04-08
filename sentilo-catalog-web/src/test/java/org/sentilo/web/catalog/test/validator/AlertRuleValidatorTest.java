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

import static org.mockito.Mockito.when;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.enums.AlertTriggerType;
import org.sentilo.common.test.AbstractBaseTest;
import org.sentilo.web.catalog.domain.AlertRule;
import org.sentilo.web.catalog.utils.Constants;
import org.sentilo.web.catalog.validator.AlertRuleValidator;
import org.sentilo.web.catalog.validator.AlertTriggerValidatorComponent;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.validation.BeanPropertyBindingResult;
import org.springframework.validation.Errors;

public class AlertRuleValidatorTest extends AbstractBaseTest {

  @Mock
  private AlertRule alertRule;

  @InjectMocks
  private AlertRuleValidator alertRuleValidator;

  private Errors errors;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    final AlertTriggerValidatorComponent alertTriggerValidatorComponent = new AlertTriggerValidatorComponent();
    ReflectionTestUtils.setField(alertRuleValidator, "alertTriggerValidatorComponent", alertTriggerValidatorComponent);
    errors = new BeanPropertyBindingResult(alertRule, "alertRule");
  }

  @Test
  public void supports() {
    Assert.assertTrue(alertRuleValidator.supports(AlertRule.class));
  }

  @Test
  public void validateNullProviderIdTest() {
    when(alertRule.getProviderId()).thenReturn(null);

    final Errors errors = new BeanPropertyBindingResult(alertRule, "alertRule");

    alertRuleValidator.validate(alertRule, errors);

    Assert.assertTrue(errors.hasErrors());
    Assert.assertFalse(errors.getFieldErrors("providerId").isEmpty());
    Assert.assertEquals(errors.getFieldError().getField(), "providerId");
    Assert.assertEquals(errors.getFieldError().getCode(), Constants.NOT_BLANK_ERROR);
  }

  @Test
  public void validateNotBlankExpressionTest() {
    when(alertRule.getProviderId()).thenReturn("providerId");
    when(alertRule.getExpression()).thenReturn(null);

    // GT, GTE, LT, LTE, EQ, CHANGE_DELTA, FROZEN

    // GT Trigger
    when(alertRule.getTrigger()).thenReturn(AlertTriggerType.GT);
    alertRuleValidator.validate(alertRule, errors);
    validateExpressionNotBlank();

    // GTE Trigger
    when(alertRule.getTrigger()).thenReturn(AlertTriggerType.GTE);
    alertRuleValidator.validate(alertRule, errors);
    validateExpressionNotBlank();

    // LT Trigger
    when(alertRule.getTrigger()).thenReturn(AlertTriggerType.LT);
    alertRuleValidator.validate(alertRule, errors);
    validateExpressionNotBlank();

    // LTE Trigger
    when(alertRule.getTrigger()).thenReturn(AlertTriggerType.LTE);
    alertRuleValidator.validate(alertRule, errors);
    validateExpressionNotBlank();

    // EQ Trigger
    when(alertRule.getTrigger()).thenReturn(AlertTriggerType.EQ);
    alertRuleValidator.validate(alertRule, errors);
    validateExpressionNotBlank();

    // CHANGE_DELTA Trigger
    when(alertRule.getTrigger()).thenReturn(AlertTriggerType.CHANGE_DELTA);
    alertRuleValidator.validate(alertRule, errors);
    validateExpressionNotBlank();

    // FROZEN Trigger
    when(alertRule.getTrigger()).thenReturn(AlertTriggerType.FROZEN);
    alertRuleValidator.validate(alertRule, errors);
    validateExpressionNotBlank();
  }

  @Test
  public void validateNotNumberExpressionTest() {
    when(alertRule.getProviderId()).thenReturn("providerId");
    when(alertRule.getExpression()).thenReturn("abcde");

    // GT, GTE, LT, LTE, CHANGE_DELTA, FROZEN

    // GT Trigger
    when(alertRule.getTrigger()).thenReturn(AlertTriggerType.GT);
    alertRuleValidator.validate(alertRule, errors);
    validateExpressionNotNumber();

    // GTE Trigger
    when(alertRule.getTrigger()).thenReturn(AlertTriggerType.GTE);
    alertRuleValidator.validate(alertRule, errors);
    validateExpressionNotNumber();

    // LT Trigger
    when(alertRule.getTrigger()).thenReturn(AlertTriggerType.LT);
    alertRuleValidator.validate(alertRule, errors);
    validateExpressionNotNumber();

    // LTE Trigger
    when(alertRule.getTrigger()).thenReturn(AlertTriggerType.LTE);
    alertRuleValidator.validate(alertRule, errors);
    validateExpressionNotNumber();

    // CHANGE_DELTA Trigger
    when(alertRule.getTrigger()).thenReturn(AlertTriggerType.CHANGE_DELTA);
    alertRuleValidator.validate(alertRule, errors);
    validateExpressionNotNumber();

    // FROZEN Trigger
    when(alertRule.getTrigger()).thenReturn(AlertTriggerType.FROZEN);
    alertRuleValidator.validate(alertRule, errors);
    validateExpressionNotNumber();
  }

  @Test
  public void validateBlankExpressionWhenChangeTriggerTest() {
    when(alertRule.getProviderId()).thenReturn("providerId");
    when(alertRule.getExpression()).thenReturn(null);

    // CHANGE Trigger
    when(alertRule.getTrigger()).thenReturn(AlertTriggerType.CHANGE);
    alertRuleValidator.validate(alertRule, errors);

    Assert.assertFalse(errors.hasErrors());
  }

  private void validateExpressionNotNumber() {
    Assert.assertTrue(errors.hasErrors());
    Assert.assertTrue(errors.getFieldErrors("providerId").isEmpty());
    Assert.assertFalse(errors.getFieldErrors("expression").isEmpty());
    Assert.assertEquals(errors.getFieldError().getField(), "expression");
    Assert.assertEquals(errors.getFieldError().getCode(), "NotNumber");
  }

  private void validateExpressionNotBlank() {
    Assert.assertTrue(errors.hasErrors());
    Assert.assertTrue(errors.getFieldErrors("providerId").isEmpty());
    Assert.assertFalse(errors.getFieldErrors("expression").isEmpty());
    Assert.assertEquals(errors.getFieldError().getField(), "expression");
    Assert.assertEquals(errors.getFieldError().getCode(), Constants.NOT_BLANK_ERROR);
  }
}
