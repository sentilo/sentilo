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
package org.sentilo.agent.alert.trigger;

import java.math.BigDecimal;
import java.text.ParseException;

import org.sentilo.agent.alert.domain.InternalAlert;
import org.sentilo.agent.alert.utils.AlertUtils;
import org.sentilo.agent.alert.utils.Constants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.StringUtils;

public class TriggerEvaluator {

  private final Logger logger = LoggerFactory.getLogger(TriggerEvaluator.class);

  private String lastAcceptedValue;

  public TriggerResult evaluate(final InternalAlert alert, final String value) {
    // GT, GTE, LT, LTE, EQ, CHANGE, CHANGE_DELTA
    logger.debug("Evaluating alarm {} for sensor value {}", alert.getId(), value);

    TriggerResult result = null;
    try {
      switch (alert.getTrigger()) {
        case GT:
          result = evaluateGreaterThanTrigger(alert, value);
          break;
        case GTE:
          result = evaluateGreaterThanOrEqualsTrigger(alert, value);
          break;
        case LT:
          result = evaluateLessThanTrigger(alert, value);
          break;
        case LTE:
          result = evaluateLessThanOrEqualsTrigger(alert, value);
          break;
        case EQ:
          result = evaluateEqualsTrigger(alert, value);
          break;
        case CHANGE:
          result = evaluateChangeTrigger(alert, value);
          break;
        case CHANGE_DELTA:
          result = evaluateChangeDeltaTrigger(alert, value);
          break;
        default:
          result = new TriggerResult();
          break;
      }
    } catch (final ParseException pe) {
      // Esta excepcion no deberia ocurrir nunca ya que al dar de alta la alarma en el catalogo se
      // valida
      // el formato numerico de la expresion, y antes de aplicar cualquier evaluacion se valida que
      // el valor
      // del sensor sea numerico. Pero es mejor controlarla que retornar una excepcion o ignorarla.
      result = new TriggerResult(String.format(Constants.TEMPLATE_NO_NUMBER_MESSAGE, alert.getId(), value, alert.getSensorId()));
    }

    logger.debug("Evaluation result was {}", result.triggerConditionChecked());

    return result;
  }

  public void setLastAcceptedValue(final String lastAcceptedValue) {
    this.lastAcceptedValue = lastAcceptedValue;
  }

  private boolean isNumberValue(final String value) {
    return value.matches("[-+]?\\d+(\\.\\d+)?");
  }

  private int compareNumbers(final String sensorValue, final String valueToCompare) throws ParseException {
    final BigDecimal bdValue = AlertUtils.transformNumber(sensorValue);
    final BigDecimal limit = AlertUtils.transformNumber(valueToCompare);
    return bdValue.compareTo(limit);
  }

  private String buildErrorMessage(final String templateErrorMessage, final InternalAlert alert, final String value) {
    final String expressionMessage = String.format(templateErrorMessage, alert.getExpression());
    return String.format(Constants.TEMPLATE_MESSAGE, alert.getId(), value, alert.getSensorId(), expressionMessage);
  }

  private TriggerResult evaluateGreaterThanTrigger(final InternalAlert alert, final String value) throws ParseException {
    TriggerResult result = null;

    if (!isNumberValue(value)) {
      result = new TriggerResult(String.format(Constants.TEMPLATE_NO_NUMBER_MESSAGE, alert.getId(), value, alert.getSensorId()));
    } else {
      final String errorMessage = buildErrorMessage(Constants.TEMPLATE_GT_MESSAGE, alert, value);
      result = (compareNumbers(value, alert.getExpression()) == 1) ? new TriggerResult(errorMessage) : new TriggerResult();
    }

    return result;
  }

  private TriggerResult evaluateGreaterThanOrEqualsTrigger(final InternalAlert alert, final String value) throws ParseException {
    TriggerResult result = null;

    if (!isNumberValue(value)) {
      result = new TriggerResult(String.format(Constants.TEMPLATE_NO_NUMBER_MESSAGE, alert.getId(), value, alert.getSensorId()));
    } else {
      final String errorMessage = buildErrorMessage(Constants.TEMPLATE_GTE_MESSAGE, alert, value);
      result = (compareNumbers(value, alert.getExpression()) != -1) ? new TriggerResult(errorMessage) : new TriggerResult();
    }

    return result;
  }

  private TriggerResult evaluateLessThanTrigger(final InternalAlert alert, final String value) throws ParseException {
    TriggerResult result = null;

    if (!isNumberValue(value)) {
      result = new TriggerResult(String.format(Constants.TEMPLATE_NO_NUMBER_MESSAGE, alert.getId(), value, alert.getSensorId()));
    } else {
      final String errorMessage = buildErrorMessage(Constants.TEMPLATE_LT_MESSAGE, alert, value);
      result = (compareNumbers(value, alert.getExpression()) == -1) ? new TriggerResult(errorMessage) : new TriggerResult();
    }

    return result;
  }

  private TriggerResult evaluateLessThanOrEqualsTrigger(final InternalAlert alert, final String value) throws ParseException {
    TriggerResult result = null;

    if (!isNumberValue(value)) {
      result = new TriggerResult(String.format(Constants.TEMPLATE_NO_NUMBER_MESSAGE, alert.getId(), value, alert.getSensorId()));
    } else {
      final String errorMessage = buildErrorMessage(Constants.TEMPLATE_LTE_MESSAGE, alert, value);
      result = (compareNumbers(value, alert.getExpression()) != 1) ? new TriggerResult(errorMessage) : new TriggerResult();
    }

    return result;
  }

  private TriggerResult evaluateEqualsTrigger(final InternalAlert alert, final String value) {
    TriggerResult result = null;
    final String errorMessage = buildErrorMessage(Constants.TEMPLATE_EQ_MESSAGE, alert, value);
    result = (value.equals(alert.getExpression()) ? new TriggerResult(errorMessage) : new TriggerResult());

    return result;
  }

  private TriggerResult evaluateChangeTrigger(final InternalAlert alert, final String value) {
    TriggerResult result = null;
    final String errorMessage = buildErrorMessage(Constants.TEMPLATE_CHANGE_MESSAGE, alert, value);
    if (StringUtils.hasText(lastAcceptedValue)) {
      result = (value.equals(lastAcceptedValue) ? new TriggerResult() : new TriggerResult(errorMessage));
    } else {
      result = new TriggerResult();
    }

    return result;
  }

  private TriggerResult evaluateChangeDeltaTrigger(final InternalAlert alert, final String value) throws ParseException {
    TriggerResult result = null;

    // La comparacion consiste en ver si la variacion entre el valor recibido (B) y el ultimo valor
    // almacenado (A)
    // es superior al % indicado
    // Variacion = (|A-B|/|A|)*100
    if (StringUtils.hasText(lastAcceptedValue)) {

      if (!isNumberValue(value)) {
        result = new TriggerResult(String.format(Constants.TEMPLATE_NO_NUMBER_MESSAGE, alert.getId(), value, alert.getSensorId()));
      } else {
        final float limit = AlertUtils.transformNumber(alert.getExpression()).floatValue();
        final float absValue = Math.abs(AlertUtils.transformNumber(value).floatValue());
        final float absLastAccepted = Math.abs(AlertUtils.transformNumber(lastAcceptedValue).floatValue());

        final float variation = ((absLastAccepted - absValue) / absLastAccepted) * 100;

        final String errorMessage = buildErrorMessage(Constants.TEMPLATE_CHANGE_DELTA_MESSAGE, alert, value);
        result = (variation > limit ? new TriggerResult(errorMessage) : new TriggerResult());
      }
    } else {
      result = new TriggerResult();
    }

    return result;
  }
}
