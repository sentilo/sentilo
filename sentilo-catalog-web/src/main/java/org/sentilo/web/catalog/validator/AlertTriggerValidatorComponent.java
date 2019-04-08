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

import org.sentilo.common.enums.AlertTriggerType;
import org.sentilo.web.catalog.utils.Constants;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.Errors;

@Component
public class AlertTriggerValidatorComponent {

  public void validateTriggerType(final AlertTriggerType alertTrigger, final String expression, final Errors errors) {
    if (alertTrigger != null) {
      switch (alertTrigger) {
        case GT:
        case GTE:
        case LT:
        case LTE:
        case CHANGE_DELTA:
        case FROZEN:
          validateIfExpressionValueIsEmpty(expression, errors);
          validateIfExpressionValueIsNumeric(expression, errors);
          break;
        case EQ:
          validateIfExpressionValueIsEmpty(expression, errors);
          break;
        case CHANGE:
          break;
      }
    } else {
      errors.reject("alert.error.unknown.trigger", new Object[] {alertTrigger}, "");
    }
  }

  private void validateIfExpressionValueIsEmpty(final String expression, final Errors errors) {
    if (!StringUtils.hasText(expression)) {
      errors.rejectValue("expression", Constants.NOT_BLANK_ERROR);
    }
  }

  private void validateIfExpressionValueIsNumeric(final String expression, final Errors errors) {
    if (StringUtils.hasText(expression) && !expression.matches("[-+]?\\d+(\\.\\d+)?")) {
      errors.rejectValue("expression", "NotNumber");
    }
  }
}
