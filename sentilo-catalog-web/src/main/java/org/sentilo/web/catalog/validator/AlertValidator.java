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

import org.sentilo.web.catalog.domain.Alert;
import org.sentilo.web.catalog.domain.Sensor;
import org.sentilo.web.catalog.service.SensorService;
import org.sentilo.web.catalog.utils.Constants;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.StringUtils;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

@SentiloValidator
public class AlertValidator implements Validator {

  @Autowired
  private SensorService sensorService;

  @Autowired
  private AlertTriggerValidatorComponent alertTriggerValidatorComponent;

  @Override
  public boolean supports(final Class<?> clazz) {
    return Alert.class.equals(clazz);
  }

  @Override
  public void validate(final Object target, final Errors errors) {
    final Alert alert = (Alert) target;

    if (alert.getType() != null) {
      switch (alert.getType()) {
        case INTERNAL:
          validateInternalAlert(alert, errors);
          break;
        case EXTERNAL:
          validateExternalAlert(alert, errors);
          break;
      }
    } else {
      errors.reject("alert.error.unknown.type", new Object[] {alert.getType()}, "");
    }
  }

  private void validateExternalAlert(final Alert alert, final Errors errors) {
    if (!StringUtils.hasText(alert.getApplicationId()) && !StringUtils.hasText(alert.getProviderId())) {
      errors.reject("alert.error.external.null.entity");
    } else if (StringUtils.hasText(alert.getApplicationId()) && StringUtils.hasText(alert.getProviderId())) {
      errors.reject("alert.error.external.duplicate.entity");
    }
  }

  private void validateInternalAlert(final Alert alert, final Errors errors) {
    if (!StringUtils.hasText(alert.getSensorId())) {
      errors.rejectValue("sensorId", Constants.NOT_BLANK_ERROR);
    }

    if (!StringUtils.hasText(alert.getComponentId())) {
      errors.rejectValue("componentId", Constants.NOT_BLANK_ERROR);
    }

    if (!StringUtils.hasText(alert.getProviderId())) {
      errors.rejectValue("providerId", Constants.NOT_BLANK_ERROR);
    }

    if (StringUtils.hasText(alert.getProviderId()) && StringUtils.hasLength(alert.getSensorId())) {
      final Sensor sensor = sensorService.find(new Sensor(alert.getProviderId(), alert.getComponentId(), alert.getSensorId()));
      if (sensor == null) {
        errors.rejectValue("sensorId", "alert.error.sensor.notfound");
      }
    }

    alertTriggerValidatorComponent.validateTriggerType(alert.getTrigger(), alert.getExpression(), errors);
  }

}
