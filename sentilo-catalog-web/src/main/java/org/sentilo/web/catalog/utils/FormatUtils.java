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
package org.sentilo.web.catalog.utils;

import org.sentilo.web.catalog.domain.Alert;
import org.sentilo.web.catalog.domain.Alert.Type;
import org.sentilo.web.catalog.domain.AlertRule;
import org.sentilo.web.catalog.domain.Sensor;
import org.sentilo.web.catalog.service.SensorSubstateService;
import org.springframework.util.StringUtils;

public abstract class FormatUtils {

  private FormatUtils() {
    // this prevents even the native class from calling this ctor as well :
    throw new AssertionError();
  }

  public static String label(final String message) {
    return String.format("<span class=\"label label-info\">%s</span>", message);
  }

  public static String formatAlertTriggerColumn(final Alert alert) {
    String value = "";
    if (Type.INTERNAL.equals(alert.getType())) {
      value = alert.getTrigger().name() + "(" + alert.getExpression() + ")";
    }
    return StringUtils.hasText(value) ? FormatUtils.label(value) : value;
  }

  public static String formatAlertRuleTriggerColumn(final AlertRule alertRule) {
    final String value = alertRule.getTrigger().name() + "(" + alertRule.getExpression() + ")";
    return StringUtils.hasText(value) ? FormatUtils.label(value) : value;
  }

  public static String substateStyleColumn(final Sensor sensor, final SensorSubstateService sensorSubStateService) {
    final String description = sensorSubStateService.find(sensor.getSubstate()).getDescription();
    return String.format("<span class=\"label label-info\" title=\"%s\">%s (%s)</span>", description, sensor.getSubstate(), description);
  }
}
