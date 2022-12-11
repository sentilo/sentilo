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
package org.sentilo.agent.alert.utils;

import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.text.ParseException;
import java.util.Locale;

import org.sentilo.agent.alert.domain.InternalAlert;
import org.sentilo.common.enums.SubscribeType;
import org.sentilo.common.utils.MessagingUtils;

public abstract class AlertUtils extends MessagingUtils {

  private static DecimalFormat decimalFormat;

  private AlertUtils() {
    throw new AssertionError();
  }

  /**
   * Return the topic to which listener should be subscribed to get the data published by the sensor
   * associated with the alert. These topic follows the syntax
   * <code>/data/{provider}/{sensor}</code>
   *
   * @param alert
   * @return the name of the topic
   */
  public static String buildAlertDataTopic(final InternalAlert alert) {
    return buildTopic(SubscribeType.DATA, alert.getProviderId(), alert.getSensorId()).getTopic();
  }

  public static String buildTopicToPublishAlert(final InternalAlert alert) {
    return buildTopic(SubscribeType.ALARM, alert.getId()).getTopic();
  }

  /**
   * Build a String with the main alert info. This String will be stored into a Redis set as a
   * member
   *
   * @param alert
   * @return Build a String with the main alert info. This String will be stored into a Redis set as
   *         a member
   */
  public static String buildFrozenAlertMember(final InternalAlert alert) {
    final StringBuilder sb = new StringBuilder();
    sb.append(alert.getProviderId());
    sb.append(Constants.REDIS_MEMBER_TOKEN);
    sb.append(alert.getSensorId());
    sb.append(Constants.REDIS_MEMBER_TOKEN);
    sb.append(alert.getId());
    return sb.toString();
  }

  public static BigDecimal transformNumber(final String value) throws ParseException {
    return (BigDecimal) getDecimalFormat().parse(value);
  }

  private static DecimalFormat getDecimalFormat() {
    if (decimalFormat == null) {
      decimalFormat = (DecimalFormat) NumberFormat.getInstance(Locale.ENGLISH);
      decimalFormat.setParseBigDecimal(true);
    }
    return decimalFormat;
  }

}
