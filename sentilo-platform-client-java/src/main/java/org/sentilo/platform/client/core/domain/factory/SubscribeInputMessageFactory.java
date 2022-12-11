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
package org.sentilo.platform.client.core.domain.factory;

import org.sentilo.common.enums.SubscribeType;
import org.sentilo.platform.client.core.domain.SubscribeInputMessage;
import org.sentilo.platform.client.core.domain.SubscriptionParams;
import org.springframework.util.StringUtils;

public abstract class SubscribeInputMessageFactory {

  private SubscribeInputMessageFactory() {
    // this prevents even the native class from calling this ctor as well :
    throw new AssertionError();
  }

  public static SubscribeInputMessage buildSubscription(final SubscribeType type) throws IllegalArgumentException {
    return buildSubscription(type, (String) null);
  }

  public static SubscribeInputMessage buildSubscription(final SubscribeType type, final SubscriptionParams subscriptionParams)
      throws IllegalArgumentException {
    return buildSubscription(type, subscriptionParams, (String) null);
  }

  public static SubscribeInputMessage buildSubscription(final SubscribeType type, final String... resources) throws IllegalArgumentException {
    return buildSubscription(type, null, resources);
  }

  public static SubscribeInputMessage buildSubscription(final SubscribeType type, final SubscriptionParams subscriptionParams,
      final String... resources) throws IllegalArgumentException {
    if (type == null) {
      throw new IllegalArgumentException("Param type is mandatory");
    }

    SubscribeInputMessage message = null;

    switch (type) {
      case ALARM:
        message = new AlarmSubscription(subscriptionParams, resources);
        break;
      case DATA:
        message = new DataSubscription(subscriptionParams, resources);
        break;
      case ORDER:
        message = new OrderSubscription(subscriptionParams, resources);
        break;
    }

    return message;
  }

  static String getArrayValue(final String[] values, final int pos) {
    if (values != null && pos < values.length) {
      return values[pos];
    }
    return null;
  }

  static class AlarmSubscription extends SubscribeInputMessage {

    private final String alarmId;

    public AlarmSubscription(final SubscriptionParams subscriptionParams, final String... resources) {
      super(SubscribeType.ALARM);
      setSubscriptionParams(subscriptionParams);
      alarmId = getArrayValue(resources, 0);
      if (StringUtils.hasText(alarmId)) {
        addResource(ALARM_ID_KEY, alarmId);
      }
    }

    public String getAlarmId() {
      return alarmId;
    }
  }

  static class OrderSubscription extends SubscribeInputMessage {

    private final String providerId;
    private final String sensorId;

    public OrderSubscription(final SubscriptionParams subscriptionParams, final String... resources) {
      super(SubscribeType.ORDER);
      setSubscriptionParams(subscriptionParams);
      providerId = getArrayValue(resources, 0);
      sensorId = getArrayValue(resources, 1);

      if (StringUtils.hasText(providerId)) {
        addResource(PROVIDER_ID_KEY, providerId);
      }

      if (StringUtils.hasText(sensorId)) {
        if (providerId == null) {
          throw new IllegalArgumentException("Provider value is mandatory");
        }
        addResource(SENSOR_ID_KEY, sensorId);
      }
    }

    public String getProviderId() {
      return providerId;
    }

    public String getSensorId() {
      return sensorId;
    }
  }

  static class DataSubscription extends SubscribeInputMessage {

    private final String providerId;
    private final String sensorId;

    public DataSubscription(final SubscriptionParams subscriptionParams, final String... resources) {
      super(SubscribeType.DATA);
      setSubscriptionParams(subscriptionParams);
      providerId = getArrayValue(resources, 0);
      sensorId = getArrayValue(resources, 1);

      if (StringUtils.hasText(providerId)) {
        addResource(PROVIDER_ID_KEY, providerId);
      }

      if (StringUtils.hasText(sensorId)) {
        if (providerId == null) {
          throw new IllegalArgumentException("Provider value is mandatory");
        }
        addResource(SENSOR_ID_KEY, sensorId);
      }
    }

    public String getProviderId() {
      return providerId;
    }

    public String getSensorId() {
      return sensorId;
    }
  }
}
