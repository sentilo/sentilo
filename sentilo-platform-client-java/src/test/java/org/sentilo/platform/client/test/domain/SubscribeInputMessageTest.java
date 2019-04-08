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
package org.sentilo.platform.client.test.domain;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import org.junit.Before;
import org.junit.Test;
import org.sentilo.common.enums.SubscribeType;
import org.sentilo.platform.client.core.domain.SubscribeInputMessage;
import org.sentilo.platform.client.core.domain.SubscriptionParams;
import org.sentilo.platform.client.core.domain.factory.SubscribeInputMessageFactory;

public class SubscribeInputMessageTest {

  static final String ALARM_ID = "alarm1";
  static final String PROVIDER_ID = "provider1";
  static final String SENSOR_ID = "sensor1";
  static final String ENDPOINT_URL = "http://dev.connecta.cat";
  SubscriptionParams subscriptionParams = new SubscriptionParams("http://dev.connecta.cat");

  @Before
  public void setUp() {
    subscriptionParams = new SubscriptionParams("http://dev.connecta.cat");
  }

  @Test
  public void buildInvalidSubscription() {
    boolean invalid = false;
    try {
      SubscribeInputMessageFactory.buildSubscription(null);
    } catch (final IllegalArgumentException iae) {
      invalid = true;
    }

    assertTrue(invalid);
  }

  @Test
  public void buildAlarmSubscription() {
    final String[] resources = {ALARM_ID};
    final SubscribeInputMessage message = SubscribeInputMessageFactory.buildSubscription(SubscribeType.ALARM, subscriptionParams, resources);
    assertEquals(message.getType(), SubscribeType.ALARM);
    assertNotNull(message.getSubscriptionParams());
    assertEquals(message.getSubscriptionParams().getEndpoint(), ENDPOINT_URL);
    assertEquals(message.getResources().size(), 1);
    assertEquals(message.getResources().get(SubscribeInputMessage.ALARM_ID_KEY), ALARM_ID);
  }

  @Test
  public void buildAlarmSubscriptionWithoutResources() {
    final SubscribeInputMessage message = SubscribeInputMessageFactory.buildSubscription(SubscribeType.ALARM, subscriptionParams);
    assertEquals(message.getType(), SubscribeType.ALARM);
    assertNotNull(message.getSubscriptionParams());
    assertEquals(message.getSubscriptionParams().getEndpoint(), ENDPOINT_URL);
    assertEquals(message.getResources().size(), 0);
  }

  @Test
  public void buildSimpleAlarmSubscription() {
    final SubscribeInputMessage message = SubscribeInputMessageFactory.buildSubscription(SubscribeType.ALARM);
    assertEquals(message.getType(), SubscribeType.ALARM);
    assertNull(message.getSubscriptionParams());
    assertEquals(message.getResources().size(), 0);
  }

  @Test
  public void buildProviderOrdersSubscription() {
    final String[] resources = {PROVIDER_ID};
    final SubscribeInputMessage message = SubscribeInputMessageFactory.buildSubscription(SubscribeType.ORDER, subscriptionParams, resources);
    assertEquals(SubscribeType.ORDER, message.getType());
    assertNotNull(message.getSubscriptionParams());
    assertEquals(ENDPOINT_URL, message.getSubscriptionParams().getEndpoint());
    assertEquals(1, message.getResources().size());
    assertEquals(PROVIDER_ID, message.getResources().get(SubscribeInputMessage.PROVIDER_ID_KEY));
  }

  @Test
  public void buildProviderSensorOrdersSubscription() {
    final String[] resources = {PROVIDER_ID, SENSOR_ID};
    final SubscribeInputMessage message = SubscribeInputMessageFactory.buildSubscription(SubscribeType.ORDER, subscriptionParams, resources);
    assertEquals(SubscribeType.ORDER, message.getType());
    assertNotNull(message.getSubscriptionParams());
    assertEquals(ENDPOINT_URL, message.getSubscriptionParams().getEndpoint());
    assertEquals(2, message.getResources().size());
    assertEquals(PROVIDER_ID, message.getResources().get(SubscribeInputMessage.PROVIDER_ID_KEY));
    assertEquals(SENSOR_ID, message.getResources().get(SubscribeInputMessage.SENSOR_ID_KEY));
  }

  @Test
  public void buildOrderSubscriptionWithoutResources() {
    final SubscribeInputMessage message = SubscribeInputMessageFactory.buildSubscription(SubscribeType.ORDER, subscriptionParams);
    assertEquals(message.getType(), SubscribeType.ORDER);
    assertNotNull(message.getSubscriptionParams());
    assertEquals(message.getSubscriptionParams().getEndpoint(), ENDPOINT_URL);
    assertEquals(message.getResources().size(), 0);
  }

  @Test
  public void buildSimpleOrderSubscription() {
    final SubscribeInputMessage message = SubscribeInputMessageFactory.buildSubscription(SubscribeType.ORDER);
    assertEquals(message.getType(), SubscribeType.ORDER);
    assertNull(message.getSubscriptionParams());
    assertEquals(message.getResources().size(), 0);
  }

  @Test
  public void buildDataSubscription() {
    final String[] resources = {PROVIDER_ID, SENSOR_ID};
    final SubscribeInputMessage message = SubscribeInputMessageFactory.buildSubscription(SubscribeType.DATA, subscriptionParams, resources);
    assertEquals(message.getType(), SubscribeType.DATA);
    assertNotNull(message.getSubscriptionParams());
    assertEquals(message.getSubscriptionParams().getEndpoint(), ENDPOINT_URL);
    assertEquals(message.getResources().size(), 2);
    assertEquals(message.getResources().get(SubscribeInputMessage.PROVIDER_ID_KEY), PROVIDER_ID);
    assertEquals(message.getResources().get(SubscribeInputMessage.SENSOR_ID_KEY), SENSOR_ID);
  }

  @Test
  public void buildProviderDataSubscription() {
    final String[] resources = {PROVIDER_ID};
    final SubscribeInputMessage message = SubscribeInputMessageFactory.buildSubscription(SubscribeType.DATA, subscriptionParams, resources);
    assertEquals(message.getType(), SubscribeType.DATA);
    assertNotNull(message.getSubscriptionParams());
    assertEquals(message.getSubscriptionParams().getEndpoint(), ENDPOINT_URL);
    assertEquals(message.getResources().size(), 1);
    assertEquals(message.getResources().get(SubscribeInputMessage.PROVIDER_ID_KEY), PROVIDER_ID);
    assertNull(message.getResources().get(SubscribeInputMessage.SENSOR_ID_KEY));
  }

  @Test
  public void buildInvalidDataSubscription() {
    final String[] resources = {null, SENSOR_ID};
    boolean invalid = false;
    try {
      SubscribeInputMessageFactory.buildSubscription(SubscribeType.DATA, subscriptionParams, resources);
    } catch (final IllegalArgumentException iae) {
      invalid = true;
    }

    assertTrue(invalid);
  }

  @Test
  public void buildDataSubscriptionWithoutResources() {
    final SubscribeInputMessage message = SubscribeInputMessageFactory.buildSubscription(SubscribeType.DATA, subscriptionParams);
    assertEquals(message.getType(), SubscribeType.DATA);
    assertNotNull(message.getSubscriptionParams());
    assertEquals(message.getSubscriptionParams().getEndpoint(), ENDPOINT_URL);
    assertEquals(message.getResources().size(), 0);
  }
}
