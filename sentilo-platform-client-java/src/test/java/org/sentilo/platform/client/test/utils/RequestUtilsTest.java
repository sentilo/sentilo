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
package org.sentilo.platform.client.test.utils;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import java.util.Date;

import org.junit.Test;
import org.sentilo.common.domain.OrderMessage;
import org.sentilo.common.domain.QueryFilterParams;
import org.sentilo.common.enums.SubscribeType;
import org.sentilo.common.rest.RequestContext;
import org.sentilo.common.rest.RequestParameters;
import org.sentilo.common.utils.DateUtils;
import org.sentilo.platform.client.core.domain.AlarmInputMessage;
import org.sentilo.platform.client.core.domain.CatalogAlertInputMessage;
import org.sentilo.platform.client.core.domain.DataInputMessage;
import org.sentilo.platform.client.core.domain.OrderInputMessage;
import org.sentilo.platform.client.core.domain.SubscribeInputMessage;
import org.sentilo.platform.client.core.domain.SubscriptionParams;
import org.sentilo.platform.client.core.domain.factory.SubscribeInputMessageFactory;
import org.sentilo.platform.client.core.utils.RequestUtils;

public class RequestUtilsTest {

  static final String PROVIDER_ID = "provider1";
  static final String SENSOR_ID = "sensor1";
  static final String ALARM_ID = "alarm1";
  static final String ORDER = "stop";
  static final String ALARM_MESSAGE = "message";
  static final String OBSERVATION = "23";
  static final SubscriptionParams subscriptionParams = new SubscriptionParams("http://dev.connecta.cat");

  @Test
  public void buildCatalogAlertRequestContext() {
    final CatalogAlertInputMessage message = new CatalogAlertInputMessage(PROVIDER_ID);
    final RequestContext rc = RequestUtils.buildContext(message);

    assertEquals("/catalog/alert/" + PROVIDER_ID, rc.getPath());
  }

  @Test
  public void buildOrderRequestContext() {
    final OrderInputMessage message = new OrderInputMessage(PROVIDER_ID, SENSOR_ID, new OrderMessage(ORDER));
    final RequestContext rc = RequestUtils.buildContext(message);

    assertEquals("/order/" + PROVIDER_ID + "/" + SENSOR_ID, rc.getPath());
  }

  @Test
  public void buildGlobalOrderRequestContext() {
    final OrderInputMessage message = new OrderInputMessage(PROVIDER_ID, new OrderMessage(ORDER));
    final RequestContext rc = RequestUtils.buildContext(message);

    assertEquals("/order/" + PROVIDER_ID, rc.getPath());
  }

  @Test
  public void buildAlarmRequestContext() {
    final AlarmInputMessage message = new AlarmInputMessage(ALARM_ID, ALARM_MESSAGE);
    final RequestContext rc = RequestUtils.buildContext(message);

    assertEquals("/alarm/" + ALARM_ID, rc.getPath());
  }

  @Test
  public void buildSubscribeGetPath() {
    SubscribeInputMessage message = SubscribeInputMessageFactory.buildSubscription(SubscribeType.ORDER);
    RequestContext rc = RequestUtils.buildContext(message);
    assertEquals("/subscribe/order", rc.getPath());

    message = SubscribeInputMessageFactory.buildSubscription(SubscribeType.DATA);
    rc = RequestUtils.buildContext(message);
    assertEquals("/subscribe/data", rc.getPath());

    message = SubscribeInputMessageFactory.buildSubscription(SubscribeType.ALARM);
    rc = RequestUtils.buildContext(message);
    assertEquals("/subscribe/alarm", rc.getPath());
  }

  @Test
  public void buildSubscribePutOrDeletePath() {
    SubscribeInputMessage message = SubscribeInputMessageFactory.buildSubscription(SubscribeType.ORDER, subscriptionParams, SENSOR_ID);
    RequestContext rc = RequestUtils.buildContext(message);
    assertEquals("/subscribe/order/" + SENSOR_ID, rc.getPath());

    message = SubscribeInputMessageFactory.buildSubscription(SubscribeType.ORDER, subscriptionParams);
    rc = RequestUtils.buildContext(message);
    assertEquals("/subscribe/order", rc.getPath());

    message = SubscribeInputMessageFactory.buildSubscription(SubscribeType.ALARM, subscriptionParams, ALARM_ID);
    rc = RequestUtils.buildContext(message);
    assertEquals("/subscribe/alarm/" + ALARM_ID, rc.getPath());

    message = SubscribeInputMessageFactory.buildSubscription(SubscribeType.ALARM, subscriptionParams);
    rc = RequestUtils.buildContext(message);
    assertEquals("/subscribe/alarm", rc.getPath());

    message = SubscribeInputMessageFactory.buildSubscription(SubscribeType.DATA, subscriptionParams, PROVIDER_ID, SENSOR_ID);
    rc = RequestUtils.buildContext(message);
    assertEquals("/subscribe/data/" + PROVIDER_ID + "/" + SENSOR_ID, rc.getPath());

    message = SubscribeInputMessageFactory.buildSubscription(SubscribeType.DATA, subscriptionParams, PROVIDER_ID);
    rc = RequestUtils.buildContext(message);
    assertEquals("/subscribe/data/" + PROVIDER_ID, rc.getPath());
  }

  @Test
  public void buildDataPath() {
    DataInputMessage message = new DataInputMessage(PROVIDER_ID, SENSOR_ID);
    RequestContext rc = RequestUtils.buildContext(message);
    assertEquals("/data/" + PROVIDER_ID + "/" + SENSOR_ID, rc.getPath());

    message = new DataInputMessage(PROVIDER_ID);
    rc = RequestUtils.buildContext(message);
    assertEquals("/data/" + PROVIDER_ID, rc.getPath());

    message = new DataInputMessage(PROVIDER_ID, SENSOR_ID, OBSERVATION);
    rc = RequestUtils.buildContext(message);
    assertEquals("/data/" + PROVIDER_ID + "/" + SENSOR_ID + "/" + OBSERVATION, rc.getPath());
  }

  @Test
  public void buildDataParameters() {
    final Integer limit = 10;
    final Date to = new Date();
    final Date from = new Date(System.currentTimeMillis() - 10000);
    final String toText = DateUtils.toStringTimestamp(to);
    final String fromText = DateUtils.toStringTimestamp(from);
    final String limitText = Integer.toString(limit);

    DataInputMessage message = new DataInputMessage(PROVIDER_ID, SENSOR_ID, new QueryFilterParams(from, to, limit));
    RequestParameters parameters = RequestUtils.buildParameters(message);
    assertEquals(parameters.size(), 3);
    assertEquals(parameters.get(RequestParameters.LIMIT), limitText);
    assertEquals(parameters.get(RequestParameters.FROM), fromText);
    assertEquals(parameters.get(RequestParameters.TO), toText);

    message = new DataInputMessage(PROVIDER_ID, SENSOR_ID, new QueryFilterParams(from.getTime(), to.getTime(), limit));
    parameters = RequestUtils.buildParameters(message);
    assertEquals(parameters.size(), 3);
    assertEquals(parameters.get(RequestParameters.LIMIT), limitText);
    assertEquals(parameters.get(RequestParameters.FROM), fromText);
    assertEquals(parameters.get(RequestParameters.TO), toText);

    message = new DataInputMessage(PROVIDER_ID, SENSOR_ID, new QueryFilterParams(from, to));
    parameters = RequestUtils.buildParameters(message);
    assertEquals(parameters.size(), 2);
    assertNull(parameters.get(RequestParameters.LIMIT));
    assertEquals(parameters.get(RequestParameters.FROM), fromText);
    assertEquals(parameters.get(RequestParameters.TO), toText);

    message = new DataInputMessage(PROVIDER_ID, SENSOR_ID, new QueryFilterParams(from, null));
    parameters = RequestUtils.buildParameters(message);
    assertEquals(parameters.size(), 1);
    assertNull(parameters.get(RequestParameters.LIMIT));
    assertEquals(parameters.get(RequestParameters.FROM), fromText);
    assertNull(parameters.get(RequestParameters.TO));

  }
}
