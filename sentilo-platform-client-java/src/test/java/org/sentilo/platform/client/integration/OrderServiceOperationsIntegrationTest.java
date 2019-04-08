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
package org.sentilo.platform.client.integration;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.junit.FixMethodOrder;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.MethodSorters;
import org.sentilo.common.domain.OrderMessage;
import org.sentilo.platform.client.core.PlatformClientOperations;
import org.sentilo.platform.client.core.domain.OrderInputMessage;
import org.sentilo.platform.client.core.domain.OrdersOutputMessage;
import org.sentilo.platform.client.core.exception.PlatformClientAccessException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.util.CollectionUtils;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = "classpath:spring/sentilo-platform-client-integration.xml")
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class OrderServiceOperationsIntegrationTest {

  static String PROVIDER_ID = "testApp_provider";
  static String APP_ID = "testApp";
  static String SENSOR1 = "sensor1";

  @Autowired
  protected PlatformClientOperations platformTemplate;

  @Test
  public void _01_publish() throws Exception {
    final OrderInputMessage message = new OrderInputMessage(PROVIDER_ID, SENSOR1, new OrderMessage("stop"));
    platformTemplate.getOrderOps().publish(message);
    assertTrue("No se ha realizado correctamente la llamada a la plataforma", true);
  }

  @Test
  public void _02_publishWithoutOrderMessage() throws Exception {
    final OrderInputMessage message = new OrderInputMessage(PROVIDER_ID);
    boolean error = false;
    try {
      platformTemplate.getOrderOps().publish(message);
    } catch (final PlatformClientAccessException e) {
      error = true;
    }
    assertTrue("No se ha realizado correctamente la llamada a la plataforma", error);
  }

  @Test
  public void _03_publishWithoutPermission() throws Exception {
    final OrderInputMessage message = new OrderInputMessage("appDemo", new OrderMessage("stop"));
    boolean error = false;
    try {
      platformTemplate.getOrderOps().publish(message);
    } catch (final PlatformClientAccessException e) {
      error = true;
    }
    assertTrue("No se ha realizado correctamente la llamada a la plataforma", error);
  }

  @Test
  public void _04_getLastOrdersFromSensor() throws Exception {
    final OrderInputMessage message = new OrderInputMessage(PROVIDER_ID, SENSOR1);
    final OrdersOutputMessage response = platformTemplate.getOrderOps().getLastOrders(message);
    assertTrue(response != null && !CollectionUtils.isEmpty(response.getOrders()));
    assertEquals(1, response.getOrders().size());
  }

  @Test
  public void _05_getLastOrdersFromProvider() throws Exception {
    final OrderInputMessage message = new OrderInputMessage(PROVIDER_ID);
    final OrdersOutputMessage response = platformTemplate.getOrderOps().getLastOrders(message);
    assertTrue(response != null && !CollectionUtils.isEmpty(response.getSensors()));
    assertEquals(1, response.getSensors().size());
  }

}
