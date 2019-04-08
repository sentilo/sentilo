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
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import org.junit.FixMethodOrder;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.MethodSorters;
import org.sentilo.common.enums.SubscribeType;
import org.sentilo.platform.client.core.PlatformTemplate;
import org.sentilo.platform.client.core.domain.SubscribeInputMessage;
import org.sentilo.platform.client.core.domain.SubscriptionParams;
import org.sentilo.platform.client.core.domain.SubscriptionsOutputMessage;
import org.sentilo.platform.client.core.domain.factory.SubscribeInputMessageFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.util.CollectionUtils;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = "classpath:spring/sentilo-platform-client-integration.xml")
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class SubscribeServiceOperationsIntegrationTest {

  @Autowired
  protected PlatformTemplate platformTemplate;

  @Test
  public void _1_subscribe() throws Exception {
    final SubscribeInputMessage message = SubscribeInputMessageFactory.buildSubscription(SubscribeType.DATA,
        new SubscriptionParams("http://dev.connecta.cat"), "testApp_provider", "sensor1");
    platformTemplate.getSubscribeOps().subscribe(message);
    assertTrue("No se ha realizado correctamente la llamada a la plataforma", true);
  }

  @Test
  public void _2_getAll() throws Exception {
    final SubscribeInputMessage message = new SubscribeInputMessage();
    // Recuperamos todas las subscripciones de la entidad appTest:sólo habrá una, la creada en el
    // paso anterior
    final SubscriptionsOutputMessage response = platformTemplate.getSubscribeOps().get(message);
    assertNotNull(response);
    assertTrue(!CollectionUtils.isEmpty(response.getSubscriptions()));
    assertTrue(response.getSubscriptions().size() == 1);
    assertEquals(SubscribeType.DATA.toString(), response.getSubscriptions().get(0).getType());
  }

  @Test
  public void _3_getAlarmSubscriptions() throws Exception {
    final SubscribeInputMessage message = new SubscribeInputMessage(SubscribeType.ALARM);
    final SubscriptionsOutputMessage response = platformTemplate.getSubscribeOps().get(message);
    assertNotNull(response);
    assertTrue(CollectionUtils.isEmpty(response.getSubscriptions()));
  }

  @Test
  public void _4_removeAlarmSubscriptions() throws Exception {
    final SubscribeInputMessage message = SubscribeInputMessageFactory.buildSubscription(SubscribeType.ALARM, "alarm1");
    platformTemplate.getSubscribeOps().remove(message);
    assertTrue("No se ha realizado correctamente la llamada a la plataforma", true);
  }

  @Test
  public void _5_removeAllSubscriptions() throws Exception {
    final SubscribeInputMessage message = new SubscribeInputMessage();
    platformTemplate.getSubscribeOps().remove(message);
    final SubscriptionsOutputMessage response = platformTemplate.getSubscribeOps().get(message);
    assertNotNull(response);
    assertTrue(CollectionUtils.isEmpty(response.getSubscriptions()));
  }
}
