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
package org.sentilo.agent.alert.test.utils;

import static org.mockito.Mockito.when;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.agent.alert.domain.InternalAlert;
import org.sentilo.agent.alert.utils.AlertUtils;
import org.sentilo.agent.alert.utils.Constants;

public class AlertUtilsTest {

  @Mock
  private InternalAlert alert;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void buildDataTopicAssociateToAlert() {
    final String providerId = "providerTest";
    final String sensorId = "sensorTest";
    final String expected = "/data/" + providerId + "/" + sensorId;
    when(alert.getProviderId()).thenReturn(providerId);
    when(alert.getSensorId()).thenReturn(sensorId);

    Assert.assertEquals(expected, AlertUtils.buildAlertDataTopic(alert));
  }

  @Test
  public void buildTopicToPublishAlert() {
    final String alertId = "alertTest";
    final String expected = "/alarm/" + alertId;
    when(alert.getId()).thenReturn(alertId);

    Assert.assertEquals(expected, AlertUtils.buildTopicToPublishAlert(alert));
  }

  @Test
  public void buildMessageToPublish() {
    final String alertId = "alertTest";
    final String value = "alarm message";
    final String topic = "/alarm/topic/mock";

    final String prefix = "{\"message\":\"alarm message\"";
    final String suffix =
        "\"topic\":\"/alarm/topic/mock\",\"type\":\"ALARM\",\"alert\":\"alertTest\",\"publisher\":\"SENTILO\",\"sender\":\"SENTILO\"}";

    when(alert.getId()).thenReturn(alertId);

    final String message = AlertUtils.buildMessageToPublish(alert, value, topic);
    Assert.assertTrue(message.startsWith(prefix));
    Assert.assertTrue(message.endsWith(suffix));
  }

  @Test
  public void buildFrozenAlertMember() {
    final String providerId = "providerTest";
    final String sensorId = "sensorTest";
    final String alertId = "alertTest";
    final String expected = providerId + Constants.REDIS_MEMBER_TOKEN + sensorId + Constants.REDIS_MEMBER_TOKEN + alertId;

    when(alert.getProviderId()).thenReturn(providerId);
    when(alert.getSensorId()).thenReturn(sensorId);
    when(alert.getId()).thenReturn(alertId);

    final String member = AlertUtils.buildFrozenAlertMember(alert);

    Assert.assertEquals(expected, member);

  }

  @Test
  public void transformNumber() throws Exception {
    final String number1 = "23";
    final float expected1 = 23f;

    final String number2 = "23.6";
    final float expected2 = 23.6f;

    final float actual1 = AlertUtils.transformNumber(number1).floatValue();
    final float actual2 = AlertUtils.transformNumber(number2).floatValue();

    Assert.assertTrue(expected1 == actual1);
    Assert.assertTrue(expected2 == actual2);
  }
}
