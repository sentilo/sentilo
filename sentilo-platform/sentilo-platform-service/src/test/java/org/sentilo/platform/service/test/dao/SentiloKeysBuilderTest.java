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
package org.sentilo.platform.service.test.dao;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;
import org.sentilo.platform.service.dao.SentiloKeysBuilder;
import org.sentilo.platform.service.utils.PubSubConstants;

public class SentiloKeysBuilderTest {

  final String sid = "1";
  final String sdid = "2";
  final String aid = "3";
  final String amid = "4";
  final String soid = "5";
  final String pid = "6";
  final String providerId = "provider1";
  final String sensorId = "sensor1";
  final String alertId = "alert1";
  final String entityId = "entity1";

  @InjectMocks
  private SentiloKeysBuilder keysBuilder;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void getSensorObservationsKeyWithLongParam() {
    Assert.assertEquals("sid:" + sid + ":observations", keysBuilder.getSensorObservationsKey(new Long(sid)));
  }

  @Test
  public void getSensorObservationsKey() {
    Assert.assertEquals("sid:" + sid + ":observations", keysBuilder.getSensorObservationsKey(sid));
  }

  @Test
  public void getObservationKeyWithLongParam() {
    Assert.assertEquals("sdid:" + sdid, keysBuilder.getObservationKey(new Long(sdid)));
  }

  @Test
  public void getObservationKey() {
    Assert.assertEquals("sdid:" + sdid, keysBuilder.getObservationKey(sdid));
  }

  @Test
  public void getAlertMessagesKey() {
    Assert.assertEquals("aid:" + aid + ":alarms", keysBuilder.getAlertAlarmsKey(new Long(aid)));
  }

  @Test
  public void getMessageKey() {
    Assert.assertEquals("amid:" + amid, keysBuilder.getAlarmKey(new Long(amid)));
  }

  @Test
  public void getSensorOrdersKey() {
    Assert.assertEquals("sid:" + sid + ":orders", keysBuilder.getSensorOrdersKey(new Long(sid)));
  }

  @Test
  public void getOrderKey() {
    Assert.assertEquals("soid:" + soid, keysBuilder.getOrderKey(new Long(soid)));
  }

  @Test
  public void getProviderKey() {
    Assert.assertEquals("pid:" + pid, keysBuilder.getProviderKey(new Long(pid)));
  }

  @Test
  public void getReverseProviderKey() {
    Assert.assertEquals("provider:" + providerId + ":pid", keysBuilder.getReverseProviderKey(providerId));
  }

  @Test
  public void getSensorKey() {
    Assert.assertEquals("sid:" + sid, keysBuilder.getSensorKey(new Long(sid)));
  }

  @Test
  public void getProviderSensorsKey() {
    Assert.assertEquals("pid:" + pid + ":sensors", keysBuilder.getProviderSensorsKey(new Long(pid)));
  }

  @Test
  public void getReverseSensorKey() {
    Assert.assertEquals("sensor:" + providerId + ":" + sensorId + ":sid", keysBuilder.getReverseSensorKey(providerId, sensorId));
  }

  @Test
  public void getAlarmKey() {
    Assert.assertEquals("aid:" + aid, keysBuilder.getAlertKey(new Long(aid)));
  }

  @Test
  public void getReverseAlertKey() {
    Assert.assertEquals("alert:" + alertId + ":aid", keysBuilder.getReverseAlertKey(alertId));
  }

  @Test
  public void getSubscriptionKey() {
    Assert.assertEquals("subs" + PubSubConstants.REDIS_KEY_TOKEN + entityId, keysBuilder.getSubscriptionKey(entityId));
  }
}
