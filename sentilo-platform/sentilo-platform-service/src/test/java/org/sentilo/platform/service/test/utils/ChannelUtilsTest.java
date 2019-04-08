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
package org.sentilo.platform.service.test.utils;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import org.junit.Assert;
import org.junit.Test;
import org.sentilo.common.enums.SubscribeType;
import org.sentilo.platform.common.domain.Observation;
import org.sentilo.platform.common.domain.Subscription;
import org.sentilo.platform.service.utils.ChannelUtils;
import org.sentilo.platform.service.utils.ChannelUtils.PubSubChannelPrefix;
import org.springframework.data.redis.listener.ChannelTopic;
import org.springframework.data.redis.listener.PatternTopic;

public class ChannelUtilsTest {

  @Test
  public void translateSubscriptionType() {
    Assert.assertEquals(PubSubChannelPrefix.alarm, ChannelUtils.translateSubscriptionType(SubscribeType.ALARM));
    Assert.assertEquals(PubSubChannelPrefix.data, ChannelUtils.translateSubscriptionType(SubscribeType.DATA));
    Assert.assertEquals(PubSubChannelPrefix.order, ChannelUtils.translateSubscriptionType(SubscribeType.ORDER));
  }

  @Test
  public void isTopicOfType() {
    Assert.assertTrue(ChannelUtils.isTopicOfType("/data/provider1/sensor23", SubscribeType.DATA));
    Assert.assertTrue(ChannelUtils.isTopicOfType("/order/provider1", SubscribeType.ORDER));
    Assert.assertTrue(ChannelUtils.isTopicOfType("/alarm/alert1", SubscribeType.ALARM));

    Assert.assertFalse(ChannelUtils.isTopicOfType("/order/provider1/sensor23", SubscribeType.DATA));

  }

  @Test
  public void filterTopicsOfType() {
    final Set<String> topics = new HashSet<String>(Arrays.asList("/data/provider1/sensor23", "/data/provider1/sensor24", "/order/provider1"));
    Assert.assertTrue(ChannelUtils.filterTopicsOfType(topics, SubscribeType.DATA).size() == 2);
    Assert.assertTrue(ChannelUtils.filterTopicsOfType(topics, SubscribeType.ORDER).size() == 1);
    Assert.assertTrue(ChannelUtils.filterTopicsOfType(topics, SubscribeType.ALARM).size() == 0);
  }

  @Test
  public void getChannel() {
    final Observation observation = new Observation("provider1", "sensor1", "23");
    Assert.assertEquals("/data/provider1/sensor1", ChannelUtils.getChannel(observation).getTopic());
  }

  @Test
  public void getSubscription() {
    final String entityId = "provider2";
    final String dataChannel = "/data/provider1/sensor23";
    final String orderChannel = "/order/provider1";
    final String alarmChannel = "/alarm/alert1";
    final String notificationParamsStr = "{\"endpoint\":\"http://127.0.0.1/endpoint\", \"secretCallbackKey\":\"secret\"}";

    final Subscription dataSubscription = ChannelUtils.getSubscription(entityId, dataChannel, notificationParamsStr);
    Assert.assertNotNull(dataSubscription);
    Assert.assertEquals("http://127.0.0.1/endpoint", dataSubscription.getNotificationParams().getEndpoint());
    Assert.assertEquals("secret", dataSubscription.getNotificationParams().getSecretCallbackKey());
    Assert.assertEquals(SubscribeType.DATA, dataSubscription.getType());
    Assert.assertEquals("provider2", dataSubscription.getSourceEntityId());
    Assert.assertEquals("provider1", dataSubscription.getOwnerEntityId());

    final Subscription orderSubscription = ChannelUtils.getSubscription(entityId, orderChannel, notificationParamsStr);
    Assert.assertNotNull(orderSubscription);
    Assert.assertEquals("http://127.0.0.1/endpoint", orderSubscription.getNotificationParams().getEndpoint());
    Assert.assertEquals("secret", orderSubscription.getNotificationParams().getSecretCallbackKey());
    Assert.assertEquals(SubscribeType.ORDER, orderSubscription.getType());
    Assert.assertEquals("provider2", orderSubscription.getSourceEntityId());
    Assert.assertEquals("provider1", orderSubscription.getOwnerEntityId());

    final Subscription alarmSubscription = ChannelUtils.getSubscription(entityId, alarmChannel, notificationParamsStr);
    Assert.assertNotNull(alarmSubscription);
    Assert.assertEquals("http://127.0.0.1/endpoint", alarmSubscription.getNotificationParams().getEndpoint());
    Assert.assertEquals("secret", alarmSubscription.getNotificationParams().getSecretCallbackKey());
    Assert.assertEquals(SubscribeType.ALARM, alarmSubscription.getType());
    Assert.assertEquals("provider2", alarmSubscription.getSourceEntityId());
    Assert.assertNull(alarmSubscription.getOwnerEntityId());
  }

  @Test
  public void buildTopic() {
    Assert.assertEquals("/data/provider1/sensor23", ChannelUtils.buildTopic(PubSubChannelPrefix.data, "provider1", "sensor23").getTopic());
    Assert.assertEquals("/data/provider1", ChannelUtils.buildTopic(PubSubChannelPrefix.data, "provider1").getTopic());
    Assert.assertEquals("/data/provider1*", ChannelUtils.buildTopic(PubSubChannelPrefix.data, "provider1", null).getTopic());
    Assert.assertEquals("/order/provider1/sensor23", ChannelUtils.buildTopic(PubSubChannelPrefix.order, "provider1", "sensor23").getTopic());
    Assert.assertEquals("/order/provider1*", ChannelUtils.buildTopic(PubSubChannelPrefix.order, "provider1", null).getTopic());
    Assert.assertEquals("/alarm/alert1", ChannelUtils.buildTopic(PubSubChannelPrefix.alarm, "alert1").getTopic());

  }

  @Test
  public void buildTopicFromString() {
    Assert.assertTrue(ChannelUtils.buildTopic("/data/provider1/sensor23") instanceof ChannelTopic);
    Assert.assertTrue(ChannelUtils.buildTopic("/data/provider1*") instanceof PatternTopic);

  }

  @Test
  public void isPatternTopic() {
    Assert.assertFalse(ChannelUtils.isPatternTopic("/data/provider1/sensor23"));
    Assert.assertTrue(ChannelUtils.isPatternTopic("/data/provider1*"));
  }

  @Test
  public void channelToPattern() {
    Assert.assertEquals("/data/provider1*", ChannelUtils.channelToPattern("/data/provider1/sensor23"));
    Assert.assertEquals("/data/provider1*", ChannelUtils.channelToPattern("/data/provider1*"));
  }
}
