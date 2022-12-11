/*
 * Sentilo
 *  
 * Original version 1.4 Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de
 * Barcelona. Modified by Opentrends adding support for multitenant deployments and SaaS. 
 * Modifications on version 1.5 Copyright (C) 2015 Opentrends Solucions i Sistemes, S.L.
 * 
 *   
 * This program is licensed and may be used, modified and redistributed under the terms  of the 
 * European Public License (EUPL), either version 1.1 or (at your option) any later version as soon 
 * as they are approved by the European Commission.
 *   
 * Alternatively, you may redistribute and/or modify this program under the terms of the GNU Lesser 
 * General Public License as published by the Free Software Foundation; either  version 3 of the 
 * License, or (at your option) any later version. 
 *   
 * Unless required by applicable law or agreed to in writing, software distributed under the License 
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR  CONDITIONS OF ANY KIND, either express 
 * or implied. 
 *   
 * See the licenses for the specific language governing permissions, limitations and more details.
 *   
 * You should have received a copy of the EUPL1.1 and the LGPLv3 licenses along with this program; 
 * if not, you may find them at: 
 *   
 * https://joinup.ec.europa.eu/software/page/eupl/licence-eupl http://www.gnu.org/licenses/   and 
 * https://www.gnu.org/licenses/lgpl.txt
 */
package org.sentilo.common.test.utils;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.junit.Assert;
import org.junit.Test;
import org.sentilo.common.enums.SubscribeType;
import org.sentilo.common.utils.MessagingUtils;

public class MessagingUtilsTest {

  @Test
  public void filterTopicsOfType() {
    final Set<String> topics = new HashSet<String>(Arrays.asList("/data/provider1/sensor23", "/data/provider1/sensor24", "/order/provider1"));
    Assert.assertTrue(MessagingUtils.filterTopicsOfType(topics, SubscribeType.DATA).size() == 2);
    Assert.assertTrue(MessagingUtils.filterTopicsOfType(topics, SubscribeType.ORDER).size() == 1);
    Assert.assertTrue(MessagingUtils.filterTopicsOfType(topics, SubscribeType.ALARM).size() == 0);
  }

  @Test
  public void buildTopic() {
    Assert.assertEquals("/data/provider1/sensor23", MessagingUtils.buildTopic(SubscribeType.DATA, "provider1", "sensor23").getTopic());
    Assert.assertEquals("/data/provider1", MessagingUtils.buildTopic(SubscribeType.DATA, "provider1").getTopic());
    Assert.assertEquals("/data/provider1", MessagingUtils.buildTopic(SubscribeType.DATA, "provider1", null).getTopic());
    Assert.assertEquals("/order/provider1/sensor23", MessagingUtils.buildTopic(SubscribeType.ORDER, "provider1", "sensor23").getTopic());
    Assert.assertEquals("/order/provider1", MessagingUtils.buildTopic(SubscribeType.ORDER, "provider1", null).getTopic());
    Assert.assertEquals("/alarm/alert1", MessagingUtils.buildTopic(SubscribeType.ALARM, "alert1").getTopic());
  }

  @Test
  public void buildCandidates() {
    final String topic = "/data/mockProvider/mockSensor";
    final String[] expected = {"/data/mockProvider/mockSensor", "/data/mockProvider", "/data"};

    final List<String> actual = MessagingUtils.buildCandidates(topic);

    Assert.assertEquals(Arrays.asList(expected), actual);
  }

  @Test
  public void formatTopicExpression() {
    final String topic1 = "/data/mockProvider*";
    final String topic2 = "/data/mockProvider";
    final String topic3 = "/data/mockProvider/mockSensor";

    Assert.assertEquals(topic2, MessagingUtils.formatTopicExpression(topic1));
    Assert.assertEquals(topic2, MessagingUtils.formatTopicExpression(topic2));
    Assert.assertEquals(topic3, MessagingUtils.formatTopicExpression(topic3));
  }

  @Test
  public void isValidSubscription() {
    Assert.assertTrue(MessagingUtils.isValidSubscription("/data/prov1/sensor1"));
    Assert.assertFalse(MessagingUtils.isValidSubscription("/data"));
    Assert.assertFalse(MessagingUtils.isValidSubscription("/datarr/prov1/sensor1"));
    Assert.assertFalse(MessagingUtils.isValidSubscription("/data/prov1/sensor1/sensor2"));
    Assert.assertFalse(MessagingUtils.isValidSubscription(""));
  }

}
