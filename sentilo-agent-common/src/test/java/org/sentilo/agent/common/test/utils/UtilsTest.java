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
package org.sentilo.agent.common.test.utils;

import org.junit.Assert;
import org.junit.Test;
import org.sentilo.agent.common.utils.Constants;
import org.sentilo.agent.common.utils.Utils;
import org.sentilo.common.enums.SubscribeType;
import org.springframework.data.redis.listener.ChannelTopic;
import org.springframework.data.redis.listener.PatternTopic;
import org.springframework.data.redis.listener.Topic;

public class UtilsTest {

  @Test
  public void buildPatternTopic() {
    final Topic topic = Utils.buildTopic(Constants.DATA);
    Assert.assertTrue(topic instanceof PatternTopic);
  }

  @Test
  public void buildChannelTopic() {
    final Topic topic = Utils.buildTopic("data/provider1/sensor1");
    Assert.assertTrue(topic instanceof ChannelTopic);
  }

  @Test
  public void isTopicPattern() {
    Assert.assertTrue(Utils.isTopicPattern(Constants.DATA));
    Assert.assertFalse(Utils.isTopicPattern(null));
    Assert.assertFalse(Utils.isTopicPattern("data/prov1/sensor1"));
  }

  @Test
  public void isTopicOfType() {
    Assert.assertTrue(Utils.isTopicOfType(Constants.DATA, SubscribeType.DATA));
    Assert.assertTrue(Utils.isTopicOfType(Constants.ORDER, SubscribeType.ORDER));
    Assert.assertTrue(Utils.isTopicOfType(Constants.ALARM, SubscribeType.ALARM));
    Assert.assertFalse(Utils.isTopicOfType(Constants.ALARM, null));
  }

  @Test
  public void isValidSubscription() {
    Assert.assertTrue(Utils.isValidSubscription("/data/prov1/sensor1"));
    Assert.assertFalse(Utils.isValidSubscription("/data"));
    Assert.assertFalse(Utils.isValidSubscription("/datarr/prov1/sensor1"));
    Assert.assertFalse(Utils.isValidSubscription("/data/prov1/sensor1/sensor2"));
    Assert.assertFalse(Utils.isValidSubscription(""));
  }
}
