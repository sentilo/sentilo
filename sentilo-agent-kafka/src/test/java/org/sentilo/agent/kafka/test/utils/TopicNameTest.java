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
package org.sentilo.agent.kafka.test.utils;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.agent.kafka.utils.TopicName;
import org.sentilo.agent.kafka.utils.TopicName.TopicNameMode;
import org.sentilo.common.domain.EventMessage;

public class TopicNameTest {

  @Mock
  private EventMessage em;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);

  }

  @Test
  public void createTopicName() {

    when(em.getType()).thenReturn("FOO");
    when(em.getProvider()).thenReturn("bar");
    when(em.getSensor()).thenReturn("sensor");
    when(em.getSensorType()).thenReturn("type");

    for (final TopicNameMode m : TopicNameMode.values()) {
      TopicName.setTopicNameMode(m);
      TopicName.setTopicPrefix("topicPrefix");
      TopicName.setTopicSeparator("._");

      final String topicName = TopicName.createTopicName(em);

      switch (m) {

        case topicPerSensor:
          assertEquals(topicName, "topicPrefix" + "._" + em.getType().toLowerCase() + "._" + em.getProvider() + "._" + em.getSensor());
          break;
        case topicPerProvider:
          assertEquals(topicName, "topicPrefix" + "._" + em.getType().toLowerCase() + "._" + em.getProvider());
          break;
        case topicPerSensorType:
          assertEquals(topicName, "topicPrefix" + "._" + em.getType().toLowerCase() + "._" + em.getSensorType());
          break;
        case topicPerMessageType:
          assertEquals(topicName, "topicPrefix" + "._" + em.getType().toLowerCase());
          break;
        case singleTopic:
          assertEquals(topicName, "topicPrefix");
          break;
        default:
          fail("Untested topic mode");

      }

    }

  }

}
