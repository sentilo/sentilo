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
package org.sentilo.agent.common.test.listener;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.agent.common.listener.AbstractMessageListenerImpl;
import org.sentilo.agent.common.metrics.AgentMetricsCounter;
import org.sentilo.common.converter.StringMessageConverter;
import org.sentilo.common.domain.EventMessage;
import org.springframework.data.redis.connection.Message;
import org.springframework.data.redis.serializer.RedisSerializer;
import org.springframework.test.util.ReflectionTestUtils;

public class AbstractMessageListenerImplTest {

  @Mock
  private RedisSerializer<String> serializer;
  @Mock
  private StringMessageConverter eventConverter;
  @Mock
  private Message message;
  @Mock
  private EventMessage eventMessage;
  @Mock
  private AgentMetricsCounter metricsCounters;

  private AbstractMessageListenerImpl listener;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);

    listener = new AbstractMessageListenerImpl("TEST") {

      @Override
      public void doWithMessage(final EventMessage eventMessage) {
      }
    };

    ReflectionTestUtils.setField(listener, "eventConverter", eventConverter);
    ReflectionTestUtils.setField(listener, "serializer", serializer);
    ReflectionTestUtils.setField(listener, "metricsCounters", metricsCounters);
  }

  @Test
  public void getName() {
    Assert.assertEquals("TEST", listener.getName());
  }

  @Test
  public void onMessage() {
    final String topic = "/TEST_PATTERN";
    final String value = "{}";

    when(message.getBody()).thenReturn(value.getBytes());
    when(message.getChannel()).thenReturn(topic.getBytes());
    when(serializer.deserialize(value.getBytes())).thenReturn(value);
    when(serializer.deserialize(topic.getBytes())).thenReturn(topic);
    when(eventConverter.unmarshal(value, EventMessage.class)).thenReturn(eventMessage);

    listener.onMessage(message, topic.getBytes());

    verify(message).getBody();
    verify(message).getChannel();
    verify(serializer).deserialize(value.getBytes());
    verify(serializer).deserialize(topic.getBytes());
    verify(eventConverter).unmarshal(value, EventMessage.class);
    verify(metricsCounters).incrementInputEvents(1);
  }

}
