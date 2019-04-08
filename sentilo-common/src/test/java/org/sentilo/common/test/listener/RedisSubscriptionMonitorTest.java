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
package org.sentilo.common.test.listener;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.listener.RedisSubscriptionMonitor;
import org.springframework.data.redis.connection.Message;
import org.springframework.data.redis.connection.MessageListener;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.listener.ChannelTopic;
import org.springframework.data.redis.listener.RedisMessageListenerContainer;
import org.springframework.data.redis.listener.Topic;
import org.springframework.test.util.ReflectionTestUtils;

public class RedisSubscriptionMonitorTest {

  final String topic = "/MONITOR/TEST_TOPIC";

  @Mock
  private RedisMessageListenerContainer listenerContainer;

  @Mock
  private RedisTemplate<String, String> redisTemplate;

  @InjectMocks
  private RedisSubscriptionMonitor monitor = new RedisSubscriptionMonitor() {

    @Override
    public String getProcessName() {
      return "TEST";
    }
  };

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void startWithListenerContainerStopped() {
    when(listenerContainer.isRunning()).thenReturn(false);

    monitor.start();

    Assert.assertFalse(monitor.isRunning());

  }

  @Test
  public void start() {
    doCallRealMethod().when(listenerContainer).afterPropertiesSet();

    when(listenerContainer.isRunning()).thenReturn(true);

    listenerContainer.afterPropertiesSet();
    monitor.start();

    Assert.assertTrue(monitor.isRunning());
  }

  @Test
  public void validateConnectionWhenNotRunning() {
    when(listenerContainer.isRunning()).thenReturn(false);

    monitor.validateConnection();
    verify(listenerContainer, times(0)).addMessageListener(any(MessageListener.class), any(Topic.class));
    verify(listenerContainer, times(0)).removeMessageListener(any(MessageListener.class), any(Topic.class));
  }

  @Test
  public void validateConnection() {
    start();

    monitor.validateConnection();

    verify(redisTemplate).convertAndSend(eq(topic), eq("PING SUBSCRIPTION"));
  }

  @Test
  public void restartListenerContainer() {
    start();
    ReflectionTestUtils.setField(monitor, "countPendingEvents", Integer.valueOf(4));
    when(listenerContainer.isRunning()).thenReturn(false);

    monitor.validateConnection();

    verify(listenerContainer).stop();
    verify(listenerContainer).start();
    verify(redisTemplate, times(0)).convertAndSend(eq(topic), eq("PING SUBSCRIPTION"));
    Assert.assertEquals(Integer.valueOf(0), ReflectionTestUtils.getField(monitor, "countPendingEvents"));

  }

  @Test
  public void stopWhenNotRunning() {
    when(listenerContainer.isRunning()).thenReturn(false);

    monitor.stop();

    verify(listenerContainer, times(0)).removeMessageListener(monitor, new ChannelTopic(topic));
    Assert.assertFalse(monitor.isRunning());
  }

  @Test
  public void stop() {
    start();
    monitor.stop();

    verify(listenerContainer).removeMessageListener(monitor, new ChannelTopic(topic));
    Assert.assertFalse(monitor.isRunning());
  }

  @Test
  public void publishPing() {
    start();

    monitor.publishPing();
    verify(redisTemplate).convertAndSend(eq(topic), eq("PING SUBSCRIPTION"));
  }

  @Test
  public void onMessage() {
    ReflectionTestUtils.setField(monitor, "countPendingEvents", Integer.valueOf(2));
    monitor.onMessage(Mockito.mock(Message.class), topic.getBytes());

    Assert.assertEquals(Integer.valueOf(1), ReflectionTestUtils.getField(monitor, "countPendingEvents"));
  }

}
