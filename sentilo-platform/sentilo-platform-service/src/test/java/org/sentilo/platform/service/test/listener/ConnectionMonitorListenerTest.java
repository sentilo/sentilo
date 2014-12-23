/*
 * Sentilo
 * 
 * Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de Barcelona.
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
package org.sentilo.platform.service.test.listener;

import static org.mockito.Matchers.any;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.platform.service.listener.ConnectionMonitorListener;
import org.springframework.data.redis.RedisConnectionFailureException;
import org.springframework.data.redis.connection.MessageListener;
import org.springframework.data.redis.listener.RedisMessageListenerContainer;
import org.springframework.data.redis.listener.Topic;

public class ConnectionMonitorListenerTest {

  @Mock
  private RedisMessageListenerContainer listenerContainer;

  @InjectMocks
  private ConnectionMonitorListener listener;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void startWithListenerStopped() {
    when(listenerContainer.isRunning()).thenReturn(false);

    listener.start();

    Assert.assertFalse(listener.isRunning());

  }

  @Test
  public void start() {
    doCallRealMethod().when(listenerContainer).afterPropertiesSet();
    when(listenerContainer.isRunning()).thenReturn(true);

    listenerContainer.afterPropertiesSet();
    listener.start();

    Assert.assertTrue(listener.isRunning());
  }

  @Test
  public void validateConnectionWhenNotRunning() {
    when(listenerContainer.isRunning()).thenReturn(false);

    listener.validateConnection();
    verify(listenerContainer, times(0)).addMessageListener(any(MessageListener.class), any(Topic.class));
    verify(listenerContainer, times(0)).removeMessageListener(any(MessageListener.class), any(Topic.class));
  }

  @Test
  public void validateConnectionWhenRunning() {
    start();
    listener.validateConnection();

    verify(listenerContainer).addMessageListener(any(MessageListener.class), any(Topic.class));
    verify(listenerContainer).removeMessageListener(any(MessageListener.class), any(Topic.class));
    verify(listenerContainer, times(0)).stop();
  }

  @Test
  public void validateConnectionWhenThowsException() {
    doThrow(RedisConnectionFailureException.class).when(listenerContainer).removeMessageListener(any(MessageListener.class), any(Topic.class));

    start();
    listener.validateConnection();

    verify(listenerContainer).addMessageListener(any(MessageListener.class), any(Topic.class));
    verify(listenerContainer).removeMessageListener(any(MessageListener.class), any(Topic.class));
    verify(listenerContainer).stop();
  }

  @Test
  public void stopWhenNotRunning() {
    when(listenerContainer.isRunning()).thenReturn(false);

    listener.stop();

    verify(listenerContainer, times(0)).removeMessageListener(any(MessageListener.class), any(Topic.class));
    Assert.assertFalse(listener.isRunning());
  }

  @Test
  public void stop() {
    start();
    listener.stop();

    verify(listenerContainer).removeMessageListener(any(MessageListener.class), any(Topic.class));
    Assert.assertFalse(listener.isRunning());
  }
}
