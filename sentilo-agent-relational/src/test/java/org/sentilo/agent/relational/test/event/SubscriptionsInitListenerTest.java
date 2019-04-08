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
package org.sentilo.agent.relational.test.event;

import static org.mockito.Matchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Properties;
import java.util.Set;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.agent.relational.event.SubscriptionsInitListener;
import org.sentilo.agent.relational.service.DataTrackService;
import org.springframework.context.ApplicationContext;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.data.redis.connection.MessageListener;
import org.springframework.data.redis.listener.RedisMessageListenerContainer;
import org.springframework.data.redis.listener.Topic;

public class SubscriptionsInitListenerTest {

  @Mock
  private Properties subscriptionsDef;
  @Mock
  private RedisMessageListenerContainer listenerContainer;
  @Mock
  private DataTrackService dataTrackService;
  @Mock
  private ApplicationContext context;

  @InjectMocks
  private SubscriptionsInitListener listener;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void onApplicationEventWithoutSubscriptions() {
    when(subscriptionsDef.isEmpty()).thenReturn(true);

    listener.onApplicationEvent(new ContextRefreshedEvent(context));

    verify(listenerContainer, times(0)).addMessageListener(any(MessageListener.class), any(Topic.class));
  }

  @Test
  public void onApplicationEventWithInvalidSubscriptions() {
    final Set<String> subscriptionKeys = new HashSet<String>(Arrays.asList("/data/PROV1*"));
    when(subscriptionsDef.isEmpty()).thenReturn(false);
    when(subscriptionsDef.stringPropertyNames()).thenReturn(subscriptionKeys);
    when(subscriptionsDef.getProperty(any(String.class))).thenReturn(null);

    listener.onApplicationEvent(new ContextRefreshedEvent(context));

    verify(listenerContainer, times(0)).addMessageListener(any(MessageListener.class), any(Topic.class));
  }

  @Test
  public void onApplicationEvent() {
    final String dataSource = "sentiloDs";
    final Set<String> subscriptionKeys = new HashSet<String>(Arrays.asList("/data/PROV1*", "/data/PROV2/sensor1"));
    when(subscriptionsDef.isEmpty()).thenReturn(false);
    when(subscriptionsDef.stringPropertyNames()).thenReturn(subscriptionKeys);
    when(subscriptionsDef.getProperty(any(String.class))).thenReturn(dataSource);

    listener.onApplicationEvent(new ContextRefreshedEvent(context));

    verify(listenerContainer, times(2)).addMessageListener(any(MessageListener.class), any(Topic.class));
  }
}
