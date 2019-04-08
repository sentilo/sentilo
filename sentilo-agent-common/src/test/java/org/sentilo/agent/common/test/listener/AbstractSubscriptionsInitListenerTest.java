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

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.agent.common.listener.AbstractSubscriptionsInitListener;
import org.sentilo.agent.common.service.AsyncPendingEventService;
import org.sentilo.agent.common.utils.Utils;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.data.redis.connection.MessageListener;
import org.springframework.data.redis.listener.RedisMessageListenerContainer;
import org.springframework.test.util.ReflectionTestUtils;

public class AbstractSubscriptionsInitListenerTest {

  private final String mockTopic = "/data/mock";

  @Mock
  private RedisMessageListenerContainer listenerContainer;

  @Mock
  private ContextRefreshedEvent event;

  @Mock
  private MessageListener messageListener;

  @InjectMocks
  private AbstractSubscriptionsInitListener subscriptionListener = new AbstractSubscriptionsInitListener() {

    @Override
    protected void subscribe() {
      registerSubscription(messageListener, Utils.buildTopic(mockTopic));
    }
  };

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void onApplicationContext() {
    subscriptionListener.onApplicationEvent(event);
    verify(listenerContainer).addMessageListener(messageListener, Utils.buildTopic(mockTopic));
  }

  @Test
  public void onApplicationContextWithPendingEventProcessor() {
    final AsyncPendingEventService apes = mock(AsyncPendingEventService.class);
    ReflectionTestUtils.setField(subscriptionListener, "pendingEventService", apes);
    subscriptionListener.onApplicationEvent(event);
    verify(listenerContainer).addMessageListener(messageListener, Utils.buildTopic(mockTopic));
    verify(apes).addMessageListener(messageListener, Utils.buildTopic(mockTopic));
  }

}
