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
package org.sentilo.agent.common.test.repository;

import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.List;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.agent.common.repository.impl.PendingEventsRepositoryImpl;
import org.sentilo.agent.common.utils.Constants;
import org.sentilo.common.domain.EventMessage;
import org.springframework.data.redis.core.SetOperations;
import org.springframework.data.redis.core.StringRedisTemplate;

public class PendingEventRepositoryImplTest {

  private static final String AGENT_NAME = "mockAgent";

  @InjectMocks
  private PendingEventsRepositoryImpl repository;

  @Mock
  private StringRedisTemplate redisTemplate;

  @Mock
  private List<EventMessage> pendingEvents;

  @Mock
  private SetOperations<String, String> sOps;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    when(redisTemplate.opsForSet()).thenReturn(sOps);

    System.setProperty(Constants.SENTILO_AGENT_NAME_ENV, AGENT_NAME);
  }

  @Test
  public void storeEmptyPendingEvents() {
    when(pendingEvents.isEmpty()).thenReturn(true);

    repository.storePendingEvents(pendingEvents);

    verify(redisTemplate, times(0)).opsForSet();
  }

  @Test
  public void storePendingEvents() {
    when(pendingEvents.isEmpty()).thenReturn(false);
    when(pendingEvents.size()).thenReturn(23);

    repository.storePendingEvents(pendingEvents);

    verify(redisTemplate, times(3)).opsForSet();
  }

}
