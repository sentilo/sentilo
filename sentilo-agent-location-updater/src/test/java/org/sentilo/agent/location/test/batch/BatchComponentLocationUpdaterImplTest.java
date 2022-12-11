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
package org.sentilo.agent.location.test.batch;

import static org.mockito.Matchers.anyListOf;
import static org.mockito.Matchers.argThat;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.List;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.sentilo.agent.common.repository.PendingEventsRepository;
import org.sentilo.agent.location.batch.BatchComponentLocationUpdaterImpl;
import org.sentilo.common.domain.EventMessage;
import org.sentilo.common.test.AbstractBaseTest;
import org.springframework.test.util.ReflectionTestUtils;

public class BatchComponentLocationUpdaterImplTest extends AbstractBaseTest {

  @Mock
  private PendingEventsRepository pendingEventRepository;

  @InjectMocks
  private BatchComponentLocationUpdaterImpl service;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @SuppressWarnings("unchecked")
  @Test
  public void add() {
    final EventMessage eventMessage = Mockito.mock(EventMessage.class);
    final List<EventMessage> sensorMsgsLocations = (List<EventMessage>) ReflectionTestUtils.getField(service, "sensorMsgsLocations");

    service.add(eventMessage);

    Assert.assertEquals(1, sensorMsgsLocations.size());
  }

  @Test
  public void save() {
    final int size = 5;
    final EventMessage eventMessage = Mockito.mock(EventMessage.class);

    for (int i = 0; i < size; i++) {
      service.add(eventMessage);
    }

    service.save();

    verify(pendingEventRepository).storePendingEvents(argThat(new EqualListSizeQueryMatcher<EventMessage>(size)));
  }

  @SuppressWarnings("unchecked")
  @Test
  public void save_with_errors() {
    final int size = 5;
    final EventMessage eventMessage = Mockito.mock(EventMessage.class);
    final EventMessage[] aFailedEvents = {eventMessage, eventMessage};
    final List<EventMessage> sensorMsgsLocations = (List<EventMessage>) ReflectionTestUtils.getField(service, "sensorMsgsLocations");

    when(pendingEventRepository.storePendingEvents(anyListOf(EventMessage.class))).thenReturn(Arrays.asList(aFailedEvents));

    for (int i = 0; i < size; i++) {
      service.add(eventMessage);
    }

    service.save();

    verify(pendingEventRepository).storePendingEvents(argThat(new EqualListSizeQueryMatcher<EventMessage>(size)));
    Assert.assertEquals(2, sensorMsgsLocations.size());
  }

}
