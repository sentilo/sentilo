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
package org.sentilo.agent.kafka.test.service;

import static org.mockito.Matchers.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.agent.kafka.repository.KafkaAgentRepository;
import org.sentilo.agent.kafka.service.CatalogService;
import org.sentilo.agent.kafka.service.impl.KafkaAgentServiceImpl;
import org.sentilo.common.domain.EventMessage;
import org.sentilo.common.enums.EventType;

public class KafkaServiceImplTest {

  @InjectMocks
  private KafkaAgentServiceImpl service;
  @Mock
  private CatalogService catalogService;
  @Mock
  private EventMessage eventMessage;
  @Mock
  private KafkaAgentRepository repository;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void processDataEvent() {
    when(eventMessage.getType()).thenReturn(EventType.DATA.name());

    service.process(eventMessage);

    verify(repository).publishMessageToKafka(any(EventMessage.class));
  }

  @Test
  public void processOrderEvent() {
    when(eventMessage.getType()).thenReturn(EventType.ORDER.name());

    service.process(eventMessage);

    verify(repository).publishMessageToKafka(any(EventMessage.class));
  }

  @Test
  public void processAlarmEvent() {
    final String alertId = "1";
    final String sensor = "mockSensor";
    final String provider = "mockProvider";
    when(eventMessage.getType()).thenReturn(EventType.ALARM.name());
    when(eventMessage.getAlert()).thenReturn(alertId);
    when(eventMessage.getSensor()).thenReturn(sensor);
    when(eventMessage.getProvider()).thenReturn(provider);

    service.process(eventMessage);

    verify(catalogService).getAlertAdditionalFields(alertId);
    verify(catalogService).getSensorType(provider, sensor);
    verify(repository).publishMessageToKafka(any(EventMessage.class));
  }
}
