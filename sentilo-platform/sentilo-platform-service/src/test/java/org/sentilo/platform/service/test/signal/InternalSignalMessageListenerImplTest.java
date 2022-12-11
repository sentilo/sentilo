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
package org.sentilo.platform.service.test.signal;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.domain.SignalMessage;
import org.sentilo.common.enums.SignalType;
import org.sentilo.platform.common.security.repository.EntityMetadataRepository;
import org.sentilo.platform.common.security.service.AuthorizationService;
import org.sentilo.platform.service.dao.SentiloSequenceUtils;
import org.sentilo.platform.service.signal.InternalSignalMessageListenerImpl;
import org.sentilo.platform.service.subscriber.SubscriberRegistry;

public class InternalSignalMessageListenerImplTest {

  @InjectMocks
  private InternalSignalMessageListenerImpl listener;

  @Mock
  private SentiloSequenceUtils sequenceUtils;

  @Mock
  private EntityMetadataRepository entityMetadataRepository;

  @Mock
  private AuthorizationService authorizationService;

  @Mock
  private SubscriberRegistry subsRegistry;

  @Mock
  private SignalMessage message;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void doWithMessage_when_delete_entities() {
    when(message.getSignal()).thenReturn(SignalType.DELETE_ENTITIES);
    listener.doWithMessage(message);

    verify(sequenceUtils).clearAll();
    verify(entityMetadataRepository).loadEntitiesMetadata();
    verify(authorizationService).loadActivePermissions();
  }

  @Test
  public void doWithMessage_when_delete_sensors() {
    when(message.getSignal()).thenReturn(SignalType.DELETE_SENSORS);
    listener.doWithMessage(message);

    verify(sequenceUtils).clearSensors();
    verify(sequenceUtils).clearAlerts();
  }

  @Test
  public void doWithMessage_when_delete_alerts() {
    when(message.getSignal()).thenReturn(SignalType.DELETE_ALERTS);
    listener.doWithMessage(message);

    verify(sequenceUtils).clearAlerts();
  }

  @Test
  public void doWithMessage_when_reload_entities() {
    when(message.getSignal()).thenReturn(SignalType.RELOAD_ENTITIES);
    listener.doWithMessage(message);

    verify(entityMetadataRepository).loadEntitiesMetadata();
    verify(authorizationService).loadActivePermissions();
  }

  @Test
  public void doWithMessage_when_reload_subscriptions() {
    when(message.getSignal()).thenReturn(SignalType.RELOAD_SUBSCRIPTIONS);
    listener.doWithMessage(message);

    verify(subsRegistry).reloadSubscriptions();
  }
}
