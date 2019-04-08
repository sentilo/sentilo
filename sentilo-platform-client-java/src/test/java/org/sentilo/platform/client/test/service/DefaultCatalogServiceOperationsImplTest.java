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
package org.sentilo.platform.client.test.service;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.converter.StringMessageConverter;
import org.sentilo.common.rest.RESTClient;
import org.sentilo.common.rest.RequestContext;
import org.sentilo.platform.client.core.domain.CatalogAlertInputMessage;
import org.sentilo.platform.client.core.domain.CatalogAlertOutputMessage;
import org.sentilo.platform.client.core.domain.CatalogDeleteInputMessage;
import org.sentilo.platform.client.core.domain.CatalogInputMessage;
import org.sentilo.platform.client.core.domain.CatalogOutputMessage;
import org.sentilo.platform.client.core.domain.PlatformClientInputMessage;
import org.sentilo.platform.client.core.service.impl.DefaultCatalogServiceOperationsImpl;

public class DefaultCatalogServiceOperationsImplTest {

  @InjectMocks
  private DefaultCatalogServiceOperationsImpl service;

  @Mock
  private RESTClient restClient;

  @Mock
  private CatalogInputMessage message;

  @Mock
  private CatalogDeleteInputMessage deleteMessage;

  @Mock
  private CatalogAlertInputMessage alertsMessage;

  @Mock
  private StringMessageConverter converter;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void getSensors() {
    service.getSensors(message);

    verify(restClient).get(any(RequestContext.class));
    verify(converter).unmarshal(any(String.class), eq(CatalogOutputMessage.class));
  }

  @Test
  public void registerSensors() {
    when(converter.marshal(any(PlatformClientInputMessage.class))).thenReturn("{}");

    service.registerSensors(message);

    verify(restClient).post(any(RequestContext.class));
  }

  @Test
  public void updateSensors() {
    service.updateSensors(message);

    verify(restClient).put(any(RequestContext.class));
  }

  @Test
  public void updateComponents() {
    service.updateComponents(message);

    verify(restClient).put(any(RequestContext.class));
  }

  @Test
  public void deleteProvider() {
    service.deleteProvider(deleteMessage);

    verify(restClient).delete(any(RequestContext.class));
  }

  @Test
  public void getAuthorizedAlerts() {
    service.getAuthorizedAlerts(alertsMessage);

    verify(restClient).get(any(RequestContext.class));
    verify(converter).unmarshal(any(String.class), eq(CatalogAlertOutputMessage.class));
  }

  @Test
  public void registerAlerts() {
    when(converter.marshal(any(PlatformClientInputMessage.class))).thenReturn("{}");

    service.registerAlerts(alertsMessage);

    verify(restClient).post(any(RequestContext.class));
  }

  @Test
  public void updateAlerts() {
    when(converter.marshal(any(PlatformClientInputMessage.class))).thenReturn("{}");

    service.updateAlerts(alertsMessage);

    verify(restClient).put(any(RequestContext.class));
  }

  @Test
  public void deleteAlerts() {
    service.deleteAlerts(alertsMessage);

    verify(restClient).delete(any(RequestContext.class));
  }
}
