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
package org.sentilo.platform.service.test.service;

import static org.mockito.Matchers.any;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.domain.CatalogAlertInputMessage;
import org.sentilo.common.domain.CatalogInputMessage;
import org.sentilo.common.domain.CatalogSensor;
import org.sentilo.common.exception.RESTClientException;
import org.sentilo.common.rest.RESTClient;
import org.sentilo.common.rest.RequestContext;
import org.sentilo.platform.common.exception.CatalogAccessException;
import org.sentilo.platform.service.impl.CatalogServiceImpl;
import org.springframework.test.util.ReflectionTestUtils;

public class CatalogServiceImplTest {

  private static final String MOCK_ENTITY = "mockEntity";
  private static final String MOCK_BODY = "{mockBody}";

  @Mock
  private CatalogInputMessage message;

  @Mock
  private CatalogAlertInputMessage alertMessage;

  @Mock
  private RESTClient restClient;

  private CatalogServiceImpl service;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    service = new CatalogServiceImpl();

    ReflectionTestUtils.setField(service, "restClient", restClient);

    when(alertMessage.getEntityId()).thenReturn(MOCK_ENTITY);
    when(alertMessage.getBody()).thenReturn(MOCK_BODY);
    when(message.getProviderId()).thenReturn(MOCK_ENTITY);
    when(message.getBody()).thenReturn(MOCK_BODY);
  }

  @Test
  public void insertSensors() {
    when(message.getSensors()).thenReturn(new ArrayList<CatalogSensor>());
    final String path = "api/provider/" + message.getProviderId();
    final RequestContext rc = new RequestContext(path, MOCK_BODY);

    service.insertSensors(message);

    verify(restClient).post(rc);
  }

  @Test
  public void updateSensorsOrComponents() {
    when(message.getSensors()).thenReturn(new ArrayList<CatalogSensor>());
    final String path = "api/provider/" + message.getProviderId();
    final RequestContext rc = new RequestContext(path, MOCK_BODY);

    service.updateSensorsOrComponents(message);

    verify(restClient).put(rc);
  }

  @Test
  public void deleteProvider() {
    final String path = "api/delete/provider/" + message.getProviderId();
    final RequestContext rc = new RequestContext(path, MOCK_BODY);

    service.deleteProvider(message);

    verify(restClient).put(rc);
  }

  @Test
  public void getCredentials() {
    final String path = "api/entities/metadata";
    final RequestContext rc = new RequestContext(path);

    service.getEntitiesMetadata();

    verify(restClient).get(rc);
  }

  @Test
  public void getPermissions() {
    final String path = "api/entities/permissions";
    final RequestContext rc = new RequestContext(path);
    service.getPermissions();

    verify(restClient).get(rc);
  }

  @Test
  public void getAlertsOwners() {
    final String path = "api/alert/owners";
    final RequestContext rc = new RequestContext(path);

    service.getAlertsOwners();

    verify(restClient).get(rc);
  }

  @Test(expected = CatalogAccessException.class)
  public void badRequestGetAlertsOwners() {
    doThrow(RESTClientException.class).when(restClient).get(any(RequestContext.class));

    service.getAlertsOwners();
  }

  @Test
  public void getAuthorizedAlerts() {
    final String path = "api/alert/entity/" + alertMessage.getEntityId();
    final RequestContext rc = new RequestContext(path, MOCK_BODY);

    service.getAuthorizedAlerts(alertMessage);

    verify(restClient).get(rc);
  }

  @Test(expected = CatalogAccessException.class)
  public void badRequestGetAuthorizedAlerts() {
    doThrow(RESTClientException.class).when(restClient).get(any(RequestContext.class));

    service.getAuthorizedAlerts(alertMessage);
  }

  @Test
  public void insertAlerts() {
    final String path = "api/alert/entity/" + alertMessage.getEntityId();
    final RequestContext rc = new RequestContext(path, MOCK_BODY);

    service.insertAlerts(alertMessage);

    verify(restClient).post(rc);
  }

  @Test(expected = CatalogAccessException.class)
  public void badRequestInsertAlerts() {
    doThrow(RESTClientException.class).when(restClient).post(any(RequestContext.class));

    service.insertAlerts(alertMessage);
  }

  @Test
  public void updateAlerts() {
    final String path = "api/alert/entity/" + alertMessage.getEntityId();
    final RequestContext rc = new RequestContext(path, MOCK_BODY);

    service.updateAlerts(alertMessage);

    verify(restClient).put(rc);
  }

  @Test(expected = CatalogAccessException.class)
  public void badRequestUpdateAlerts() {
    doThrow(RESTClientException.class).when(restClient).put(any(RequestContext.class));

    service.updateAlerts(alertMessage);
  }

  @Test
  public void deleteAlerts() {
    final String path = "api/alert/entity/" + alertMessage.getEntityId() + "/delete";
    final RequestContext rc = new RequestContext(path, MOCK_BODY);

    service.deleteAlerts(alertMessage);

    verify(restClient).put(rc);
  }

  @Test(expected = CatalogAccessException.class)
  public void badRequestDeleteAlerts() {
    doThrow(RESTClientException.class).when(restClient).put(any(RequestContext.class));

    service.deleteAlerts(alertMessage);
  }

}
