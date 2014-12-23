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
package org.sentilo.platform.service.test.service;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
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
import org.sentilo.common.rest.RESTClient;
import org.sentilo.common.rest.RequestParameters;
import org.sentilo.platform.common.exception.CatalogAccessException;
import org.sentilo.platform.service.impl.CatalogServiceImpl;
import org.springframework.web.client.RestClientException;

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
    service.setRestClient(restClient);

    when(alertMessage.getEntityId()).thenReturn(MOCK_ENTITY);
    when(alertMessage.getBody()).thenReturn(MOCK_BODY);
    when(message.getProviderId()).thenReturn(MOCK_ENTITY);
    when(message.getBody()).thenReturn(MOCK_BODY);
  }

  @Test
  public void insertSensors() {
    when(message.getSensors()).thenReturn(new ArrayList<CatalogSensor>());
    final String path = "api/provider/" + message.getProviderId();

    service.insertSensors(message);

    verify(restClient).post(path, MOCK_BODY);
  }

  @Test
  public void updateSensorsOrComponents() {
    when(message.getSensors()).thenReturn(new ArrayList<CatalogSensor>());
    final String path = "api/provider/" + message.getProviderId();

    service.updateSensorsOrComponents(message);

    verify(restClient).put(path, MOCK_BODY);
  }

  @Test
  public void deleteProvider() {
    final String path = "api/delete/provider/" + message.getProviderId();

    service.deleteProvider(message);

    verify(restClient).put(path, MOCK_BODY);
  }

  @Test
  public void getCredentials() {
    final String path = "api/credentials";
    service.getCredentials();

    verify(restClient).get(path);
  }

  @Test
  public void getPermissions() {
    final String path = "api/permissions";
    service.getPermissions();

    verify(restClient).get(path);
  }

  @Test
  public void getAlertsOwners() {
    final String path = "api/alert/owners";
    service.getAlertsOwners();

    verify(restClient).get(path);
  }

  @Test(expected = CatalogAccessException.class)
  public void badRequestGetAlertsOwners() {
    doThrow(RestClientException.class).when(restClient).get(any(String.class));

    service.getAlertsOwners();
  }

  @Test
  public void getAuthorizedAlerts() {
    final String path = "api/alert/entity/" + alertMessage.getEntityId();

    service.getAuthorizedAlerts(alertMessage);

    verify(restClient).get(eq(path), any(RequestParameters.class));
  }

  @Test(expected = CatalogAccessException.class)
  public void badRequestGetAuthorizedAlerts() {
    doThrow(RestClientException.class).when(restClient).get(any(String.class), any(RequestParameters.class));

    service.getAuthorizedAlerts(alertMessage);
  }

  @Test
  public void insertAlerts() {
    final String path = "api/alert/entity/" + alertMessage.getEntityId();

    service.insertAlerts(alertMessage);

    verify(restClient).post(path, MOCK_BODY);
  }

  @Test(expected = CatalogAccessException.class)
  public void badRequestInsertAlerts() {
    doThrow(RestClientException.class).when(restClient).post(any(String.class), any(String.class));

    service.insertAlerts(alertMessage);
  }

  @Test
  public void updateAlerts() {
    final String path = "api/alert/entity/" + alertMessage.getEntityId();

    service.updateAlerts(alertMessage);

    verify(restClient).put(path, MOCK_BODY);
  }

  @Test(expected = CatalogAccessException.class)
  public void badRequestUpdateAlerts() {
    doThrow(RestClientException.class).when(restClient).put(any(String.class), any(String.class));

    service.updateAlerts(alertMessage);
  }

  @Test
  public void deleteAlerts() {
    final String path = "api/alert/entity/" + alertMessage.getEntityId() + "/delete";

    service.deleteAlerts(alertMessage);

    verify(restClient).put(path, MOCK_BODY);
  }

  @Test(expected = CatalogAccessException.class)
  public void badRequestDeleteAlerts() {
    doThrow(RestClientException.class).when(restClient).put(any(String.class), any(String.class));

    service.deleteAlerts(alertMessage);
  }

}
