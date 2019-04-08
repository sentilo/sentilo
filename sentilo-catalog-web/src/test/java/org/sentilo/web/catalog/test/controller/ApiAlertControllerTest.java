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
package org.sentilo.web.catalog.test.controller;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyBoolean;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Matchers;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;
import org.sentilo.common.domain.CatalogAlert;
import org.sentilo.common.domain.CatalogAlertInputMessage;
import org.sentilo.common.domain.CatalogAlertResponseMessage;
import org.sentilo.common.domain.CatalogResponseMessage;
import org.sentilo.common.enums.AlertTriggerType;
import org.sentilo.common.test.AbstractBaseTest;
import org.sentilo.web.catalog.controller.api.ApiAlertController;
import org.sentilo.web.catalog.domain.Alert;
import org.sentilo.web.catalog.domain.Alert.Type;
import org.sentilo.web.catalog.domain.Permission;
import org.sentilo.web.catalog.security.CatalogUserDetails;
import org.sentilo.web.catalog.security.service.CatalogUserDetailsService;
import org.sentilo.web.catalog.service.AlertService;
import org.sentilo.web.catalog.service.PermissionService;
import org.sentilo.web.catalog.service.ProviderService;
import org.sentilo.web.catalog.validator.ApiAlertValidator;
import org.sentilo.web.catalog.validator.ApiValidationResults;
import org.springframework.dao.DataAccessException;
import org.springframework.util.CollectionUtils;

public class ApiAlertControllerTest extends AbstractBaseTest {

  private static final String MOCK_ENTITY = "mockEntity";
  private static final String MOCK_ERROR_MSG = "mockMessage";

  @InjectMocks
  ApiAlertController controller;

  @Mock
  private PermissionService permissionService;

  @Mock
  private AlertService alertService;

  @Mock
  private ProviderService providerService;

  @Mock
  private CatalogAlertInputMessage inputMessage;

  @Mock
  private ApiAlertValidator validator;

  @Mock
  private CatalogUserDetailsService userDetailsService;

  @Mock
  private CatalogUserDetails catalogUser;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);

    // initialize a CatalogUserDetails mock object
    when(catalogUser.getUsername()).thenReturn("testUser");
    when(userDetailsService.getCatalogUserDetails()).thenReturn(catalogUser);
  }

  @Test
  public void getAuthorizedAlerts() throws Exception {
    final List<Permission> permissions = generateRandomList(Permission.class);
    final List<Alert> alerts = generateRandomList(Alert.class);
    when(permissionService.getActivePermissions(any(String.class))).thenReturn(permissions);

    doAnswer(new Answer<List<Alert>>() {

      public List<Alert> answer(final InvocationOnMock invocation) throws Throwable {
        for (final Alert alert : alerts) {
          if (Math.random() > 0.5) {
            alert.setType(Type.INTERNAL);
            alert.setTrigger(AlertTriggerType.CHANGE);
            alert.setProviderId(MOCK_ENTITY);
            alert.setSensorId(MOCK_ENTITY);
            alert.setComponentId(MOCK_ENTITY + "." + MOCK_ENTITY);
          } else {
            alert.setApplicationId(MOCK_ENTITY);
            alert.setType(Type.EXTERNAL);
          }
          alert.setCreatedAt(new Date());
          alert.setUpdatedAt(new Date());
        }
        return alerts;
      }
    }).when(alertService).getAlertsByEntities(Matchers.<Collection<String>>any(), Matchers.<Map<String, Object>>any());

    final CatalogAlertResponseMessage response = controller.getAuthorizedAlerts(MOCK_ENTITY, Collections.<String, Object>emptyMap());

    verify(permissionService).getActivePermissions(MOCK_ENTITY);
    verify(alertService).getAlertsByEntities(Matchers.<Collection<String>>any(), Matchers.<Map<String, Object>>any());
    Assert.assertEquals(CatalogResponseMessage.OK, response.getCode());
    Assert.assertTrue(!CollectionUtils.isEmpty(response.getAlerts()));
    Assert.assertTrue(response.getAlerts().size() == alerts.size());
  }

  @Test
  public void getEmptyAuthorizedAlerts() {
    when(permissionService.getActivePermissions(any(String.class))).thenReturn(Collections.<Permission>emptyList());

    final CatalogAlertResponseMessage response = controller.getAuthorizedAlerts(MOCK_ENTITY, Collections.<String, Object>emptyMap());

    verify(permissionService).getActivePermissions(MOCK_ENTITY);
    verify(alertService).getAlertsByEntities(Matchers.<Collection<String>>any(), Matchers.<Map<String, Object>>any());
    Assert.assertEquals(CatalogResponseMessage.OK, response.getCode());
    Assert.assertTrue(CollectionUtils.isEmpty(response.getAlerts()));
  }

  @Test
  public void getAuthorizedAlertsWithError() {
    doThrow(MockDataAccessException.class).when(permissionService).getActivePermissions(any(String.class));

    final CatalogAlertResponseMessage response = controller.getAuthorizedAlerts(MOCK_ENTITY, Collections.<String, Object>emptyMap());

    verify(permissionService).getActivePermissions(MOCK_ENTITY);
    Assert.assertEquals(CatalogResponseMessage.INTERNAL_SERVER_ERROR, response.getCode());
    Assert.assertTrue(CollectionUtils.isEmpty(response.getAlerts()));
  }

  @Test
  public void createAlerts() throws Exception {
    when(inputMessage.getAlerts()).thenReturn(generateRandomList(CatalogAlert.class));

    final CatalogAlertResponseMessage response = controller.createAlerts(MOCK_ENTITY, inputMessage);

    Assert.assertEquals(CatalogResponseMessage.OK, response.getCode());
  }

  @Test
  public void backendExceptionWhenCreateAlerts() throws Exception {
    when(inputMessage.getAlerts()).thenReturn(generateRandomList(CatalogAlert.class));
    doThrow(MockDataAccessException.class).when(alertService).insertAll(Matchers.<Collection<Alert>>any());

    final CatalogAlertResponseMessage response = controller.createAlerts(MOCK_ENTITY, inputMessage);

    Assert.assertEquals(CatalogResponseMessage.INTERNAL_SERVER_ERROR, response.getCode());
  }

  @Test
  public void genericExceptionWhenCreateAlerts() throws Exception {
    when(inputMessage.getAlerts()).thenReturn(generateRandomList(CatalogAlert.class));
    doThrow(NullPointerException.class).when(alertService).insertAll(Matchers.<Collection<Alert>>any());

    final CatalogAlertResponseMessage response = controller.createAlerts(MOCK_ENTITY, inputMessage);

    Assert.assertEquals(CatalogResponseMessage.INTERNAL_SERVER_ERROR, response.getCode());
  }

  @Test
  public void badRequestWhenCreateAlerts() throws Exception {
    final List<CatalogAlert> catalogAlerts = generateRandomList(CatalogAlert.class);
    when(inputMessage.getAlerts()).thenReturn(catalogAlerts);
    doAnswer(new Answer<Void>() {

      public Void answer(final InvocationOnMock invocation) throws Throwable {
        final ApiValidationResults results = (ApiValidationResults) invocation.getArguments()[1];
        results.addErrorMessage(MOCK_ERROR_MSG);
        return null;
      }
    }).when(validator).validate(Matchers.<List<Alert>>any(), any(ApiValidationResults.class), anyBoolean());

    final CatalogAlertResponseMessage response = controller.createAlerts(MOCK_ENTITY, inputMessage);

    Assert.assertEquals(CatalogResponseMessage.BAD_REQUEST, response.getCode());
  }

  @Test
  public void updateUnknownAlerts() throws Exception {
    when(inputMessage.getAlerts()).thenReturn(generateRandomList(CatalogAlert.class));
    when(alertService.find(any(Alert.class))).thenReturn(new Alert());

    final CatalogAlertResponseMessage response = controller.updateAlerts(MOCK_ENTITY, inputMessage);

    Assert.assertEquals(CatalogResponseMessage.BAD_REQUEST, response.getCode());
    verify(alertService, times(inputMessage.getAlerts().size())).find(any(Alert.class));
  }

  @Test
  public void updateAlerts() throws Exception {
    final List<CatalogAlert> catalogAlerts = generateRandomList(CatalogAlert.class);

    doAnswer(new Answer<List<CatalogAlert>>() {

      public List<CatalogAlert> answer(final InvocationOnMock invocation) throws Throwable {
        final List<CatalogAlert> alerts = catalogAlerts;
        for (final CatalogAlert alert : alerts) {
          alert.setType(Type.EXTERNAL.name());
          alert.setEntity(MOCK_ENTITY);
        }
        return alerts;
      }
    }).when(inputMessage).getAlerts();

    doAnswer(new Answer<Alert>() {

      public Alert answer(final InvocationOnMock invocation) throws Throwable {
        final Alert alert = (Alert) invocation.getArguments()[0];
        alert.setType(Type.EXTERNAL);
        alert.setApplicationId(MOCK_ENTITY);
        return alert;
      }
    }).when(alertService).find(any(Alert.class));

    final CatalogAlertResponseMessage response = controller.updateAlerts(MOCK_ENTITY, inputMessage);

    Assert.assertEquals(CatalogResponseMessage.OK, response.getCode());
    verify(alertService, times(catalogAlerts.size())).find(any(Alert.class));
    verify(alertService).updateAll(Matchers.<Collection<Alert>>any());
  }

  @Test
  public void backendExceptionWhenUpdateAlerts() throws Exception {
    when(inputMessage.getAlerts()).thenReturn(generateRandomList(CatalogAlert.class));
    doThrow(MockDataAccessException.class).when(alertService).find(any(Alert.class));

    final CatalogAlertResponseMessage response = controller.updateAlerts(MOCK_ENTITY, inputMessage);

    Assert.assertEquals(CatalogResponseMessage.INTERNAL_SERVER_ERROR, response.getCode());
  }

  @Test
  public void deleteOwnAlerts() throws Exception {
    when(inputMessage.getAlertsIds()).thenReturn(null);
    final CatalogAlertResponseMessage response = controller.deleteAlerts(MOCK_ENTITY, inputMessage);

    verify(alertService).deleteOwnAlerts(MOCK_ENTITY);
    verify(alertService, times(0)).delete(Matchers.<Collection<Alert>>any());
    Assert.assertEquals(CatalogResponseMessage.OK, response.getCode());
  }

  @Test
  public void deleteAlerts() throws Exception {
    final String[] alertsIds = generateRandomList(String.class).toArray(new String[0]);
    when(inputMessage.getAlertsIds()).thenReturn(alertsIds);
    final CatalogAlertResponseMessage response = controller.deleteAlerts(MOCK_ENTITY, inputMessage);

    verify(alertService, times(0)).deleteOwnAlerts(MOCK_ENTITY);
    verify(alertService).deleteOwnAlerts(any(String[].class), any(String.class));
    Assert.assertEquals(CatalogResponseMessage.OK, response.getCode());
  }

  @Test
  public void genericExceptionWhenDeleteAlerts() throws Exception {
    when(inputMessage.getAlertsIds()).thenReturn(null);
    doThrow(NullPointerException.class).when(alertService).deleteOwnAlerts(MOCK_ENTITY);

    final CatalogAlertResponseMessage response = controller.deleteAlerts(MOCK_ENTITY, inputMessage);

    verify(alertService).deleteOwnAlerts(MOCK_ENTITY);
    verify(alertService, times(0)).delete(Matchers.<Collection<Alert>>any());
    Assert.assertEquals(CatalogResponseMessage.INTERNAL_SERVER_ERROR, response.getCode());
  }

  public class MockDataAccessException extends DataAccessException {

    private static final long serialVersionUID = 1L;
    private static final String MOCK_MESSAGE = "mock message";

    public MockDataAccessException() {
      super(MOCK_MESSAGE);
    }

  }

}
