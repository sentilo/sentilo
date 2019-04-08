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
package org.sentilo.platform.client.integration;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.sentilo.common.domain.CatalogAlert;
import org.sentilo.platform.client.core.PlatformClientOperations;
import org.sentilo.platform.client.core.domain.CatalogAlertInputMessage;
import org.sentilo.platform.client.core.domain.CatalogAlertOutputMessage;
import org.sentilo.platform.client.core.exception.PlatformClientAccessException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.TestExecutionListeners;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.context.support.DependencyInjectionTestExecutionListener;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = "classpath:spring/sentilo-platform-client-integration.xml")
@TestExecutionListeners(listeners = {BackupAlertsCollectionHook.class, DependencyInjectionTestExecutionListener.class})
public class CatalogAlertServiceOperationsIntegrationTest {

  static String PROVIDER_ID = "testApp_provider";
  static String APP_ID = "testApp";

  @Value("${testApp.token}")
  private String tokenApp;
  @Value("${testApp.provider.token}")
  private String tokenProv;

  @Autowired
  protected PlatformClientOperations platformTemplate;

  @Test(expected = PlatformClientAccessException.class)
  public void registerWithoutAlerts() throws Exception {
    final CatalogAlertInputMessage message = new CatalogAlertInputMessage(PROVIDER_ID);
    platformTemplate.getCatalogOps().registerAlerts(message);
  }

  @Test
  public void doRegisterGetAndDelete() {
    deleteAlerts();
    getAlerts(0);
    registerAlerts();
    getAlerts(2);
    getAlerts(2, "EXTERNAL");
    getAlerts(0, "INTERNAL");
    deleteAlerts();
    getAlerts(0);
  }

  @Test
  public void doRegisterUpdateGetAndDelete() {
    deleteAlerts();
    getAlerts(0);
    registerAlerts();
    getAlerts(2);
    getAlerts(2, "EXTERNAL");
    getAlerts(0, "INTERNAL");
    updateAlerts();
    getAlerts(2);
    getAlerts(2, "EXTERNAL");
    getAlerts(0, "INTERNAL");
    deleteAlerts();
    getAlerts(0);
  }

  private void registerAlerts() {
    final CatalogAlertInputMessage message = new CatalogAlertInputMessage(PROVIDER_ID);
    message.setAlerts(buildAlertsToRegister());
    message.setIdentityToken(tokenProv);
    platformTemplate.getCatalogOps().registerAlerts(message);
  }

  private void updateAlerts() {
    final CatalogAlertInputMessage message = new CatalogAlertInputMessage(PROVIDER_ID);
    message.setAlerts(buildAlertsToUpdate());
    message.setIdentityToken(tokenProv);
    platformTemplate.getCatalogOps().updateAlerts(message);
  }

  private void getAlerts(final int expectedAlertsSize) {
    getAlerts(expectedAlertsSize, null);
  }

  private void getAlerts(final int expectedAlertsSize, final String type) {
    final CatalogAlertInputMessage message = new CatalogAlertInputMessage();
    message.setIdentityToken(tokenApp);
    if (StringUtils.hasText(type)) {
      final Map<String, String> parameters = new HashMap<String, String>();
      parameters.put("type", type);
      message.setParameters(parameters);
    }

    final CatalogAlertOutputMessage outputMessage = platformTemplate.getCatalogOps().getAuthorizedAlerts(message);
    assertNotNull(outputMessage);
    if (expectedAlertsSize == 0) {
      assertTrue(CollectionUtils.isEmpty(outputMessage.getAlerts()));
    } else {
      assertTrue("Expected " + expectedAlertsSize + " alerts but found " + outputMessage.getAlerts().size(),
          outputMessage.getAlerts().size() == expectedAlertsSize);
    }

  }

  private void deleteAlerts() {
    final CatalogAlertInputMessage message = new CatalogAlertInputMessage(PROVIDER_ID);
    message.setIdentityToken(tokenProv);
    platformTemplate.getCatalogOps().deleteAlerts(message);
  }

  private List<CatalogAlert> buildAlertsToUpdate() {
    final List<CatalogAlert> alerts = new ArrayList<CatalogAlert>();
    final CatalogAlert alert1 = buildAlert("MOCK_ALERT012", "ALERT012 updated", "mock alert 12 updated", PROVIDER_ID);
    final CatalogAlert alert2 = buildAlert("MOCK_ALERT013", "ALERT013 updated ", "mock alert 13 updated", PROVIDER_ID);
    alerts.add(alert1);
    alerts.add(alert2);

    return alerts;
  }

  private List<CatalogAlert> buildAlertsToRegister() {
    final List<CatalogAlert> alerts = new ArrayList<CatalogAlert>();
    final CatalogAlert alert1 = buildAlert("MOCK_ALERT012", "ALERT012", "mock alert 12", PROVIDER_ID);
    final CatalogAlert alert2 = buildAlert("MOCK_ALERT013", "ALERT013", "mock alert 13", PROVIDER_ID);
    alerts.add(alert1);
    alerts.add(alert2);

    return alerts;
  }

  private CatalogAlert buildAlert(final String id, final String name, final String description, final String entity) {
    final CatalogAlert alert = new CatalogAlert();
    alert.setId(id);
    alert.setName(name);
    alert.setDescription(description);
    alert.setEntity(entity);
    alert.setType("EXTERNAL");

    return alert;
  }
}
