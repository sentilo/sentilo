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

import java.util.List;

import org.sentilo.common.domain.CatalogAlert;
import org.sentilo.platform.client.core.PlatformClientOperations;
import org.sentilo.platform.client.core.domain.CatalogAlertInputMessage;
import org.sentilo.platform.client.core.domain.CatalogAlertOutputMessage;
import org.springframework.test.context.TestContext;
import org.springframework.test.context.support.AbstractTestExecutionListener;
import org.springframework.util.CollectionUtils;

/**
 * This hook lets run CatalogAlertServiceOperationsIntegrationTest without break catalog alerts
 * state, i.e., this hooks conserve the internal state after run all tests from
 * CatalogAlertServiceOperationsIntegrationTest.
 *
 * Backup any alert belonging to provider testApp_provider before runs any test. This backup allows
 * to restore catalog state after runs all tests.
 *
 */
public class BackupAlertsCollectionHook extends AbstractTestExecutionListener {

  static String PROVIDER_ID = "testApp_provider";

  protected PlatformClientOperations platformTemplate;

  private List<CatalogAlert> alertsBackup;

  public void beforeTestClass(final TestContext testContext) throws Exception {
    final CatalogAlertInputMessage message = new CatalogAlertInputMessage();
    // message.setIdentityToken(tokenApp);
    final CatalogAlertOutputMessage outputMessage = getPlatformTemplate(testContext).getCatalogOps().getAuthorizedAlerts(message);
    alertsBackup = outputMessage.getAlerts();
  }

  public void afterTestClass(final TestContext testContext) throws Exception {
    if (!CollectionUtils.isEmpty(alertsBackup)) {
      final CatalogAlertInputMessage message = new CatalogAlertInputMessage(PROVIDER_ID);
      message.setAlerts(alertsBackup);
      getPlatformTemplate(testContext).getCatalogOps().registerAlerts(message);
    }
  }

  private PlatformClientOperations getPlatformTemplate(final TestContext testContext) {
    if (platformTemplate == null) {
      platformTemplate = testContext.getApplicationContext().getBean(PlatformClientOperations.class);
    }

    return platformTemplate;
  }
}
