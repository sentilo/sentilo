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
package org.sentilo.web.catalog.test.admin.scheduler;

import static org.mockito.Mockito.verify;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.web.catalog.admin.scheduler.FederatedSynchronizationJob;
import org.sentilo.web.catalog.admin.service.FederatedSynchronizationService;
import org.sentilo.web.catalog.utils.Constants;
import org.springframework.security.core.context.SecurityContextHolder;

public class FederatedSynchronizationJobTest {

  @InjectMocks
  private FederatedSynchronizationJob job;

  @Mock
  private FederatedSynchronizationService service;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);

    System.setProperty(SentiloConstants.SENTILO_FEDERATION_ENABLED_PROP_KEY, "true");
  }

  @Test
  public void syncFederatedServices() {
    job.syncFederatedServices();

    Assert.assertNotNull(SecurityContextHolder.getContext().getAuthentication());
    Assert.assertEquals(Constants.BATCH_USER, SecurityContextHolder.getContext().getAuthentication().getName());
    verify(service).syncCatalogs();
  }

  @After
  public void tearDown() throws Exception {
    SecurityContextHolder.clearContext();
    System.clearProperty(SentiloConstants.SENTILO_FEDERATION_ENABLED_PROP_KEY);
  }
}
