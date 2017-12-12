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
package org.sentilo.web.catalog.test.security.audit;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.powermock.api.mockito.PowerMockito.mock;
import static org.powermock.api.mockito.PowerMockito.mockStatic;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import org.sentilo.web.catalog.domain.CatalogDocument;
import org.sentilo.web.catalog.security.CatalogUserDetails;
import org.sentilo.web.catalog.security.audit.AuditHandler;
import org.sentilo.web.catalog.security.service.CatalogUserDetailsService;
import org.sentilo.web.catalog.utils.Constants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@RunWith(PowerMockRunner.class)
@PrepareForTest({AuditHandler.class, LoggerFactory.class, Logger.class})
public class AuditHandlerTest {

  private static final String RESOURCE_ID = "1";
  private static final String USER_NAME = "userName";

  @InjectMocks
  private AuditHandler handler;

  @Mock
  private CatalogUserDetailsService userDetailsService;

  @Mock
  private CatalogDocument catalogDocument;

  @Mock
  private CatalogUserDetails catalogUser;

  private static Logger logger;

  @BeforeClass
  public static void setUpStatic() throws Exception {
    mockStatic(LoggerFactory.class);
    logger = mock(Logger.class);
    when(LoggerFactory.getLogger(Constants.AUDIT_LOGGER_NAME)).thenReturn(logger);
  }

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    when(userDetailsService.getCatalogUserDetails()).thenReturn(catalogUser);
    when(catalogDocument.getId()).thenReturn(RESOURCE_ID);
    when(catalogUser.getUsername()).thenReturn(USER_NAME);
  }

  @Test
  public void logCatalogDocumentAction() {
    handler.logCreate(catalogDocument);
    handler.logUpdate(catalogDocument);
    handler.logRemove(catalogDocument);

    verify(userDetailsService, times(3)).getCatalogUserDetails();
    verify(catalogUser, times(3)).getUsername();
    verify(logger, times(3)).info(any(String.class), new Object[] {any(String.class), eq(RESOURCE_ID), eq(USER_NAME)});
  }

  @Test
  public void logUserLogin() {
    handler.logUserLogin(USER_NAME);
    handler.logUserLogout(USER_NAME);

    verify(logger, times(2)).info(any(String.class), any(String.class));
  }
}
