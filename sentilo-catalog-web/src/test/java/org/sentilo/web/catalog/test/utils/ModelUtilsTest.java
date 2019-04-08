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
package org.sentilo.web.catalog.test.utils;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.web.catalog.domain.TenantPermission;
import org.sentilo.web.catalog.dto.TenantCustomParamsDTO;
import org.sentilo.web.catalog.utils.Constants;
import org.sentilo.web.catalog.utils.ModelUtils;
import org.springframework.ui.Model;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

public class ModelUtilsTest {

  @Mock
  private Model model;

  @Mock
  private RedirectAttributes modelRedirect;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void addConfirmationMessageTo() {
    ModelUtils.addConfirmationMessageTo(model, "mockMessage");
    Assert.assertNotNull(model.containsAttribute(Constants.MODEL_CONFIRMATION_MESSAGE_KEY));
  }

  @Test
  public void addConfirmationMessageToRedirect() {
    ModelUtils.addConfirmationMessageTo(modelRedirect, "mockMessage");
    Assert.assertNotNull(modelRedirect.containsAttribute(Constants.MODEL_CONFIRMATION_MESSAGE_KEY));
  }

  @Test
  public void setCreateMode() {
    ModelUtils.setCreateMode(model);
    Assert.assertNotNull(model.containsAttribute(Constants.MODEL_MODE));
  }

  @Test
  public void setEditMode() {
    ModelUtils.setEditMode(model);
    Assert.assertNotNull(model.containsAttribute(Constants.MODEL_MODE));
  }

  @Test
  public void setDetailMode() {
    ModelUtils.setDetailMode(model);
    Assert.assertNotNull(model.containsAttribute(Constants.MODEL_MODE));
  }

  @Test
  public void setDataMode() {
    ModelUtils.setDataMode(model);
    Assert.assertNotNull(model.containsAttribute(Constants.MODEL_MODE));
  }

  @Test
  public void addErrorMessageTo() {
    ModelUtils.addErrorMessageTo(model, "mockErrorMessage");
    Assert.assertNotNull(model.containsAttribute(Constants.MODEL_ERROR_MESSAGE));
  }

  @Test
  public void addOpenedTabTo() {
    ModelUtils.addOpenedTabTo(model, 1);
    Assert.assertNotNull(model.containsAttribute(Constants.MODEL_OPENED_TAB));
  }

  @Test
  public void addActiveMenuTo() {
    ModelUtils.addActiveMenuTo(model, "mockMenu");
    Assert.assertNotNull(model.containsAttribute(Constants.MODEL_ACTIVE_MENU));
  }

  @Test
  public void addCatalogAccessPermissions() {
    ModelUtils.addCatalogAccessPermissions(model, TenantPermission.Type.READ);
    Assert.assertNotNull(model.containsAttribute(Constants.MODEL_TENANT_PERMISSION));
  }

  @Test
  public void addTenantCustomParams() {
    ModelUtils.addTenantCustomParams(model, new TenantCustomParamsDTO());
    Assert.assertNotNull(model.containsAttribute(Constants.MODEL_TENANT_CUSTOM_PARAMS));
  }
}
