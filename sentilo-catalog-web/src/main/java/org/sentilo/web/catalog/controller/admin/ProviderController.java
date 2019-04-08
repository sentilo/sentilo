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
package org.sentilo.web.catalog.controller.admin;

import java.util.ArrayList;
import java.util.List;

import org.sentilo.web.catalog.controller.CrudController;
import org.sentilo.web.catalog.domain.Provider;
import org.sentilo.web.catalog.security.CatalogUserDetails;
import org.sentilo.web.catalog.service.CrudService;
import org.sentilo.web.catalog.service.ProviderService;
import org.sentilo.web.catalog.utils.Constants;
import org.sentilo.web.catalog.utils.ExcelGeneratorUtils;
import org.sentilo.web.catalog.utils.TenantUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestMapping;

@Controller
@RequestMapping("/admin/provider")
public class ProviderController extends CrudController<Provider> {

  @Autowired
  private ProviderService providerService;

  @ModelAttribute(Constants.MODEL_ACTIVE_MENU)
  public String getActiveMenu() {
    return Constants.MENU_PROVIDER;
  }

  @Override
  protected List<String> toRow(final Provider provider) {
    final List<String> row = new ArrayList<String>();
    row.add(provider.getId()); // checkbox
    row.add(provider.getId());
    row.add(provider.getName());
    row.add(provider.getDescription());
    row.add(getLocalDateFormat().printAsLocalTime(provider.getCreatedAt(), Constants.DATETIME_FORMAT));
    return row;
  }

  @Override
  protected List<String> toExcelRow(final Provider provider) {
    return ExcelGeneratorUtils.getProviderExcelRowsData(provider, getLocalDateFormat(), userDetailsService.getCatalogUserDetails());
  }

  @Override
  protected void initViewNames() {
    getViewNames().put(LIST_ACTION, Constants.VIEW_PROVIDER_LIST);
    getViewNames().put(DETAIL_ACTION, Constants.VIEW_PROVIDER_DETAIL);
    getViewNames().put(NEW_ACTION, Constants.VIEW_NEW_PROVIDER);
  }

  @Override
  protected CrudService<Provider> getService() {
    return providerService;
  }

  @Override
  protected Provider buildNewEntity(final String id) {
    return new Provider(id);
  }

  @Override
  protected String getEntityModelKey() {
    return Constants.MODEL_PROVIDER;
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.web.catalog.controller.CrudController#doAfterViewResource(org.springframework.ui
   * .Model)
   */
  @Override
  protected void doAfterViewResource(final Model model) {
    // If provider belongs to a third organization then its token must be hidden to user.
    final Provider resource = (Provider) model.asMap().get(getEntityModelKey());
    final CatalogUserDetails userDetails = userDetailsService.getCatalogUserDetails();
    if (!TenantUtils.isCurrentTenantResource(resource) || userDetails.isUser()) {
      resource.setToken(Constants.HIDDEN_TOKEN_STR);
    }
  }
}
