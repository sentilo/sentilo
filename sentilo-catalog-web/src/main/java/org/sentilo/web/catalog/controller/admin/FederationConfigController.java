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

import javax.servlet.http.HttpServletRequest;

import org.sentilo.web.catalog.controller.CrudController;
import org.sentilo.web.catalog.domain.FederationConfig;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.service.CrudService;
import org.sentilo.web.catalog.service.FederationConfigService;
import org.sentilo.web.catalog.utils.Constants;
import org.sentilo.web.catalog.utils.ExcelGeneratorUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestMapping;

@Controller
@RequestMapping("/admin/federation")
public class FederationConfigController extends CrudController<FederationConfig> {

  @Autowired
  private FederationConfigService federationConfigService;

  @ModelAttribute(Constants.MODEL_ACTIVE_MENU)
  public String getActiveMenu() {
    return Constants.MENU_FEDERATION;
  }

  @Override
  protected void initViewNames() {
    getViewNames().put(LIST_ACTION, Constants.VIEW_FEDERATION_LIST);
    getViewNames().put(DETAIL_ACTION, Constants.VIEW_FEDERATION_DETAIL);
    getViewNames().put(NEW_ACTION, Constants.VIEW_NEW_FEDERATION);
  }

  @Override
  protected List<String> toRow(final FederationConfig federation) {
    final List<String> row = new ArrayList<String>();
    row.add(federation.getId()); // checkbox
    row.add(federation.getId());
    row.add(federation.getName());
    row.add(federation.getAppClientName());
    row.add(getLocalDateFormat().printAsLocalTime(federation.getLastSyncTime(), Constants.DATETIME_FORMAT));
    row.add(getLocalDateFormat().printAsLocalTime(federation.getCreatedAt(), Constants.DATETIME_FORMAT));
    return row;
  }

  @Override
  protected List<String> toExcelRow(final FederationConfig federation) {
    return ExcelGeneratorUtils.getFederationConfigExcelRowsData(federation, getLocalDateFormat());
  }

  @Override
  protected CrudService<FederationConfig> getService() {
    return federationConfigService;
  }

  @Override
  protected FederationConfig buildNewEntity(final String id) {
    return new FederationConfig(id);
  }

  @Override
  protected String getEntityModelKey() {
    return Constants.MODEL_FEDERATION;
  }

  @Override
  protected void doBeforeCreateResource(final FederationConfig resource, final Model model) {
    super.doBeforeCreateResource(resource, model);
    // By default, a federation config is always created active
    resource.setActive(true);
  }

  @Override
  protected void doBeforeSearchPage(final HttpServletRequest request, final SearchFilter filter) {
    super.doBeforeSearchPage(request, filter);
    // Only active federationConfig services should be displayed in the view
    filter.addAndParam("active", Boolean.TRUE);
  }

  @Override
  protected void doBeforeUpdateResource(final FederationConfig resource, final Model model) {
    super.doBeforeUpdateResource(resource, model);
    // As active field isn't shown in edit view, its value is set again to true before update.
    resource.setActive(true);
  }
}
