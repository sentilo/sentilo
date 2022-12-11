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
import java.util.Map;

import javax.servlet.http.HttpServletRequest;

import org.sentilo.web.catalog.context.TenantContextHolder;
import org.sentilo.web.catalog.controller.CrudController;
import org.sentilo.web.catalog.domain.Tenant;
import org.sentilo.web.catalog.search.builder.SearchFilterUtils;
import org.sentilo.web.catalog.service.CrudService;
import org.sentilo.web.catalog.service.TenantService;
import org.sentilo.web.catalog.utils.Constants;
import org.sentilo.web.catalog.utils.ExcelGeneratorUtils;
import org.sentilo.web.catalog.utils.TenantUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

@Controller
@RequestMapping("/admin/tenant")
public class TenantController extends CrudController<Tenant> {

  @Autowired
  private TenantService tenantService;

  @ModelAttribute(Constants.MODEL_ACTIVE_MENU)
  public String getActiveMenu() {
    return Constants.MENU_TENANT;
  }

  @Override
  protected void initViewNames() {
    getViewNames().put(LIST_ACTION, Constants.VIEW_TENANT_LIST);
    getViewNames().put(DETAIL_ACTION, Constants.VIEW_TENANT_DETAIL);
    getViewNames().put(NEW_ACTION, Constants.VIEW_NEW_TENANT);
  }

  @Override
  protected List<String> toRow(final Tenant tenant) {
    final List<String> row = new ArrayList<String>();
    row.add(tenant.getId()); // checkbox
    row.add(tenant.getId());
    row.add(tenant.getName());
    row.add(tenant.getDescription());
    row.add(getLocalDateFormat().printAsLocalTime(tenant.getCreatedAt(), Constants.DATETIME_FORMAT));
    return row;
  }

  @Override
  protected List<String> toExcelRow(final Tenant tenant) {
    return ExcelGeneratorUtils.getTenantExcelRowsData(tenant, getLocalDateFormat());
  }

  @Override
  protected void addRowMetadata(final Tenant resource, final Map<String, String> rowMetadata) {
    super.addRowMetadata(resource, rowMetadata);
    // If rowMetadata hideCheckbox field has not been added to row yet and tenant is default, then
    // it field is added
    if (Boolean.TRUE.equals(resource.getIsDefault()) && !rowMetadata.containsKey("hideCheckbox")) {
      rowMetadata.put("hideCheckbox", Boolean.TRUE.toString());
    }
  }

  @Override
  protected CrudService<Tenant> getService() {
    return tenantService;
  }

  @Override
  protected Tenant buildNewEntity(final String id) {
    return new Tenant(id);
  }

  @Override
  protected String getEntityModelKey() {
    return Constants.MODEL_TENANT;
  }

  @Override
  protected String redirect(final Model model, final HttpServletRequest request, final RedirectAttributes attributes) {
    // If current user isn't a SuperAdmin user then process must be redirected to its tenant detail
    // page
    // instead of the tenant list page. Only SuperAdmin users are
    // allowed to view the tenant list
    if (userDetailsService.getCatalogUserDetails().isSuperAdminUser()) {
      return super.redirectToList(model, request, attributes);
    } else {
      final String tenantPrefix = TenantContextHolder.hasContext() ? "/" + TenantUtils.getCurrentTenant() : "";
      final String id = SearchFilterUtils.getUriVariableValue(request, "/admin/tenant/{id}/edit", "id");
      return "redirect:" + tenantPrefix + "/admin/tenant/" + id + "/detail";
    }
  }
}
