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
import org.sentilo.web.catalog.domain.User;
import org.sentilo.web.catalog.security.CatalogUserDetails;
import org.sentilo.web.catalog.service.CrudService;
import org.sentilo.web.catalog.service.TenantService;
import org.sentilo.web.catalog.service.UserService;
import org.sentilo.web.catalog.utils.CatalogUtils;
import org.sentilo.web.catalog.utils.Constants;
import org.sentilo.web.catalog.utils.ExcelGeneratorUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestMapping;

@Controller
@RequestMapping("/admin/users")
public class UserController extends CrudController<User> {

  @Autowired
  private UserService userService;

  @Autowired
  private TenantService tenantService;

  @ModelAttribute(Constants.MODEL_ACTIVE_MENU)
  public String getActiveMenu() {
    return Constants.MENU_USER;
  }

  @Override
  protected CrudService<User> getService() {
    return userService;
  }

  @Override
  protected void initViewNames() {
    getViewNames().put(LIST_ACTION, Constants.VIEW_USER_LIST);
    getViewNames().put(DETAIL_ACTION, Constants.VIEW_USER_DETAIL);
    getViewNames().put(NEW_ACTION, Constants.VIEW_NEW_USER);
  }

  @Override
  protected List<String> toRow(final User user) {
    final List<String> row = new ArrayList<String>();
    row.add(user.getUserName()); // checkbox
    row.add(user.getUserName());
    row.add(user.getName());
    row.add(user.getEmail());
    row.add(getLocalDateFormat().printAsLocalTime(user.getCreatedAt(), Constants.DATETIME_FORMAT));
    return row;
  }

  @Override
  protected List<String> toExcelRow(final User user) {
    return ExcelGeneratorUtils.getUserExcelRowsData(user, getLocalDateFormat());
  }

  @Override
  protected User buildNewEntity(final String id) {
    return new User(id);
  }

  @Override
  protected String getEntityModelKey() {
    return Constants.MODEL_USER;
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.controller.CrudController#doBeforeNewResource(javax.servlet.http.
   * HttpServletRequest, org.springframework.ui.Model)
   */
  @Override
  protected void doBeforeNewResource(final HttpServletRequest request, final Model model) {
    super.doBeforeNewResource(request, model);

    getTenantsList(model);
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.web.catalog.controller.CrudController#doBeforeEditResource(org.springframework.
   * ui.Model)
   */
  @Override
  protected void doBeforeEditResource(final String id, final Model model) {
    super.doBeforeEditResource(id, model);

    getTenantsList(model);
  }

  private void getTenantsList(final Model model) {
    final CatalogUserDetails userDetails = userDetailsService.getCatalogUserDetails();

    if (userDetails.isSuperAdminUser()) {
      // If it's a super admin, show all the public tenants
      model.addAttribute(Constants.MODEL_TENANTS, CatalogUtils.toOptionList(tenantService.findPublicTenants()));
    }
  }

}
