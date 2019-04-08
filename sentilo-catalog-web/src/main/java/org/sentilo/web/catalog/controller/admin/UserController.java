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

import org.sentilo.web.catalog.context.TenantContextHolder;
import org.sentilo.web.catalog.controller.CrudController;
import org.sentilo.web.catalog.domain.User;
import org.sentilo.web.catalog.dto.ChangeUserPasswordDTO;
import org.sentilo.web.catalog.dto.ChangeUserPasswordResponseDTO;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.security.CatalogUserDetails;
import org.sentilo.web.catalog.security.Role;
import org.sentilo.web.catalog.service.CrudService;
import org.sentilo.web.catalog.service.TenantService;
import org.sentilo.web.catalog.service.UserService;
import org.sentilo.web.catalog.utils.CatalogUtils;
import org.sentilo.web.catalog.utils.Constants;
import org.sentilo.web.catalog.utils.ExcelGeneratorUtils;
import org.sentilo.web.catalog.utils.TenantUtils;
import org.sentilo.web.catalog.validator.UserPasswordValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.util.StringUtils;
import org.springframework.validation.BindingResult;
import org.springframework.validation.ObjectError;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

@Controller
@RequestMapping("/admin/users")
public class UserController extends CrudController<User> {

  @Autowired
  private UserService userService;

  @Autowired
  private TenantService tenantService;

  @Autowired
  private UserPasswordValidator passwordValidator;

  @Autowired
  private PasswordEncoder passwordEncoder;

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

  @RequestMapping(value = "/{id}/changePassword", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
  public @ResponseBody ChangeUserPasswordResponseDTO changePassword(@RequestBody final ChangeUserPasswordDTO resource, final BindingResult result,
      @PathVariable final String id, final Model model, final HttpServletRequest request) {

    passwordValidator.validate(resource, result);

    final String returnCode = result.hasErrors() ? "KO" : "OK";
    final ChangeUserPasswordResponseDTO response = new ChangeUserPasswordResponseDTO(returnCode);

    for (final ObjectError error : result.getAllErrors()) {
      response.addErrorMessage(error);
    }

    if (!result.hasErrors()) {
      final User currentUser = userService.find(new User(resource.getUserName()));
      currentUser.setPassword(passwordEncoder.encode(resource.getNewPassword()));
      userService.update(currentUser);
    }

    return response;
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
    addChangePasswordDTOToModel(model);
  }

  @Override
  protected void doBeforeUpdateResource(final User resource, final Model model) {
    super.doBeforeUpdateResource(resource, model);
    // As view doesn't display/contain password, to keep current password it should be copied from
    // existing database data before save user
    final User user = getService().find(resource);
    resource.setPassword(user.getPassword());
  }

  @Override
  protected void doBeforeCreateResource(final User resource, final Model model) {
    super.doBeforeCreateResource(resource, model);
    // Set default password. By convention it is equals userName
    resource.setPassword(passwordEncoder.encode(resource.getUserName()));
  }

  @Override
  protected void doBeforeSearchPage(final HttpServletRequest request, final SearchFilter filter) {
    super.doBeforeSearchPage(request, filter);

    // Filter users with role PLATFORM from the search
    filter.addNorParam("roles", Role.PLATFORM);
  }

  @Override
  protected String redirect(final Model model, final HttpServletRequest request, final RedirectAttributes attributes) {
    if (!userDetailsService.getCatalogUserDetails().isAdminUser() && !userDetailsService.getCatalogUserDetails().isSuperAdminUser()) {
      return redirectToDetail(model, request, attributes);
    }
    return super.redirect(model, request, attributes);
  }

  private void addChangePasswordDTOToModel(final Model model) {
    model.addAttribute(Constants.MODEL_USER_PASSWORD, new ChangeUserPasswordDTO());
  }

  private void getTenantsList(final Model model) {
    final CatalogUserDetails userDetails = userDetailsService.getCatalogUserDetails();

    if (userDetails.isSuperAdminUser()) {
      // If logged in user is a super admin user then a list with public tenants is added to the
      // model
      // view.
      model.addAttribute(Constants.MODEL_TENANTS, CatalogUtils.toOptionList(tenantService.findPublicTenants()));
    }
  }

  private String redirectToDetail(final Model model, final HttpServletRequest request, final RedirectAttributes redirectAttributes) {
    // URL must redirect current user to its detail view
    final String username = userDetailsService.getCatalogUserDetails().getUsername();

    final StringBuilder sb = new StringBuilder("/");
    if (TenantContextHolder.hasContext() && StringUtils.hasText(TenantUtils.getCurrentTenant())) {
      sb.append(TenantUtils.getCurrentTenant());
      sb.append("/");
    }

    sb.append("admin/users/" + username + "/detail");
    final String redirectUrl = redirectStrategy.buildRedirectUrl(request, sb.toString());

    return "redirect:" + redirectUrl;
  }
}
