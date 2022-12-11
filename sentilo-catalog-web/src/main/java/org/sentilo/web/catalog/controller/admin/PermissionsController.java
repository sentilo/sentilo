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

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.validation.Valid;

import org.sentilo.web.catalog.controller.SearchController;
import org.sentilo.web.catalog.domain.Application;
import org.sentilo.web.catalog.domain.Permission;
import org.sentilo.web.catalog.dto.DataTablesDTO;
import org.sentilo.web.catalog.dto.OptionDTO;
import org.sentilo.web.catalog.dto.PermissionsDTO;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.search.builder.SearchFilterUtils;
import org.sentilo.web.catalog.service.ApplicationService;
import org.sentilo.web.catalog.service.CrudService;
import org.sentilo.web.catalog.service.PermissionService;
import org.sentilo.web.catalog.service.ProviderService;
import org.sentilo.web.catalog.utils.CatalogUtils;
import org.sentilo.web.catalog.utils.Constants;
import org.sentilo.web.catalog.utils.ExcelGeneratorUtils;
import org.sentilo.web.catalog.utils.ModelUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.MessageSource;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.data.domain.Pageable;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.util.StringUtils;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.servlet.ModelAndView;

@Controller
@RequestMapping("/admin/permissions")
public class PermissionsController extends SearchController<Permission> {

  private static final String PERMISSION = "permission.";

  @Autowired
  private ApplicationService applicationService;

  @Autowired
  private PermissionService permissionService;

  @Autowired
  private ProviderService providerService;

  @Autowired
  private MessageSource messageSource;

  @ModelAttribute(Constants.MODEL_ACTIVE_MENU)
  public String getActiveMenu() {
    return Constants.MENU_APPLICATION;
  }

  @RequestMapping(value = "/application/{id}/add", method = RequestMethod.GET)
  public String showAddApplicationPermissionsForm(@PathVariable final String id, final Model model) {
    final PermissionsDTO form = createForm(id, model);
    model.addAttribute(Constants.MODEL_PERMISSIONS, form);
    return Constants.VIEW_ADD_APPLICATION_PERMISSIONS;
  }

  @RequestMapping(value = "/application/{id}/add", method = RequestMethod.POST)
  public String addApplicationPermissions(@Valid final PermissionsDTO permissions, final BindingResult result, @PathVariable final String id,
      final Model model) {

    if (result.hasErrors()) {
      ModelUtils.addErrorMessageTo(model, result.getGlobalError().getCode());
      ModelUtils.addOpenedTabTo(model, Constants.TAB_2);
      model.addAttribute(Constants.MODEL_PERMISSIONS, createForm(id, model));
      return Constants.VIEW_ADD_APPLICATION_PERMISSIONS;
    }

    createPermissions(id, permissions.getSelectedProvidersIds(), permissions.getPermissionType());
    createPermissions(id, permissions.getSelectedApplicationsIds(), permissions.getPermissionType());

    return viewApplicationDetail(id, model, "permission.added");
  }

  @RequestMapping(value = "/application/{id}/remove", method = RequestMethod.POST)
  public String removeApplicationPermissions(final PermissionsDTO permissions, final BindingResult result, @PathVariable final String id,
      final Model model) {
    permissionService.delete(permissions.getSelectedPermissions());
    return viewApplicationDetail(id, model, "permission.deleted");
  }

  @RequestMapping(value = "/application/{id}", produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  public DataTablesDTO getApplicationPermissions(final HttpServletRequest request, final Model model, final Pageable pageable,
      @PathVariable final String id, @RequestParam final Integer sEcho, @RequestParam final String tableName,
      @RequestParam(required = false) final String search) {
    return getPageList(model, request, pageable, sEcho, tableName, search);
  }

  @RequestMapping("/application/{id}/excel")
  public ModelAndView getApplicationPermissionsExcel(final HttpServletRequest request, final Model model, final HttpServletResponse response,
      @PathVariable final String id, @RequestParam final String tableName, @RequestParam(required = false) final String search) throws IOException {
    return getExcel(model, request, response, tableName);
  }

  @Override
  protected void doBeforeSearchPage(final HttpServletRequest request, final SearchFilter filter) {
    String uri = "/admin/permissions/application/{id}";
    if (request.getRequestURI().contains("excel")) {
      uri += "/excel";
    }

    final String id = SearchFilterUtils.getUriVariableValue(request, uri, "id");
    if (StringUtils.hasText(id)) {
      // To find all permissions associate to an entity with id identity we must get all permissions
      // with source equal to identity
      filter.addAndParam("source", id);
    }
  }

  @Override
  protected List<String> toRow(final Permission permission) {
    final List<String> row = new ArrayList<String>();
    row.add(permission.getId());
    row.add(permission.getTarget());
    row.add(messageSource.getMessage(PERMISSION + permission.getType().toString(), null, LocaleContextHolder.getLocale()));
    return row;
  }

  @Override
  protected List<String> toExcelRow(final Permission permission) {
    return ExcelGeneratorUtils.getPermissionsExcelRowsData(permission, messageSource, getLocalDateFormat());
  }

  @Override
  protected CrudService<Permission> getService() {
    return permissionService;
  }

  @Override
  protected Class<Permission> getRowClass() {
    return Permission.class;
  }

  @Override
  protected void initViewNames() {
    // Do nothing.
  }

  private void createPermissions(final String sourceId, final String[] selectedIds, final Permission.Type type) {
    for (final String targetId : selectedIds) {
      final Permission permission = new Permission(sourceId, targetId, type);
      permissionService.create(permission);
    }
  }

  private String viewApplicationDetail(final String id, final Model model, final String messageCode) {
    ModelUtils.addConfirmationMessageTo(model, messageCode);
    ModelUtils.addOpenedTabTo(model, Constants.TAB_2);
    addApplicationToModel(id, model);
    return Constants.VIEW_APPLICATION_DETAIL;
  }

  private void addApplicationToModel(final String id, final Model model) {
    final Application application = applicationService.findAndThrowErrorIfNotExist(new Application(id));
    model.addAttribute(Constants.MODEL_APPLICATION, application);
  }

  private void addPermissionTypesToModel(final Model model) {
    model.addAttribute(Constants.MODEL_PERMISSION_TYPES, CatalogUtils.toOptionList(Permission.Type.class, "permission", messageSource));
  }

  private PermissionsDTO createForm(final String id, final Model model) {
    addPermissionTypesToModel(model);
    final List<OptionDTO> providers = CatalogUtils.toOptionList(providerService.findAllowed());
    final List<OptionDTO> applications = CatalogUtils.toOptionList(applicationService.findAllowed());
    return new PermissionsDTO(id, providers, applications);
  }
}
