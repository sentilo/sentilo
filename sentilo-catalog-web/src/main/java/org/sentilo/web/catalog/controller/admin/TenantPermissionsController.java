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
import org.sentilo.web.catalog.domain.Tenant;
import org.sentilo.web.catalog.domain.TenantPermission;
import org.sentilo.web.catalog.dto.DataTablesDTO;
import org.sentilo.web.catalog.dto.OptionDTO;
import org.sentilo.web.catalog.dto.TenantPermissionsDTO;
import org.sentilo.web.catalog.format.datetime.LocalDateFormatter;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.search.builder.SearchFilterUtils;
import org.sentilo.web.catalog.service.CrudService;
import org.sentilo.web.catalog.service.ProviderService;
import org.sentilo.web.catalog.service.TenantPermissionService;
import org.sentilo.web.catalog.service.TenantService;
import org.sentilo.web.catalog.utils.CatalogUtils;
import org.sentilo.web.catalog.utils.Constants;
import org.sentilo.web.catalog.utils.ExcelGeneratorUtils;
import org.sentilo.web.catalog.utils.ModelUtils;
import org.sentilo.web.catalog.utils.TenantUtils;
import org.sentilo.web.catalog.utils.enums.EntityType;
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
@RequestMapping("/admin/grants")
public class TenantPermissionsController extends SearchController<TenantPermission> {

  private static final String PERMISSION = "permission.";

  private static final String PERMISSION_TO = "to";

  private static final String PERMISSION_FROM = "from";

  @Autowired
  private TenantPermissionService tenantPermissionService;

  @Autowired
  private ProviderService providerService;

  @Autowired
  private TenantService tenantService;

  @Autowired
  private LocalDateFormatter localDateFormat;

  @Autowired
  private MessageSource messageSource;

  @ModelAttribute(Constants.MODEL_ACTIVE_MENU)
  public String getActiveMenu() {
    return Constants.MENU_TENANT;
  }

  @RequestMapping(value = "/{type}/{id}", produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  public DataTablesDTO getTenantPermissions(final HttpServletRequest request, final Model model, final Pageable pageable,
      @PathVariable final String type, @PathVariable final String id, @RequestParam final Integer sEcho, @RequestParam final String tableName,
      @RequestParam(required = false) final String search) {

    return getPageList(model, request, pageable, sEcho, tableName, search);
  }

  @Override
  protected void doBeforeSearchPage(final HttpServletRequest request, final SearchFilter filter) {
    String uri = "/admin/grants/{type}/{id}";
    if (request.getRequestURI().contains("excel")) {
      uri += "/excel";
    }

    final String id = SearchFilterUtils.getUriVariableValue(request, uri, "id");
    final String type = SearchFilterUtils.getUriVariableValue(request, uri, "type");

    if (StringUtils.hasText(id) && StringUtils.hasText(type)) {
      if (PERMISSION_TO.equalsIgnoreCase(type)) {
        filter.addAndParam("source", id);
      } else if (PERMISSION_FROM.equalsIgnoreCase(type)) {
        filter.addAndParam("target", id);
      }
    }
  }

  @RequestMapping(value = "/{id}/add", method = RequestMethod.GET)
  public String showAddTenantPermissionsForm(@PathVariable final String id, final Model model) {
    addPermissionTypesToModel(model);
    final TenantPermissionsDTO form = createForm(id, model);
    model.addAttribute(Constants.MODEL_TENANT_PERMISSIONS, form);
    return Constants.VIEW_ADD_TENANT_PERMISSIONS;
  }

  @RequestMapping(value = "/{id}/add", method = RequestMethod.POST)
  public String addTenantPermissions(@Valid final TenantPermissionsDTO form, final BindingResult result, @PathVariable final String id,
      final Model model) {
    if (result.hasErrors()) {
      ModelUtils.setCreateMode(model);
      model.addAttribute(Constants.MODEL_TENANT_PERMISSIONS, createForm(id, model));
      ModelUtils.addErrorMessageTo(model, result.getGlobalError().getCode());
      return Constants.VIEW_ADD_TENANT_PERMISSIONS;
    }

    createPermissions(id, form.getSelectedProvidersIds(), form.getSelectedEntitiesIds(), form.getPermissionType(), EntityType.PROVIDER,
        form.getVisible(), form.getListVisible());

    return viewTenantDetailPermissionsTab(id, model, "tenant.permissions.added", false, Constants.TAB_3);
  }

  @RequestMapping(value = "/{id}/edit", method = RequestMethod.GET)
  public String showEditTenantPermissionsForm(@PathVariable final String id, @Valid final TenantPermissionsDTO permissions, final Model model) {
    if (permissions.getSelectedIds() != null && permissions.getSelectedIds().length > 0) {
      final String tenantPermissionId = permissions.getSelectedIds()[0];
      final TenantPermission tenantPermission = new TenantPermission(tenantPermissionId);
      model.addAttribute(Constants.MODEL_TENANT_PERMISSION, tenantPermissionService.findAndThrowErrorIfNotExist(tenantPermission));
      model.addAttribute("tenantId", id);
      return Constants.VIEW_EDIT_TENANT_PERMISSIONS;
    } else {
      final String messageCode = "tenant.permission.edit.error.selectOne";
      return viewTenantDetailPermissionsTab(id, model, messageCode, true, Constants.TAB_4);
    }
  }

  @RequestMapping(value = "/{id}/edit", method = RequestMethod.POST)
  public String editTenantPermissions(@Valid final TenantPermission tenantPermission, final BindingResult result, @PathVariable final String id,
      final Model model) {
    tenantPermissionService.update(tenantPermission);
    return viewTenantDetailPermissionsTab(id, model, "tenant.permissions.saved", false, Constants.TAB_4);
  }

  @RequestMapping(value = "/{id}/remove", method = RequestMethod.POST)
  public String removeTenantPermissions(@Valid final TenantPermissionsDTO permissions, final BindingResult result, @PathVariable final String id,
      final Model model) {
    deletePermissions(permissions.getSelectedPermissions());
    return viewTenantDetailPermissionsTab(id, model, "tenant.permissions.deleted", false, Constants.TAB_3);
  }

  @RequestMapping(value = "/{id}/changeMapVisibility", method = RequestMethod.POST)
  public String changeMapVisibility(@PathVariable final String id, @RequestParam final String newMapVisibility,
      @RequestParam final String[] selectedIds, final Model model) {
    final boolean isMapVisible = StringUtils.hasText(newMapVisibility) && "public".equals(newMapVisibility) ? true : false;
    tenantPermissionService.changeMapVisibility(selectedIds, isMapVisible);
    return viewTenantDetailPermissionsTab(id, model, "mapVisibility.changed", false, Constants.TAB_4);
  }

  @RequestMapping(value = "/{id}/changeListVisibility", method = RequestMethod.POST)
  public String changeListVisibility(@PathVariable final String id, @RequestParam final String newListVisibility,
      @RequestParam final String[] selectedIds, final Model model) {
    final boolean isListVisible = StringUtils.hasText(newListVisibility) && "public".equals(newListVisibility) ? true : false;
    tenantPermissionService.changeListVisibility(selectedIds, isListVisible);
    return viewTenantDetailPermissionsTab(id, model, "listVisibility.changed", false, Constants.TAB_4);
  }

  @RequestMapping("/{id}/list/excel")
  public ModelAndView getTenantPermissionsExcel(final HttpServletRequest request, final Model model, final HttpServletResponse response,
      @PathVariable final String id, @RequestParam final String tableName, @RequestParam(required = false) final String search) throws IOException {
    return getExcel(model, request, response, tableName);
  }

  @Override
  protected CrudService<TenantPermission> getService() {
    return tenantPermissionService;
  }

  @Override
  protected Class<TenantPermission> getRowClass() {
    return TenantPermission.class;
  }

  @Override
  protected List<String> toRow(final TenantPermission permission) {
    final List<String> row = new ArrayList<String>();
    row.add(permission.getId());

    // Assume that TenantContext exists
    final boolean isToPermission = permission.getSource().equals(TenantUtils.getCurrentTenant());

    if (isToPermission) {
      row.add(permission.getTarget());
    } else {
      row.add(permission.getSource());
    }

    row.add(permission.getEntity());
    row.add(messageSource.getMessage(PERMISSION + permission.getType(), null, LocaleContextHolder.getLocale()));
    row.add(localDateFormat.printAsLocalTime(permission.getCreatedAt(), Constants.DATETIME_FORMAT));

    if (isToPermission) {
      row.add(permission.getCreatedBy());
    } else {
      row.add(messageSource.getMessage(String.valueOf(permission.getVisible()), null, LocaleContextHolder.getLocale()));
      row.add(messageSource.getMessage(String.valueOf(permission.getListVisible()), null, LocaleContextHolder.getLocale()));
    }

    return row;
  }

  @Override
  protected List<String> toExcelRow(final TenantPermission permission) {
    return ExcelGeneratorUtils.getTenantPermissionsExcelRowsData(permission, messageSource, getLocalDateFormat());
  }

  @Override
  protected void doBeforeExcelBuilder(final Model model) {
    super.doBeforeExcelBuilder(model);
    model.addAttribute(Constants.MESSAGE_KEYS_PREFIX, "tenant.permissions");
  }

  @Override
  protected void initViewNames() {
    // Do nothing.
  }

  protected void deletePermissions(final List<TenantPermission> tenantPermissions) {
    for (final TenantPermission tenantPermissionAux : tenantPermissions) {
      final TenantPermission tenantPermission = tenantPermissionService.find(tenantPermissionAux);
      if (tenantPermission != null) {
        tenantPermissionService.delete(tenantPermission);
      }
    }
  }

  private void createPermissions(final String tenantSource, final String[] tenantSourceEntitiesIds, final String[] tenantTargetsIds,
      final TenantPermission.Type type, final EntityType entityType, final boolean visible, final boolean listVisible) {

    for (final String entityId : tenantSourceEntitiesIds) {
      for (final String targetId : tenantTargetsIds) {
        // Create the permission
        tenantPermissionService.create(new TenantPermission(tenantSource, entityId, targetId, type, entityType, visible, listVisible));
      }
    }
  }

  private String viewTenantDetailPermissionsTab(final String id, final Model model, final String messageCode, final boolean isError, final int tab) {
    if (isError) {
      ModelUtils.addErrorMessageTo(model, messageCode);
    } else {
      ModelUtils.addConfirmationMessageTo(model, messageCode);
    }
    ModelUtils.addOpenedTabTo(model, tab);
    addTenantToModel(id, model);
    return Constants.VIEW_TENANT_DETAIL;
  }

  private void addTenantToModel(final String tenantId, final Model model) {
    final Tenant tenant = tenantService.findAndThrowErrorIfNotExist(new Tenant(tenantId));
    model.addAttribute(Constants.MODEL_TENANT, tenant);
  }

  private TenantPermissionsDTO createForm(final String id, final Model model) {
    model.addAttribute(Constants.MODEL_PERMISSION_TYPES, CatalogUtils.toOptionList(TenantPermission.Type.class, "permission", messageSource));
    model.addAttribute(Constants.MODEL_TENANT, tenantService.findAndThrowErrorIfNotExist(new Tenant(id)));

    final List<OptionDTO> providers = CatalogUtils.toOptionList(providerService.findAll());
    final List<OptionDTO> tenants = CatalogUtils.toOptionList(tenantService.findPublicsButNotMe(id));

    return new TenantPermissionsDTO(id, providers, tenants);
  }

  private void addPermissionTypesToModel(final Model model) {
    model.addAttribute(Constants.MODEL_PERMISSION_TYPES, CatalogUtils.toOptionList(TenantPermission.Type.class, "permission", messageSource));
  }
}
