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
package org.sentilo.web.catalog.controller;

import javax.servlet.http.HttpServletRequest;

import org.sentilo.common.config.SentiloModuleConfigService;
import org.sentilo.web.catalog.context.TenantContextHolder;
import org.sentilo.web.catalog.dto.TenantCustomParamsDTO;
import org.sentilo.web.catalog.security.SentiloRedirectStrategy;
import org.sentilo.web.catalog.service.TenantCustomParamsService;
import org.sentilo.web.catalog.utils.Constants;
import org.sentilo.web.catalog.utils.TenantUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

@Controller
public abstract class CatalogBaseController {

  protected static final Logger LOGGER = LoggerFactory.getLogger(CatalogBaseController.class);

  protected SentiloRedirectStrategy redirectStrategy = new SentiloRedirectStrategy();

  @Autowired
  private TenantCustomParamsService tenantCustomParamsService;

  @Autowired
  private SentiloModuleConfigService configService;

  @ModelAttribute(Constants.MODEL_TENANT_CUSTOM_PARAMS)
  public TenantCustomParamsDTO getTenantCustomParams() {
    return tenantCustomParamsService.getTenantCustomParams();
  }

  @ModelAttribute(Constants.MODEL_IS_MULTITENANT_ENABLED)
  public boolean isMultitenant() {
    return TenantContextHolder.isEnabled();
  }

  protected SentiloModuleConfigService getConfigService() {
    return configService;
  }

  protected String redirect(final Model model, final HttpServletRequest request, final RedirectAttributes attributes) {
    return redirectToList(model, request, attributes);
  }

  protected String redirectToList(final Model model, final HttpServletRequest request, final RedirectAttributes redirectAttributes) {
    final String currentTable = "Table";
    final String fromBack = redirectAttributes.containsAttribute("sfbr") ? (String) redirectAttributes.asMap().get("sfbr") : "true";
    // After update a resource, the resource list page must be displayed keeping the previous list's
    // state
    final String tableName = model.asMap().get(Constants.MODEL_ACTIVE_MENU).toString();

    return getRedirectToListUrl(request, tableName.substring(1) + currentTable, fromBack);
  }

  private String getRedirectToListUrl(final HttpServletRequest request, final String nameTableRecover, final String fromBack) {
    // postPath follows the expression /admin/{resource}/*** and redirect url will be something like
    // /admin/{resource}/list?
    final StringBuilder sb = new StringBuilder(buildRedirectUrl(request, false));
    sb.append("/list?nameTableRecover=").append(nameTableRecover).append("&sfbr=").append(fromBack);

    final String redirectUrl = redirectStrategy.buildRedirectUrl(request, sb.toString());

    return "redirect:" + redirectUrl;
  }

  protected String redirectToDetail(final HttpServletRequest request) {
    final StringBuilder sb = new StringBuilder(buildRedirectUrl(request, true)).append("/detail");
    final String redirectUrl = redirectStrategy.buildRedirectUrl(request, sb.toString());
    return "redirect:" + redirectUrl;
  }

  /**
   * Generates base redirect url from request.
   * 
   * @param request - servlet request
   * @param isDetail - if is true, add the entity name to path.
   * @return string with the url base
   */
  private String buildRedirectUrl(final HttpServletRequest request, final boolean isDetail) {
    final String postPath = request.getServletPath();
    final String[] tokens = postPath.split("/");

    final StringBuilder sb = new StringBuilder("/");
    if (TenantContextHolder.hasContext() && StringUtils.hasText(TenantUtils.getCurrentTenant())) {
      sb.append(TenantUtils.getCurrentTenant());
      sb.append("/");
    }
    sb.append(tokens[1]).append("/").append(tokens[2]);
    if (isDetail) {
      sb.append("/").append(tokens[3]);
    }
    return sb.toString();
  }
}
