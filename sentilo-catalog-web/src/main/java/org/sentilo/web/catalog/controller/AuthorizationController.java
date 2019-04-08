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

import org.sentilo.web.catalog.context.TenantContextHolder;
import org.sentilo.web.catalog.utils.Constants;
import org.sentilo.web.catalog.utils.TenantUtils;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

@Controller
@RequestMapping("/auth")
public class AuthorizationController extends CatalogBaseController {

  @RequestMapping(value = "/login", method = RequestMethod.GET)
  public String getLoginPage(@RequestParam(value = "error", required = false) final String error) {
    return Constants.VIEW_LOGIN;
  }

  @RequestMapping(value = "/denied", method = RequestMethod.GET)
  public String getLoginDeniedPage() {
    return Constants.VIEW_LOGIN_DENIED;
  }

  @RequestMapping(value = "/login/failure")
  public String loginFailure() {
    final String message = "Login Failure!";
    return buildRedirectUrl(message);
  }

  @RequestMapping(value = "/logout/success")
  public String logoutSuccess() {
    final String message = "Logout Success!";
    return buildRedirectUrl(message);
  }

  private String buildRedirectUrl(final String message) {
    final String prefixRedirectUrl = TenantContextHolder.hasContext() ? "/" + TenantUtils.getCurrentTenant() : "";
    return "redirect:" + prefixRedirectUrl + "/auth/login.html?message=" + message;
  }
}
