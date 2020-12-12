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
import org.sentilo.web.catalog.domain.ActiveSubscription;
import org.sentilo.web.catalog.domain.CatalogDocument;
import org.sentilo.web.catalog.service.ActiveSubscriptionsService;
import org.sentilo.web.catalog.service.CrudService;
import org.sentilo.web.catalog.utils.Constants;
import org.sentilo.web.catalog.utils.ExcelGeneratorUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestMapping;

@Controller
@RequestMapping("/admin/activesubscriptions")
public class ActiveSubscriptionsController extends CrudController<ActiveSubscription> {

  @Autowired
  private ActiveSubscriptionsService activeSubscriptionsService;

  @ModelAttribute(Constants.MODEL_ACTIVE_MENU)
  public String getActiveMenu() {
    return Constants.MENU_ACTIVE_SUBSCRIPTIONS;
  }

  @Override
  protected CrudService<ActiveSubscription> getService() {
    return activeSubscriptionsService;
  }

  @Override
  protected List<String> toRow(final ActiveSubscription resource) {
    final List<String> row = new ArrayList<String>();
    row.add(""); // First column
    row.add(resource.getEntityId());
    row.add(resource.getEntityType().name());
    row.add(resource.getSubscriptionType());
    if (resource.getProvider() != null && resource.getProvider().endsWith("*")) {
      row.add(resource.getProvider().substring(0, resource.getProvider().length() - 1));
    } else {
      row.add(resource.getProvider());
    }
    row.add(resource.getSensor());
    row.add(resource.getEndpoint());
    row.add(String.valueOf(resource.getMaxRetries()));
    row.add(String.valueOf(resource.getRetryDelay()));
    return row;
  }

  @Override
  protected List<String> toExcelRow(final ActiveSubscription activeSubscription) {
    return ExcelGeneratorUtils.getActiveSubscriptionsExcelRowsData(activeSubscription, getLocalDateFormat(),
        userDetailsService.getCatalogUserDetails());
  }

  @Override
  protected void initViewNames() {
    getViewNames().put(LIST_ACTION, Constants.VIEW_ACTIVE_SUBSCRIPTIONS_LIST);
  }

  @Override
  protected Class<? extends CatalogDocument> getRowClass() {
    return ActiveSubscription.class;
  }

  @Override
  protected ActiveSubscription buildNewEntity(final String id) {
    return new ActiveSubscription(id);
  }

  @Override
  protected String getEntityModelKey() {
    return Constants.MODEL_ACTIVE_SUBSCRIPTIONS;
  }

}
