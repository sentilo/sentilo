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

import org.sentilo.web.catalog.controller.SearchController;
import org.sentilo.web.catalog.domain.ActiveSubscription;
import org.sentilo.web.catalog.domain.CatalogDocument;
import org.sentilo.web.catalog.dto.DataTablesDTO;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.search.builder.SearchFilterUtils;
import org.sentilo.web.catalog.service.ActiveSubscriptionsService;
import org.sentilo.web.catalog.service.CrudService;
import org.sentilo.web.catalog.utils.Constants;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Pageable;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

@Controller
@RequestMapping("/admin/subscriptions")
public class SubscriptionsController extends SearchController<ActiveSubscription> {

  @Autowired
  private ActiveSubscriptionsService activeSubscriptionService;

  @RequestMapping(value = "/{entityId}", produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  public DataTablesDTO getActiveSubscriptions(final HttpServletRequest request, final Model model, final Pageable pageable,
      @PathVariable final String entityId, @RequestParam final Integer sEcho, @RequestParam final String tableName,
      @RequestParam(required = false) final String search) {

    return getPageList(model, request, pageable, sEcho, tableName, search);
  }

  @Override
  protected void doBeforeSearchPage(final HttpServletRequest request, final SearchFilter filter) {
    final String uri = "/admin/subscriptions/{entityId}";
    final String entityId = SearchFilterUtils.getUriVariableValue(request, uri, Constants.MODEL_ENTITY_ID);

    if (StringUtils.hasText(entityId)) {
      filter.addAndParam("entityId", entityId);
    }
  }

  @Override
  protected List<String> toRow(final ActiveSubscription resource) {
    final List<String> row = new ArrayList<String>();
    row.add(resource.getId()); // First column. It's not visible
    row.add(resource.getSubscriptionType());
    if (resource.getProvider() != null && resource.getProvider().endsWith("*")) {
      row.add(resource.getProvider().substring(0, resource.getProvider().length() - 1));
    } else {
      row.add(resource.getProvider());
    }
    row.add(resource.getSensor());
    row.add(resource.getAlert());
    row.add(resource.getEndpoint());
    row.add(String.valueOf(resource.getMaxRetries()));
    row.add(String.valueOf(resource.getRetryDelay()));
    return row;
  }

  @Override
  protected CrudService<ActiveSubscription> getService() {
    return activeSubscriptionService;
  }

  @Override
  protected void initViewNames() {
    // Doesn't apply for this implementation (child search tab)
  }

  @Override
  protected Class<? extends CatalogDocument> getRowClass() {
    return ActiveSubscription.class;
  }

}
