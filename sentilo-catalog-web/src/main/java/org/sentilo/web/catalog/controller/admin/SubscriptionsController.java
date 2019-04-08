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

import org.sentilo.web.catalog.domain.ActiveSubscription;
import org.sentilo.web.catalog.dto.DataTablesDTO;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.search.SearchFilterResult;
import org.sentilo.web.catalog.search.builder.DefaultSearchFilterBuilderImpl;
import org.sentilo.web.catalog.search.builder.SearchFilterBuilder;
import org.sentilo.web.catalog.security.service.CatalogUserDetailsService;
import org.sentilo.web.catalog.service.ActiveSubscriptionsService;
import org.sentilo.web.catalog.utils.CatalogUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Pageable;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

@Controller
@RequestMapping("/admin/subscriptions")
public class SubscriptionsController {

  @Autowired
  private ActiveSubscriptionsService activeSubscriptionService;

  @Autowired
  protected CatalogUserDetailsService userDetailsService;

  @RequestMapping(value = "/{entityId}", produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  public DataTablesDTO getActiveSubscriptions(final HttpServletRequest request, final Model model, final Pageable pageable,
      @PathVariable final String entityId, @RequestParam final Integer sEcho, @RequestParam(required = false) final String search) {
    final SearchFilter searchFilter = buildSearchFilter(request, pageable, entityId, search);
    final SearchFilterResult<ActiveSubscription> result = activeSubscriptionService.search(searchFilter);
    return toDataTables(pageable, sEcho, result.getContent());
  }

  private SearchFilter buildSearchFilter(final HttpServletRequest request, final Pageable pageable, final String entityId, final String search) {
    final SearchFilterBuilder searchFilterBuilder = new DefaultSearchFilterBuilderImpl();
    final SearchFilter searchFilter =
        searchFilterBuilder.buildSearchFilter(request, pageable, CatalogUtils.decodeAjaxParam(search), userDetailsService);
    searchFilter.addAndParam("entityId", entityId);
    return searchFilter;
  }

  private DataTablesDTO toDataTables(final Pageable pageable, final Integer sEcho, final List<ActiveSubscription> list) {
    // Subscriptions are stored in Redis and no in MongoDB, so the pagination could not be done in
    // the repository
    List<ActiveSubscription> subsList;

    if (pageable.getPageNumber() == 0) {
      if (list.size() >= pageable.getPageSize()) {
        subsList = list.subList(0, pageable.getPageSize());
      } else {
        subsList = list;
      }
    } else {
      final int fromIndex = pageable.getPageSize() * pageable.getPageNumber();
      final int toIndex = fromIndex + pageable.getPageSize();
      if (list.size() >= toIndex) {
        subsList = list.subList(fromIndex, toIndex);
      } else {
        if (list.size() >= fromIndex) {
          subsList = list.subList(fromIndex, list.size());
        } else {
          subsList = list;
        }
      }
    }
    return toDataTables(sEcho, subsList, (long) list.size());
  }

  private DataTablesDTO toDataTables(final Integer sEcho, final List<ActiveSubscription> resources, final Long count) {

    final DataTablesDTO dataTables = new DataTablesDTO();
    dataTables.setsEcho(sEcho);
    dataTables.setiTotalDisplayRecords(Long.valueOf(resources.size()));
    dataTables.setiTotalRecords(count);
    dataTables.setTotalCount(count);

    for (final ActiveSubscription subscription : resources) {
      final List<String> row = toRow(subscription);
      dataTables.add(row);
    }
    return dataTables;
  }

  private List<String> toRow(final ActiveSubscription resource) {
    final List<String> row = new ArrayList<String>();
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

}
