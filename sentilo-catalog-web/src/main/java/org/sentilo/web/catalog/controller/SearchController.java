/*
 * Sentilo
 * 
 * Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de Barcelona.
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

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;

import org.sentilo.web.catalog.domain.CatalogDocument;
import org.sentilo.web.catalog.dto.DataTablesDTO;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.search.SearchFilterResolver;
import org.sentilo.web.catalog.search.SearchFilterResult;
import org.sentilo.web.catalog.search.builder.DefaultSearchFilterBuilderImpl;
import org.sentilo.web.catalog.search.builder.SearchFilterBuilder;
import org.sentilo.web.catalog.service.CrudService;
import org.sentilo.web.catalog.utils.CatalogUtils;
import org.springframework.data.domain.Pageable;
import org.springframework.ui.Model;
import org.springframework.util.CollectionUtils;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

/**
 * Base controller for search use cases.
 */
public abstract class SearchController<T extends CatalogDocument> implements SearchFilterResolver {

  protected static final String LIST_ACTION = "list";
  protected SearchFilterBuilder searchFilterBuilder = new DefaultSearchFilterBuilderImpl();
  protected Map<String, String> viewNames = new HashMap<String, String>();

  protected abstract CrudService<T> getService();

  protected abstract List<String> toRow(T resource);

  protected abstract void initViewNames();

  public SearchFilterBuilder getSearchFilterBuilder() {
    return this.searchFilterBuilder;
  }

  @RequestMapping(value = {"/", "/list"}, method = RequestMethod.GET)
  public String emptyList(final Model model) {
    return getNameOfViewToReturn(LIST_ACTION);
  }

  @RequestMapping("/list/json")
  @ResponseBody
  public DataTablesDTO getPageList(final Model model, final HttpServletRequest request, final Pageable pageable, @RequestParam final Integer sEcho, @RequestParam(required = false) final String search) {
    final SearchFilter filter = getSearchFilterBuilder().buildSearchFilter(request, pageable, CatalogUtils.decodeAjaxParam(search));
    doBeforeSearchPage(request, filter);
    final SearchFilterResult<T> result = getService().search(filter);
    final List<T> resources = result.getContent();
    final Long count = result.getTotalElements();
    return toDataTables(sEcho, resources, count);
  }

  protected DataTablesDTO toDataTables(final Integer sEcho, final List<T> resources, final Long count) {

    final DataTablesDTO dataTables = new DataTablesDTO();
    dataTables.setsEcho(sEcho);
    dataTables.setiTotalDisplayRecords(Long.valueOf(resources.size()));
    dataTables.setiTotalRecords(count);
    dataTables.setTotalCount(count);

    for (final T resource : resources) {
      final List<String> row = toRow(resource);
      dataTables.add(row);
    }
    return dataTables;
  }

  protected String getNameOfViewToReturn(final String actionName) {
    if (CollectionUtils.isEmpty(viewNames)) {
      initViewNames();
    }

    return viewNames.get(actionName);
  }

  protected void doBeforeSearchPage(final HttpServletRequest request, final SearchFilter filter) {
    // To override by subclasses.
  }

}
