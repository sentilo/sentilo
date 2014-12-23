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

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.sentilo.web.catalog.domain.CatalogDocument;
import org.sentilo.web.catalog.dto.DataTablesDTO;
import org.sentilo.web.catalog.dto.LastSearchParamsDTO;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.search.SearchFilterResolver;
import org.sentilo.web.catalog.search.SearchFilterResult;
import org.sentilo.web.catalog.search.builder.Column;
import org.sentilo.web.catalog.search.builder.DefaultSearchFilterBuilderImpl;
import org.sentilo.web.catalog.search.builder.SearchFilterBuilder;
import org.sentilo.web.catalog.search.builder.SearchFilterUtils;
import org.sentilo.web.catalog.service.CrudService;
import org.sentilo.web.catalog.utils.CatalogUtils;
import org.sentilo.web.catalog.utils.Constants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort.Order;
import org.springframework.ui.Model;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.servlet.ModelAndView;

/**
 * Base controller for search use cases.
 */
public abstract class SearchController<T extends CatalogDocument> implements SearchFilterResolver {

  protected static final String LIST_ACTION = "list";
  protected static final String EXCEL_VIEW = "excelView";

  /** Object with the parameters of the last search */
  private static final String LAST_SEARCH_PARAMS = "lastSearchParams";
  /**
   * Map with key = name of the Table list and value = Object with the parameters of the last search
   */
  private static final String LAST_SEARCH_PARAMS_MAP = "lastSearchParamsMap";

  protected final Logger logger = LoggerFactory.getLogger(SearchController.class);
  private final SearchFilterBuilder searchFilterBuilder = new DefaultSearchFilterBuilderImpl();
  private final Map<String, String> viewNames = new HashMap<String, String>();

  protected abstract CrudService<T> getService();

  protected abstract List<String> toRow(T resource);

  protected abstract void doBeforeExcelBuilder(Model model);

  protected abstract void initViewNames();

  @Override
  public SearchFilterBuilder getSearchFilterBuilder() {
    return this.searchFilterBuilder;
  }

  @RequestMapping(value = {"/", "/list"}, method = RequestMethod.GET)
  public String emptyList(final Model model, final HttpServletRequest request, @RequestParam final String nameTableRecover,
      @RequestParam(required = false) final String fromBack) {
    final LastSearchParamsDTO lastSearchParamsDTO = getLastSearch(request, nameTableRecover);
    if (lastSearchParamsDTO != null) {
      // lastSearchParamsMap must be cleaned when the user selects any option from the menu (last
      // search filter only must be conserved when the user navigates into the list and come back
      // to it after leaving any register detail). Parameter fromBack.
      // In case fromBack parameter is null, it means the user has selected an option from the menu,
      // otherwise the lastSearchParamsMap must be recovered and iterated for get the
      // nameTableRecover
      // object and pass it to the view
      if (!StringUtils.hasText(fromBack)) {
        lastSearchParamsDTO.clean();
      }
      model.addAttribute(LAST_SEARCH_PARAMS, lastSearchParamsDTO);
    }

    return getNameOfViewToReturn(LIST_ACTION);
  }

  @RequestMapping("/list/json")
  @ResponseBody
  public DataTablesDTO getPageList(final Model model, final HttpServletRequest request, final Pageable pageable, @RequestParam final Integer sEcho,
      @RequestParam final String tableName, @RequestParam(required = false) final String search) {

    saveLastSearchParams(request, tableName, pageable, search);
    final SearchFilterResult<T> result = getResultList(request, pageable, search);
    final List<T> resources = result.getContent();
    final Long count = result.getTotalElements();
    return toDataTables(sEcho, resources, count);
  }

  @RequestMapping(value = "/list/excel", method = RequestMethod.GET)
  public ModelAndView getExcel(final Model model, final HttpServletRequest request, final HttpServletResponse response,
      @RequestParam final String tableName) throws IOException {
    // Extract the list with all his search parameters into an Excel.
    final SearchFilterResult<T> result = getExcelExport(request, response, tableName);
    final List<List<String>> rows = new ArrayList<List<String>>();
    for (final T resource : result.getContent()) {
      final List<String> row = toRow(resource);
      rows.add(row);
    }

    doBeforeExcelBuilder(model);
    model.addAttribute(Constants.RESULT_LIST, rows);
    return new ModelAndView(EXCEL_VIEW, tableName, model);
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
    if (CollectionUtils.isEmpty(getViewNames())) {
      initViewNames();
    }

    return getViewNames().get(actionName);
  }

  protected void doBeforeSearchPage(final HttpServletRequest request, final SearchFilter filter) {
    // To override by subclasses.
  }

  protected Map<String, String> getViewNames() {
    return viewNames;
  }

  @SuppressWarnings("unchecked")
  private void saveLastSearchParams(final HttpServletRequest request, final String listName, final Pageable pageable, final String search) {

    final List<Column> listColumns = SearchFilterUtils.getListColumns(request);
    boolean isAsc = true;
    int sortColumnPosition = 0;
    for (int i = 0; i < listColumns.size(); i++) {
      final Order order = pageable.getSort().getOrderFor(listColumns.get(i).getName());
      if (order != null) {
        isAsc = order.isAscending();
        sortColumnPosition = i + 1;
        break;
      }
    }

    final String sortDirection = (isAsc ? Constants.ASC : Constants.DESC);

    final LastSearchParamsDTO backSearch =
        new LastSearchParamsDTO(pageable.getPageNumber(), pageable.getPageSize(), search, listName, sortColumnPosition, sortDirection);
    Map<String, LastSearchParamsDTO> map = (Map<String, LastSearchParamsDTO>) request.getSession().getAttribute(LAST_SEARCH_PARAMS_MAP);
    if (map == null) {
      map = new HashMap<String, LastSearchParamsDTO>();
    }
    map.put(listName, backSearch);
    request.getSession().setAttribute(LAST_SEARCH_PARAMS_MAP, map);
  }

  @SuppressWarnings("unchecked")
  private LastSearchParamsDTO getLastSearch(final HttpServletRequest request, final String nameTableRecover) {
    final Map<String, LastSearchParamsDTO> lastSearchParamsMap =
        (Map<String, LastSearchParamsDTO>) request.getSession().getAttribute(LAST_SEARCH_PARAMS_MAP);
    LastSearchParamsDTO lastSearchParamsDTO = null;
    if (!CollectionUtils.isEmpty(lastSearchParamsMap) && lastSearchParamsMap.get(nameTableRecover) != null) {
      lastSearchParamsDTO = lastSearchParamsMap.get(nameTableRecover);
    }

    return lastSearchParamsDTO;
  }

  private SearchFilterResult<T> getResultList(final HttpServletRequest request, final Pageable pageable, final String search) {
    final SearchFilter filter = getSearchFilterBuilder().buildSearchFilter(request, pageable, CatalogUtils.decodeAjaxParam(search));
    doBeforeSearchPage(request, filter);
    return getService().search(filter);
  }

  private SearchFilterResult<T> getExcelExport(final HttpServletRequest request, final HttpServletResponse response, final String tableName)
      throws IOException {
    final LastSearchParamsDTO lastSearchParamsDTO = getLastSearch(request, tableName);
    final Pageable pageable = new PageRequest(0, Integer.MAX_VALUE, SearchFilterUtils.getSort(request, lastSearchParamsDTO));
    return getResultList(request, pageable, lastSearchParamsDTO.getWordToSearch());
  }

}
