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

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.sentilo.web.catalog.context.TenantContextHolder;
import org.sentilo.web.catalog.domain.CatalogDocument;
import org.sentilo.web.catalog.domain.FederatedResource;
import org.sentilo.web.catalog.dto.DataTablesDTO;
import org.sentilo.web.catalog.dto.LastSearchParamsDTO;
import org.sentilo.web.catalog.format.datetime.LocalDateFormatter;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.search.SearchFilterResolver;
import org.sentilo.web.catalog.search.SearchFilterResult;
import org.sentilo.web.catalog.search.builder.Column;
import org.sentilo.web.catalog.search.builder.DefaultSearchFilterBuilderImpl;
import org.sentilo.web.catalog.search.builder.SearchFilterBuilder;
import org.sentilo.web.catalog.search.builder.SearchFilterUtils;
import org.sentilo.web.catalog.security.service.CatalogUserDetailsService;
import org.sentilo.web.catalog.service.CrudService;
import org.sentilo.web.catalog.utils.CatalogUtils;
import org.sentilo.web.catalog.utils.Constants;
import org.sentilo.web.catalog.utils.ExcelGeneratorUtils;
import org.sentilo.web.catalog.utils.TenantUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.GenericTypeResolver;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort.Order;
import org.springframework.http.MediaType;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.ui.Model;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.servlet.ModelAndView;

import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * Base controller for search use cases.
 */
public abstract class SearchController<T extends CatalogDocument> extends CatalogBaseController implements SearchFilterResolver {

  protected static final String LIST_ACTION = "list";
  protected static final String EXCEL_VIEW = "excelView";

  /** Object with the parameters of the last search */
  private static final String LAST_SEARCH_PARAMS = "lastSearchParams";
  /**
   * Map with key = name of the Table list and value = Object with the parameters of the last search
   */
  private static final String LAST_SEARCH_PARAMS_MAP = "lastSearchParamsMap";

  @Autowired
  protected CatalogUserDetailsService userDetailsService;

  @Autowired
  private LocalDateFormatter localDateFormat;

  private final SearchFilterBuilder searchFilterBuilder = new DefaultSearchFilterBuilderImpl();
  private final Map<String, String> viewNames = new HashMap<String, String>();

  private String typeShortName;

  protected abstract CrudService<T> getService();

  protected abstract List<String> toRow(T resource);

  protected abstract void initViewNames();

  protected abstract Class<? extends CatalogDocument> getRowClass();

  @ModelAttribute(Constants.MODEL_TENANT_ID)
  public String getCurrentTenant() {
    return TenantUtils.getCurrentTenant();
  }

  @Override
  public SearchFilterBuilder getSearchFilterBuilder() {
    return this.searchFilterBuilder;
  }

  @RequestMapping(value = {"/", "/list"}, method = RequestMethod.GET)
  @PreAuthorize("@accessControlHandler.checkAccess(this, 'LIST')")
  public String emptyList(final Model model, final HttpServletRequest request, @RequestParam final String nameTableRecover,
      @RequestParam(required = false) final String sfbr) {

    // lastSearch state must be cleaned when the user selects any option from the menu (last
    // search filter only must be conserved when the user navigates into the list and come back
    // to it after leaving any register detail). Parameter sfbr indicates this case.

    // In case sfbr parameter is null, it means the user has selected any option from the
    // menu. Otherwise the lastSearchParamsMap should be recovered and iterated for get the
    // nameTableRecover object and pass it to the view

    if (!StringUtils.hasText(sfbr)) {
      clearLastSearch(request);
    } else {
      final LastSearchParamsDTO lastSearchParamsDTO = getLastSearch(request, nameTableRecover);
      model.addAttribute(LAST_SEARCH_PARAMS, lastSearchParamsDTO);
    }

    doBeforeShowListPage(model);

    return getNameOfViewToReturn(LIST_ACTION);
  }

  @RequestMapping(value = "/list/json", produces = MediaType.APPLICATION_JSON_VALUE)
  @PreAuthorize("@accessControlHandler.checkAccess(this, 'LIST')")
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
  @PreAuthorize("@accessControlHandler.checkAccess(this, 'LIST')")
  public ModelAndView getExcel(final Model model, final HttpServletRequest request, final HttpServletResponse response,
      @RequestParam final String tableName) throws IOException {
    // Extract the list with all his search parameters into an Excel.
    final SearchFilterResult<T> result = getExcelExport(request, response, tableName);
    final List<List<String>> rows = new ArrayList<List<String>>();
    for (final T resource : result.getContent()) {
      final List<String> row = toExcelRow(resource);
      rows.add(row);
    }

    doBeforeExcelBuilder(model);
    model.addAttribute(Constants.RESULT_LIST, rows);

    return new ModelAndView(EXCEL_VIEW, tableName, model);
  }

  public LocalDateFormatter getLocalDateFormat() {
    return localDateFormat;
  }

  protected String getTypeShortName() {
    if (typeShortName == null) {
      final Class<?> clazz = GenericTypeResolver.resolveTypeArgument(this.getClass(), SearchController.class);
      typeShortName = clazz.getSimpleName().toLowerCase();
    }
    return typeShortName;
  }

  protected DataTablesDTO toDataTables(final Integer sEcho, final List<T> resources, final Long count) {

    final DataTablesDTO dataTables = new DataTablesDTO();
    dataTables.setsEcho(sEcho);
    dataTables.setiTotalDisplayRecords(Long.valueOf(resources.size()));
    dataTables.setiTotalRecords(count);
    dataTables.setTotalCount(count);

    for (final T resource : resources) {
      final Map<String, String> rowMetadata = new HashMap<String, String>();
      final List<String> row = toRow(resource);
      addRowMetadata(resource, rowMetadata);

      if (!rowMetadata.isEmpty()) {
        writeRowMetadata(rowMetadata, row);
      }

      dataTables.add(row);
    }
    return dataTables;
  }

  private void writeRowMetadata(final Map<String, String> rowMetadata, final List<String> row) {
    try {
      row.add(new ObjectMapper().writeValueAsString(rowMetadata));
    } catch (final Exception e) {
      LOGGER.warn("Error writing rowMetadata to row . This rowMetadata is rejected", e);
    }
  }

  /**
   * rowMetadata allows to add additional info needed to render the row
   *
   * @param resource
   * @param rowMetadata
   */
  protected void addRowMetadata(final T resource, final Map<String, String> rowMetadata) {
    final boolean isSuperAdminUser =
        userDetailsService.getCatalogUserDetails() != null && userDetailsService.getCatalogUserDetails().isSuperAdminUser();
    if (TenantContextHolder.hasContext() && !isSuperAdminUser) {
      checkResourceOwner(resource, rowMetadata);
    }
    checkIsFederated(resource, rowMetadata);
  }

  protected void addHideCbMetadata(final Map<String, String> rowMetadata) {
    rowMetadata.put("hideCheckbox", Boolean.TRUE.toString());
  }

  /**
   * If multitenant feature is enabled, only resources which are owned by the user's tenant must
   * render the first column as a checkbox.
   *
   * @param resource
   * @param rowMetadata
   */
  private void checkResourceOwner(final T resource, final Map<String, String> rowMetadata) {
    if (!TenantUtils.isCurrentTenantResource(resource)) {
      addHideCbMetadata(rowMetadata);
    }
  }

  /**
   * Only the not federated resources must render the first column as a checkbox.
   *
   * @param resource
   * @param rowMetadata
   */
  private void checkIsFederated(final T resource, final Map<String, String> rowMetadata) {
    if (resource instanceof FederatedResource && ((FederatedResource) resource).getFederatedResource()) {
      addHideCbMetadata(rowMetadata);
    }
  }

  protected String getNameOfViewToReturn(final String actionName) {
    if (CollectionUtils.isEmpty(getViewNames())) {
      initViewNames();
    }

    return getViewNames().get(actionName);
  }

  protected void doBeforeShowListPage(final Model model) {
    // To override by subclasses.
  }

  protected void doBeforeSearchPage(final HttpServletRequest request, final SearchFilter filter) {
    // To override by subclasses.
  }

  protected Map<String, String> getViewNames() {
    return viewNames;
  }

  protected List<String> toExcelRow(final T resource) {
    // Must be overrided by subclasses to get the excel export rows
    return toRow(resource);
  }

  protected void doBeforeExcelBuilder(final Model model) {
    // To override by subclasses.
    model.addAttribute(Constants.MESSAGE_KEYS_PREFIX, getTypeShortName());
    model.addAttribute(Constants.LIST_COLUMN_NAMES, ExcelGeneratorUtils.getColumnsNames(getTypeShortName()));
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

    final String sortDirection = isAsc ? Constants.ASC : Constants.DESC;

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

  private void clearLastSearch(final HttpServletRequest request) {
    request.getSession().removeAttribute(LAST_SEARCH_PARAMS_MAP);
  }

  private SearchFilterResult<T> getResultList(final HttpServletRequest request, final Pageable pageable, final String search) {
    final SearchFilter filter =
        getSearchFilterBuilder().buildSearchFilter(request, pageable, CatalogUtils.decodeAjaxParam(search), userDetailsService);
    doBeforeSearchPage(request, filter);
    return getService().search(filter);
  }

  private SearchFilterResult<T> getExcelExport(final HttpServletRequest request, final HttpServletResponse response, final String tableName)
      throws IOException {
    final LastSearchParamsDTO lastSearchParamsDTO = getLastSearch(request, tableName);
    final Pageable pageable = PageRequest.of(0, Integer.MAX_VALUE, SearchFilterUtils.getSort(request, lastSearchParamsDTO));
    return getResultList(request, pageable, lastSearchParamsDTO.getWordToSearch());
  }

}
