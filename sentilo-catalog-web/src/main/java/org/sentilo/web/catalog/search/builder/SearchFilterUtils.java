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
package org.sentilo.web.catalog.search.builder;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;

import org.sentilo.web.catalog.dto.LastSearchParamsDTO;
import org.sentilo.web.catalog.exception.SearchFilterException;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.util.AntPathMatcher;
import org.springframework.util.PathMatcher;
import org.springframework.web.util.UrlPathHelper;

/**
 * Utility class that lets translate view sort columns (which are represented by numbers 1,2.----)
 * in logic sort columns (i.e. field domain object names).
 */
public abstract class SearchFilterUtils {

  private static final Map<String, List<Column>> GLOBAL_LIST_COLUMNS = new HashMap<String, List<Column>>();
  private static final UrlPathHelper URL_PATH_HELPER = new UrlPathHelper();
  private static PathMatcher pathMatcher = new AntPathMatcher();

  private SearchFilterUtils() {
    // this prevents even the native class from calling this ctor as well :
    throw new AssertionError();
  }

  public static String translateSortProperty(final String listName, final String columnNumber) {
    final List<Column> listColumns = getListColumns(listName);

    final int pos = Integer.parseInt(columnNumber);

    if (listColumns.size() < pos) {
      throw new SearchFilterException("error.search.filter.invalid.columns.number", new Object[] {listName});
    }

    return listColumns.get(pos - 1).getName();
  }

  public static List<Column> getListColumns(final String listName) {
    if (!GLOBAL_LIST_COLUMNS.containsKey(listName)) {
      throw new SearchFilterException("error.search.filter.columns.not.found", new Object[] {listName});
    }

    return GLOBAL_LIST_COLUMNS.get(listName);
  }

  public static List<Column> getListColumns(final HttpServletRequest request) {
    return getListColumns(getListName(request));
  }

  public static String getListName(final HttpServletRequest request) {
    final String pathWithinApplication = URL_PATH_HELPER.getLookupPathForRequest(request);
    // pathWithinApplication must have the pattern /{listName}/list/json with listName equals to
    // provider, sensor, component, ...
    // or /admin/XYZ.
    // If listName is /admin/XYZ, this method return only XYZ
    final String[] tokens = pathWithinApplication.split("/");
    if (pathWithinApplication.startsWith("/admin")) {
      return tokens[2];
    } else {
      return tokens[1];
    }
  }

  public static String getUriVariableValue(final HttpServletRequest request, final String pattern, final String variable) {
    final Map<String, String> uriVariables = pathMatcher.extractUriTemplateVariables(pattern, URL_PATH_HELPER.getLookupPathForRequest(request));
    return uriVariables.get(variable);
  }

  public static Sort getSort(final HttpServletRequest request, final LastSearchParamsDTO lastSearchParamsDTO) {
    final String columnName = translateSortProperty(SearchFilterUtils.getListName(request), String.valueOf(lastSearchParamsDTO.getSortColum()));
    final Direction order = Direction.fromString(lastSearchParamsDTO.getSortDirecction());
    return new Sort(order, columnName);
  }

  /**
   * @see #getListName(HttpServletRequest)
   */
  static void addListColumns(final String listName, final List<Column> columns) {
    GLOBAL_LIST_COLUMNS.put(listName, columns);
  }

}
