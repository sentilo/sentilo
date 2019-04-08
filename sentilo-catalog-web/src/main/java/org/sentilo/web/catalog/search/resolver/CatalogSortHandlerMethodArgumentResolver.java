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
package org.sentilo.web.catalog.search.resolver;

import javax.servlet.http.HttpServletRequest;

import org.sentilo.web.catalog.search.builder.SearchFilterUtils;
import org.springframework.core.MethodParameter;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.data.web.SortDefault;
import org.springframework.data.web.SortHandlerMethodArgumentResolver;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.support.WebDataBinderFactory;
import org.springframework.web.context.request.NativeWebRequest;
import org.springframework.web.method.support.HandlerMethodArgumentResolver;
import org.springframework.web.method.support.ModelAndViewContainer;

/**
 * {@link HandlerMethodArgumentResolver} to automatically create {@link Sort} instances from request
 * parameters or {@link SortDefault} annotations from catalog lists.
 *
 * Based on {@link SortHandlerMethodArgumentResolver}.
 *
 * @since 1.3.0
 */
public class CatalogSortHandlerMethodArgumentResolver extends SortHandlerMethodArgumentResolver {

  private static final String DEFAULT_SORT_NUMBER_COLUMN = "1";
  private static final String DEFAULT_SORT_DIRECTION = "ASC";

  private static final String SORT_PARAMETER = "page.sort";
  private static final String SORT_DIR_PARAMETER = "page.sort.dir";

  /*
   * (non-Javadoc)
   *
   * @see org.springframework.web.method.support.HandlerMethodArgumentResolver#resolveArgument(org.
   * springframework.core.MethodParameter,
   * org.springframework.web.method.support.ModelAndViewContainer,
   * org.springframework.web.context.request.NativeWebRequest,
   * org.springframework.web.bind.support.WebDataBinderFactory)
   */
  @Override
  public Sort resolveArgument(final MethodParameter parameter, final ModelAndViewContainer mavContainer, final NativeWebRequest webRequest,
      final WebDataBinderFactory binderFactory) {

    // By default, all lists are ordered by the first column in an ascending direction
    final String sortParameterValue = webRequest.getParameter(SORT_PARAMETER);
    final String sortDirParameterValue = webRequest.getParameter(SORT_DIR_PARAMETER);

    if (StringUtils.hasText(sortDirParameterValue) && StringUtils.hasText(sortParameterValue)) {
      return parseParameterIntoSort(sortParameterValue, sortDirParameterValue, webRequest);
    } else {
      return parseParameterIntoSort(DEFAULT_SORT_NUMBER_COLUMN, DEFAULT_SORT_DIRECTION, webRequest);
    }
  }

  private Sort parseParameterIntoSort(final String sortParameterValue, final String sortDirParameterValue, final NativeWebRequest webRequest) {
    final String columnName = SearchFilterUtils.translateSortProperty(getListName(webRequest), sortParameterValue);
    final Direction order = null == sortDirParameterValue ? Direction.ASC : Direction.fromString(sortDirParameterValue);
    return new Sort(order, columnName);
  }

  private String getListName(final NativeWebRequest webRequest) {
    return SearchFilterUtils.getListName((HttpServletRequest) webRequest.getNativeRequest());
  }

}
