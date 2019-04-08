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
package org.sentilo.web.catalog.test.search.resolver;

import static org.mockito.Mockito.when;

import javax.servlet.http.HttpServletRequest;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.web.catalog.search.builder.DefaultSearchFilterBuilderImpl;
import org.sentilo.web.catalog.search.resolver.CatalogSortHandlerMethodArgumentResolver;
import org.springframework.core.MethodParameter;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.web.bind.support.WebDataBinderFactory;
import org.springframework.web.context.request.NativeWebRequest;
import org.springframework.web.method.support.ModelAndViewContainer;

public class CatalogSortHandlerMethodArgumentResolverTest {

  @InjectMocks
  private CatalogSortHandlerMethodArgumentResolver resolver;

  @Mock
  private MethodParameter parameter;

  @Mock
  private ModelAndViewContainer mavContainer;

  @Mock
  private NativeWebRequest webRequest;

  @Mock
  private WebDataBinderFactory binderFactory;

  @Mock
  private HttpServletRequest request;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    when(webRequest.getNativeRequest()).thenReturn(request);
    when(request.getContextPath()).thenReturn("/sentilo-catalog-web");
    when(request.getServletPath()).thenReturn("/");
    when(request.getRequestURI()).thenReturn("/admin/users/list");

    // Initializes static variables needed to resolve arguments
    new DefaultSearchFilterBuilderImpl();
  }

  @Test
  public void resolveDefaultArguments() throws Exception {
    final Sort sort = resolver.resolveArgument(parameter, mavContainer, webRequest, binderFactory);

    Assert.assertNotNull(sort.getOrderFor("_id"));
    Assert.assertEquals(Direction.ASC, sort.getOrderFor("_id").getDirection());
  }

  @Test
  public void resolveArgument() throws Exception {
    when(webRequest.getParameter("page.sort")).thenReturn("2");
    when(webRequest.getParameter("page.sort.dir")).thenReturn("DESC");

    final Sort sort = resolver.resolveArgument(parameter, mavContainer, webRequest, binderFactory);

    Assert.assertNotNull(sort.getOrderFor("name"));
    Assert.assertEquals(Direction.DESC, sort.getOrderFor("name").getDirection());
  }
}
