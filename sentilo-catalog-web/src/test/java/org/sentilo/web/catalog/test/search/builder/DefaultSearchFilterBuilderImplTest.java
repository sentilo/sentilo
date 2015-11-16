/*
 * Sentilo
 *  
 * Original version 1.4 Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de Barcelona.
 * Modified by Opentrends adding support for multitenant deployments and SaaS. Modifications on version 1.5 Copyright (C) 2015 Opentrends Solucions i Sistemes, S.L.
 * 
 *   
 * This program is licensed and may be used, modified and redistributed under the
 * terms  of the European Public License (EUPL), either version 1.1 or (at your 
 * option) any later version as soon as they are approved by the European 
 * Commission.
 *   
 * Alternatively, you may redistribute and/or modify this program under the terms
 * of the GNU Lesser General Public License as published by the Free Software 
 * Foundation; either  version 3 of the License, or (at your option) any later 
 * version. 
 *   
 * Unless required by applicable law or agreed to in writing, software distributed
 * under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR 
 * CONDITIONS OF ANY KIND, either express or implied. 
 *   
 * See the licenses for the specific language governing permissions, limitations 
 * and more details.
 *   
 * You should have received a copy of the EUPL1.1 and the LGPLv3 licenses along 
 * with this program; if not, you may find them at: 
 *   
 *   https://joinup.ec.europa.eu/software/page/eupl/licence-eupl
 *   http://www.gnu.org/licenses/ 
 *   and 
 *   https://www.gnu.org/licenses/lgpl.txt
 */
package org.sentilo.web.catalog.test.search.builder;

import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.web.catalog.context.TenantContextHolder;
import org.sentilo.web.catalog.context.TenantContextImpl;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.search.builder.Column;
import org.sentilo.web.catalog.search.builder.DefaultSearchFilterBuilderImpl;
import org.sentilo.web.catalog.search.builder.SearchFilterUtils;
import org.sentilo.web.catalog.security.CatalogUserDetails;
import org.sentilo.web.catalog.security.service.CatalogUserDetailsService;
import org.springframework.data.domain.Pageable;

@RunWith(PowerMockRunner.class)
@PrepareForTest(SearchFilterUtils.class)
public class DefaultSearchFilterBuilderImplTest {

  private final String mockTenant = "mockTenant";

  @InjectMocks
  private final DefaultSearchFilterBuilderImpl search = new DefaultSearchFilterBuilderImpl();

  @Mock
  private HttpServletRequest request;

  @Mock
  private Pageable pageable;

  @Mock
  private CatalogUserDetailsService userDetailsService;

  @Mock
  private CatalogUserDetails userDetails;

  private List<Column> columns;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);

    final Map<String, Object> dictionary = new HashMap<String, Object>();
    dictionary.put("Static", 0);
    dictionary.put("Mobile", 1);

    columns = new ArrayList<Column>();
    columns.add(new Column("name", true, true));
    columns.add(new Column("description", true, true));
    columns.add(new Column("providerId", true, true));
    columns.add(new Column("mobile", true, true, dictionary));
    columns.add(new Column("createdAt", true));

    TenantContextHolder.setContext(new TenantContextImpl(mockTenant));

    when(userDetailsService.getCatalogUserDetails()).thenReturn(userDetails);
  }

  @Test
  public void testBuildSearchParams() {
    PowerMockito.mockStatic(SearchFilterUtils.class);
    when(SearchFilterUtils.getListColumns(request)).thenReturn(columns);
    when(request.getParameter("tableName")).thenReturn("mockTable");
    final SearchFilter result = search.buildSearchFilter(request, pageable, "Mobi", userDetailsService);
    Assert.assertEquals(1, result.getParams().get("mobile"));

  }

  @Test
  public void testFilterContainsTenant() {
    System.setProperty(SentiloConstants.SENTILO_MULTITENANT_PROP_KEY, Boolean.TRUE.toString());
    PowerMockito.mockStatic(SearchFilterUtils.class);
    when(SearchFilterUtils.getListColumns(request)).thenReturn(columns);
    when(request.getParameter("tableName")).thenReturn("mockTable").thenReturn("sensorTypeTable");

    // First request: dataTable requested is named mockTable
    SearchFilter result = search.buildSearchFilter(request, pageable, "Mobi", userDetailsService);
    Assert.assertTrue(result.getAndParams().containsKey("tenantsAuth"));

    // Second request: dataTable requested is named sensorTypeTable
    result = search.buildSearchFilter(request, pageable, "Mobi", userDetailsService);
    Assert.assertFalse(result.getAndParams().containsKey("tenantsAuth"));
  }

  @Test
  public void testRoleUserRequestUserList() {
    System.setProperty(SentiloConstants.SENTILO_MULTITENANT_PROP_KEY, Boolean.TRUE.toString());
    PowerMockito.mockStatic(SearchFilterUtils.class);
    when(SearchFilterUtils.getListColumns(request)).thenReturn(columns);
    when(request.getParameter("tableName")).thenReturn("userTable");
    when(userDetails.isSuperAdminUser()).thenReturn(true).thenReturn(false);

    // First request: user is super-admin
    SearchFilter result = search.buildSearchFilter(request, pageable, "Mobi", userDetailsService);
    Assert.assertFalse(result.getAndParams().containsKey("tenantsAuth"));

    // Second request: user isn't super-admin
    result = search.buildSearchFilter(request, pageable, "Mobi", userDetailsService);
    Assert.assertTrue(result.getAndParams().containsKey("tenantsAuth"));
  }

  @Test
  public void buildMapSearchFilter() {
    System.setProperty(SentiloConstants.SENTILO_MULTITENANT_PROP_KEY, Boolean.TRUE.toString());

    final SearchFilter filter = search.buildMapSearchFilter();

    Assert.assertTrue(filter.paramsIsEmpty() && !filter.andParamsIsEmpty());
    Assert.assertEquals(mockTenant, filter.getAndParams().get("tenantsMapVisible"));
    Assert.assertEquals(mockTenant, filter.getAndParams().get("tenantsAuth"));

  }

  @Test
  public void buildMapSearchFilterNoMultitenant() {
    System.setProperty(SentiloConstants.SENTILO_MULTITENANT_PROP_KEY, Boolean.FALSE.toString());

    final SearchFilter filter = search.buildMapSearchFilter();

    Assert.assertTrue(filter.paramsIsEmpty() && filter.andParamsIsEmpty());
  }

}
