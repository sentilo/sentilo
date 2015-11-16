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
package org.sentilo.web.catalog.test.service;

import static org.mockito.Matchers.any;
import static org.mockito.Mockito.when;

import java.util.Arrays;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.web.catalog.context.TenantContextHolder;
import org.sentilo.web.catalog.context.TenantContextImpl;
import org.sentilo.web.catalog.domain.LngLat;
import org.sentilo.web.catalog.domain.MapParams;
import org.sentilo.web.catalog.domain.Tenant;
import org.sentilo.web.catalog.dto.TenantCustomParamsDTO;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.search.SearchFilterResult;
import org.sentilo.web.catalog.service.TenantService;
import org.sentilo.web.catalog.service.impl.TenantCustomParamsServiceImpl;

public class TenantCustomParamsServiceImplTest {

  @InjectMocks
  private TenantCustomParamsServiceImpl service;

  @Mock
  private TenantService tenantService;

  @Mock
  private Tenant tenant;

  @Mock
  private Tenant defaultTenant;

  @Mock
  private SearchFilterResult<Tenant> searchResult;

  @Mock
  private MapParams mapParams;
  @Mock
  private LngLat lngLat;

  final String defaultTenantId = "tenantId";
  final String defaultStyleClass = "defaultCSS";

  @Before
  public void setUp() {
    System.setProperty(SentiloConstants.SENTILO_MULTITENANT_PROP_KEY, Boolean.TRUE.toString());
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void testGetTenantCustomParamsWithNullTenantId() {
    TenantContextHolder.setContext(new TenantContextImpl(null));
    when(tenantService.search(any(SearchFilter.class))).thenReturn(searchResult);
    when(searchResult.getContent()).thenReturn(Arrays.asList(new Tenant[] {defaultTenant}));
    when(defaultTenant.getId()).thenReturn("defaultId");

    final TenantCustomParamsDTO customParams = service.getTenantCustomParams();

    Assert.assertTrue(customParams.getTenantId().equals("defaultId"));
    Assert.assertTrue(customParams.getStyleClass().equals("defaultId.css"));
  }

  @Test
  public void testGetTenantCustomParamsWithNonExistantTenantId() {
    TenantContextHolder.setContext(new TenantContextImpl("mockTenant"));
    when(tenantService.find(any(Tenant.class))).thenReturn(null);
    when(tenantService.search(any(SearchFilter.class))).thenReturn(searchResult);
    when(searchResult.getContent()).thenReturn(Arrays.asList(new Tenant[] {defaultTenant}));
    when(defaultTenant.getId()).thenReturn("defaultId");

    final TenantCustomParamsDTO customParams = service.getTenantCustomParams();

    Assert.assertTrue(customParams.getTenantId().equals("defaultId"));
    Assert.assertTrue(customParams.getStyleClass().equals("defaultId.css"));
  }

  @Test
  public void testGetTenantCustomParamsWithExistantTenantId() {
    TenantContextHolder.setContext(new TenantContextImpl(defaultTenantId));
    final double tenantMapCenterLatitude = 41.29;
    final double tenantMapCenterLongitude = 2.44;
    final int tenantMapZoomLevel = 15;

    when(tenant.getId()).thenReturn(defaultTenantId);
    when(lngLat.getLatitude()).thenReturn(tenantMapCenterLatitude);
    when(lngLat.getLongitude()).thenReturn(tenantMapCenterLongitude);
    when(mapParams.getCenter()).thenReturn(lngLat);
    when(mapParams.getZoomLevel()).thenReturn(tenantMapZoomLevel);
    when(tenant.getMapParams()).thenReturn(mapParams);
    when(tenantService.search(any(SearchFilter.class))).thenReturn(searchResult);
    when(searchResult.getContent()).thenReturn(Arrays.asList(new Tenant[] {tenant}));

    final TenantCustomParamsDTO customParams = service.getTenantCustomParams();

    Assert.assertEquals(defaultTenantId, customParams.getTenantId());
    Assert.assertEquals(new Double(tenantMapCenterLatitude), customParams.getMapParams().getCenter().getLatitude());
    Assert.assertEquals(new Double(tenantMapCenterLongitude), customParams.getMapParams().getCenter().getLongitude());
    Assert.assertEquals(tenantMapZoomLevel, customParams.getMapParams().getZoomLevel());
    Assert.assertEquals(defaultTenantId + ".css", customParams.getStyleClass());
  }
}
