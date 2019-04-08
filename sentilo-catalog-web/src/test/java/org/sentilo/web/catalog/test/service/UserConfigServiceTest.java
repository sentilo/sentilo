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
package org.sentilo.web.catalog.test.service;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.config.SentiloArtifactConfigService;
import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.web.catalog.context.TenantContextHolder;
import org.sentilo.web.catalog.context.TenantContextImpl;
import org.sentilo.web.catalog.context.UserConfigContext;
import org.sentilo.web.catalog.context.UserConfigContextHolder;
import org.sentilo.web.catalog.domain.Tenant;
import org.sentilo.web.catalog.domain.User;
import org.sentilo.web.catalog.domain.VisualConfiguration;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.search.SearchFilterResult;
import org.sentilo.web.catalog.security.CatalogUserDetails;
import org.sentilo.web.catalog.security.Role;
import org.sentilo.web.catalog.service.TenantService;
import org.sentilo.web.catalog.service.UserService;
import org.sentilo.web.catalog.service.impl.UserConfigServiceImpl;
import org.sentilo.web.catalog.utils.Constants;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;

public class UserConfigServiceTest {

  private final static String USER_TIME_ZONE = "GMT+04:00";
  private final static String USER_DATE_FORMAT_PATTERN = "dd/MM/yyyy HH:mm";
  private final static Integer USER_CHART_NUM_OBS = 12;

  private final static String TENANT_TIME_ZONE = "CET";
  private final static String TENANT_DATE_FORMAT_PATTERN = "yy/MM/dd HH:mm:ss";
  private final static Integer TENANT_CHART_NUM_OBS = 19;

  private final static String INSTANCE_TIME_ZONE = "GMT-04:00";
  private final static String INSTANCE_DATE_FORMAT_PATTERN = "dd.MM.yy";
  private final static Integer INSTANCE_CHART_NUM_POINTS = 15;

  private final static String DEFAULT_TIME_ZONE = Constants.DEFAULT_TIME_ZONE;
  private final static String DEFAULT_DATE_FORMAT_PATTERN = SentiloConstants.TIMESTAMP_PATTERN;
  private final static Integer DEFAULT_CHART_NUM_OBS = Constants.DEFAULT_CHART_POINTS_NUMBER;

  private final static String defaulTenantId = "defaulTenantId";

  private final static String currentUserId = "userId";
  private final static String currentTenantId = "currentTenantId";

  @Mock
  private UserService userService;

  @Mock
  private TenantService tenantService;

  @Mock
  private SentiloArtifactConfigService configService;

  @Mock
  private VisualConfiguration tenantVisualConfiguration;

  @InjectMocks
  private UserConfigServiceImpl userConfigService;

  private User currentUser;

  @Mock
  private Tenant defaultTenant;

  @Mock
  private Tenant currentTenant;

  @Mock
  private SearchFilterResult<Tenant> searchFilterResult;

  @Mock
  private Authentication authentication;

  @Before
  public void setUp() {
    System.setProperty(SentiloConstants.SENTILO_MULTITENANT_PROP_KEY, Boolean.TRUE.toString());
    MockitoAnnotations.initMocks(this);

    TenantContextHolder.setContext(new TenantContextImpl(currentTenantId));

    // Instance config params are setted
    when(configService.getConfigValue(eq("catalog.default.timezone"), any(String.class))).thenReturn(INSTANCE_TIME_ZONE);
    when(configService.getConfigValue(eq("catalog.default.datePattern"), any(String.class))).thenReturn(INSTANCE_DATE_FORMAT_PATTERN);
    when(configService.getConfigValue(eq("catalog.default.chart.numPoints"), eq(Integer.class), any(Integer.class)))
        .thenReturn(INSTANCE_CHART_NUM_POINTS);

    // Mocked Current Tenant
    when(currentTenant.getId()).thenReturn(currentTenantId);
    when(tenantService.find(any(Tenant.class))).thenReturn(currentTenant);

    // Mocked Default Tenant
    when(defaultTenant.getId()).thenReturn(defaulTenantId);

    // Tenant Visual Configuration
    when(currentTenant.getVisualConfiguration()).thenReturn(tenantVisualConfiguration);

    // Set user default values
    currentUser = new User(currentUserId);
    currentUser.getVisualConfiguration().setTimeZone(USER_TIME_ZONE);
    currentUser.getVisualConfiguration().setDateFormatPattern(USER_DATE_FORMAT_PATTERN);
    currentUser.getVisualConfiguration().setChartVisiblePointsNumber(USER_CHART_NUM_OBS);
    currentUser.setTenantId(currentTenantId);
    currentUser.setActive(true);
    currentUser.setRoles(new ArrayList<Role>());
  }

  @After
  public void tearDown() {
    System.clearProperty(SentiloConstants.SENTILO_MULTITENANT_PROP_KEY);
    TenantContextHolder.clearContext();
    SecurityContextHolder.clearContext();
  }

  @Test
  public void getUserConfigContextNullTest() {
    UserConfigContextHolder.clearContext();
    final UserConfigContext context = UserConfigContextHolder.getContext();
    Assert.assertNull(context);
  }

  @Test
  public void refreshCatalogUserConfigContextWithDefaultValues() {

    UserConfigContextHolder.clearContext();
    when(configService.getConfigValue("catalog.default.chart.numPoints", Integer.class, DEFAULT_CHART_NUM_OBS)).thenReturn(DEFAULT_CHART_NUM_OBS);
    userConfigService.refreshCatalogUserConfigContext();
    final UserConfigContext context = UserConfigContextHolder.getContext();
    Assert.assertNotNull(context);
    Assert.assertEquals(INSTANCE_DATE_FORMAT_PATTERN, context.getUserDatePattern());
    Assert.assertEquals(INSTANCE_TIME_ZONE, context.getUserTimeZone().getID());
    Assert.assertEquals(DEFAULT_CHART_NUM_OBS, context.getChartVisiblePointsNumber());
  }

  @Test
  public void refreshCatalogAnonymousUserConfigContext() {

    // Test that if user isn't logged in then instance values are used
    SecurityContextHolder.clearContext();
    UserConfigContextHolder.clearContext();

    userConfigService.refreshCatalogUserConfigContext();

    final UserConfigContext context = UserConfigContextHolder.getContext();
    Assert.assertNotNull(context);
    Assert.assertEquals(INSTANCE_DATE_FORMAT_PATTERN, context.getUserDatePattern());
    Assert.assertEquals(INSTANCE_TIME_ZONE, context.getUserTimeZone().getID());
    Assert.assertEquals(INSTANCE_CHART_NUM_POINTS, context.getChartVisiblePointsNumber());
  }

  @Test
  public void refreshCatalogUnknownUserConfigContext() {

    // Test that there is an authentication context but not user details are setted, and no tenant,
    // so get data from instance

    UserConfigContextHolder.clearContext();

    final String catalogUserDetails = "unknown";
    final Authentication authentication = Mockito.mock(Authentication.class);
    final SecurityContext securityContext = Mockito.mock(SecurityContext.class);
    when(securityContext.getAuthentication()).thenReturn(authentication);
    SecurityContextHolder.setContext(securityContext);
    when(authentication.getPrincipal()).thenReturn(catalogUserDetails);

    userConfigService.refreshCatalogUserConfigContext();

    final UserConfigContext context = UserConfigContextHolder.getContext();
    Assert.assertNotNull(context);
    Assert.assertEquals(INSTANCE_DATE_FORMAT_PATTERN, context.getUserDatePattern());
    Assert.assertEquals(INSTANCE_TIME_ZONE, context.getUserTimeZone().getID());
    Assert.assertEquals(INSTANCE_CHART_NUM_POINTS, context.getChartVisiblePointsNumber());
  }

  @Test
  public void getUserConfigNoUserNoTenantNoInstanceFromDefaultTest() {

    // Case 1:
    // * user not logged in (no security context is setted for this test)
    // * tenant params not configured (no tenant context is setted for this test)
    // * then get config params from default values (sentilo catalog constants)

    UserConfigContextHolder.clearContext();
    when(configService.getConfigValue("catalog.default.timezone", DEFAULT_TIME_ZONE)).thenReturn(DEFAULT_TIME_ZONE);
    when(configService.getConfigValue("catalog.default.datePattern", DEFAULT_DATE_FORMAT_PATTERN)).thenReturn(DEFAULT_DATE_FORMAT_PATTERN);
    when(configService.getConfigValue("catalog.default.chart.numPoints", Integer.class, DEFAULT_CHART_NUM_OBS)).thenReturn(DEFAULT_CHART_NUM_OBS);
    when(tenantService.search(any(SearchFilter.class))).thenReturn(searchFilterResult);
    when(searchFilterResult.getContent()).thenReturn(Arrays.asList(defaultTenant));
    when(tenantService.find(any(Tenant.class))).thenReturn(null);

    userConfigService.refreshCatalogUserConfigContext();

    final UserConfigContext context = UserConfigContextHolder.getContext();
    Assert.assertNotNull(context);
    Assert.assertEquals(DEFAULT_DATE_FORMAT_PATTERN, context.getUserDatePattern());
    Assert.assertEquals(DEFAULT_TIME_ZONE, context.getUserTimeZone().getID());
    Assert.assertEquals(DEFAULT_CHART_NUM_OBS, context.getChartVisiblePointsNumber());
  }

  @Test
  public void getUserConfigNoUserNoTenantFromInstanceTest() {

    // Case 2:
    // * user not logged in (no security context is setted for this test)
    // * tenant params not configured (no tenant context is setted for this test)
    // * instance params are setted (mock values setted to instance values)
    // * then get config params from instance params values

    UserConfigContextHolder.clearContext();

    when(tenantService.search(any(SearchFilter.class))).thenReturn(searchFilterResult);
    when(searchFilterResult.getContent()).thenReturn(Arrays.asList(defaultTenant));
    when(tenantService.find(any(Tenant.class))).thenReturn(null);

    userConfigService.refreshCatalogUserConfigContext();

    final UserConfigContext context = UserConfigContextHolder.getContext();
    Assert.assertNotNull(context);
    Assert.assertEquals(INSTANCE_DATE_FORMAT_PATTERN, context.getUserDatePattern());
    Assert.assertEquals(INSTANCE_TIME_ZONE, context.getUserTimeZone().getID());
    Assert.assertEquals(INSTANCE_CHART_NUM_POINTS, context.getChartVisiblePointsNumber());
  }

  @Test
  public void getUserConfigNoUserFromTenantTest() {

    // Case 3:
    // * user not logged in (no security context is setted for this test)
    // * tenant params are configured (tenant context is setted for this test)
    // * then get config params from tenant configuration
    UserConfigContextHolder.clearContext();
    when(tenantVisualConfiguration.getChartVisiblePointsNumber()).thenReturn(TENANT_CHART_NUM_OBS);
    when(tenantVisualConfiguration.getDateFormatPattern()).thenReturn(TENANT_DATE_FORMAT_PATTERN);
    when(tenantVisualConfiguration.getTimeZone()).thenReturn(TENANT_TIME_ZONE);

    when(userService.find(any(User.class))).thenReturn(null);

    userConfigService.refreshCatalogUserConfigContext();

    final UserConfigContext context = UserConfigContextHolder.getContext();

    Assert.assertNotNull(context);
    Assert.assertEquals(TENANT_DATE_FORMAT_PATTERN, context.getUserDatePattern());
    Assert.assertEquals(TENANT_TIME_ZONE, context.getUserTimeZone().getID());
    Assert.assertEquals(TENANT_CHART_NUM_OBS, context.getChartVisiblePointsNumber());
  }

  @Test
  public void getUserConfigNoUserConfigValuesFromTenantTest() {

    // Case 3:
    // * user not logged in (no security context is setted for this test)
    // * tenant params are configured (tenant context is setted for this test)
    // * then get config params from tenant configuration
    UserConfigContextHolder.clearContext();
    when(tenantVisualConfiguration.getChartVisiblePointsNumber()).thenReturn(TENANT_CHART_NUM_OBS);
    when(tenantVisualConfiguration.getDateFormatPattern()).thenReturn(TENANT_DATE_FORMAT_PATTERN);
    when(tenantVisualConfiguration.getTimeZone()).thenReturn(TENANT_TIME_ZONE);

    // No user
    currentUser = null;
    userConfigService.refreshCatalogUserConfigContext();
    UserConfigContext context = UserConfigContextHolder.getContext();
    Assert.assertNotNull(context);
    Assert.assertEquals(TENANT_DATE_FORMAT_PATTERN, context.getUserDatePattern());
    Assert.assertEquals(TENANT_TIME_ZONE, context.getUserTimeZone().getID());
    Assert.assertEquals(TENANT_CHART_NUM_OBS, context.getChartVisiblePointsNumber());

    // No user visual configuration
    currentUser = new User(currentUserId);
    currentUser.setVisualConfiguration(null);
    userConfigService.refreshCatalogUserConfigContext();
    context = UserConfigContextHolder.getContext();
    Assert.assertNotNull(context);
    Assert.assertEquals(TENANT_DATE_FORMAT_PATTERN, context.getUserDatePattern());
    Assert.assertEquals(TENANT_TIME_ZONE, context.getUserTimeZone().getID());
    Assert.assertEquals(TENANT_CHART_NUM_OBS, context.getChartVisiblePointsNumber());

    // No user visual configuration->
    currentUser.setVisualConfiguration(new VisualConfiguration(null, null, null));
    userConfigService.refreshCatalogUserConfigContext();
    context = UserConfigContextHolder.getContext();
    Assert.assertNotNull(context);
    Assert.assertEquals(TENANT_DATE_FORMAT_PATTERN, context.getUserDatePattern());
    Assert.assertEquals(TENANT_TIME_ZONE, context.getUserTimeZone().getID());
    Assert.assertEquals(TENANT_CHART_NUM_OBS, context.getChartVisiblePointsNumber());
  }

  @Test
  public void getUserConfigFromUserFromTest() {

    // Case 4:
    // * user is logged in (security context is setted for this test)
    // * then get config params from user configuration

    final CatalogUserDetails catalogUserDetails = new CatalogUserDetails(currentUser);
    final Authentication authentication = Mockito.mock(Authentication.class);
    final SecurityContext securityContext = Mockito.mock(SecurityContext.class);
    when(securityContext.getAuthentication()).thenReturn(authentication);
    SecurityContextHolder.setContext(securityContext);
    when(authentication.getPrincipal()).thenReturn(catalogUserDetails);

    UserConfigContextHolder.clearContext();

    when(userService.find(new User(currentUserId))).thenReturn(currentUser);

    userConfigService.refreshCatalogUserConfigContext();

    final UserConfigContext context = UserConfigContextHolder.getContext();
    Assert.assertNotNull(context);
    Assert.assertEquals(currentUser.getVisualConfiguration().getDateFormatPattern(), context.getUserDatePattern());
    Assert.assertEquals(currentUser.getVisualConfiguration().getTimeZone(), context.getUserTimeZone().getID());
    Assert.assertEquals(currentUser.getVisualConfiguration().getChartVisiblePointsNumber(), context.getChartVisiblePointsNumber());
  }

  @Test
  public void clearCatalogUserConfigContextTest() {
    userConfigService.clearCatalogUserConfigContext();
    final UserConfigContext context = UserConfigContextHolder.getContext();
    Assert.assertNull(context);
  }
}
