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
package org.sentilo.web.catalog.service.impl;

import java.util.TimeZone;

import org.sentilo.common.config.SentiloArtifactConfigService;
import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.web.catalog.context.UserConfigContextHolder;
import org.sentilo.web.catalog.context.UserConfigContextImpl;
import org.sentilo.web.catalog.domain.Tenant;
import org.sentilo.web.catalog.domain.User;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.security.CatalogUserDetails;
import org.sentilo.web.catalog.service.TenantService;
import org.sentilo.web.catalog.service.UserConfigService;
import org.sentilo.web.catalog.service.UserService;
import org.sentilo.web.catalog.utils.Constants;
import org.sentilo.web.catalog.utils.TenantUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

@Component
public class UserConfigServiceImpl implements UserConfigService {

  private static final Logger LOGGER = LoggerFactory.getLogger(UserConfigServiceImpl.class);

  @Autowired
  private SentiloArtifactConfigService configService;

  @Autowired
  private UserService userService;

  @Autowired
  private TenantService tenantService;

  @Override
  public void refreshCatalogUserConfigContext() {

    /*
     * Load user config params (TZ ,date format pattern and number of points to display in charts).
     * Algorithm to load these parameters follows the following hierarchy:
     *
     * - first try to get them from user's profile - next try to get them from current tenant's
     * profile - else, try to get them from instance configuration - and finally, get their default
     * values (UTC, dd/MM/yyyy'T'HH:mm:ss, 10)
     */

    User user = null;

    // Get tenant
    final Tenant tenant = getRequestTenant();

    // Get the instance config values: these are the default values to set
    final String instanceTimeZone = configService.getConfigValue("catalog.default.timezone", Constants.DEFAULT_TIME_ZONE);
    final String instanceDateFormatPattern = configService.getConfigValue("catalog.default.datePattern", SentiloConstants.TIMESTAMP_PATTERN);
    final Integer instanceChartNumPoints =
        configService.getConfigValue("catalog.default.chart.numPoints", Integer.class, Constants.DEFAULT_CHART_POINTS_NUMBER);

    // Test if user is logged and get its info
    if (SecurityContextHolder.getContext().getAuthentication() != null
        && SecurityContextHolder.getContext().getAuthentication().getPrincipal() instanceof CatalogUserDetails) {
      // User is logged, get its principal
      final CatalogUserDetails catalogUser = (CatalogUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
      user = userService.find(new User(catalogUser.getUsername()));
    }

    // Finally create the correct config values
    final String timeZone = getTimeZone(user, tenant, instanceTimeZone);
    final String dateFormatPattern = getDateFormatPatter(user, tenant, instanceDateFormatPattern);
    final Integer chartNumPoints = getChartVisiblePointsNumber(user, tenant, instanceChartNumPoints);

    // Create / Update the user config context
    final UserConfigContextImpl userConfigContext = new UserConfigContextImpl(TimeZone.getTimeZone(timeZone), dateFormatPattern, chartNumPoints);
    UserConfigContextHolder.setContext(userConfigContext);

    // All the user config params are mandatory
    assertCorrectConfigParamsValues(userConfigContext);

    LOGGER.debug("Refreshing catalog user config params: {}", userConfigContext);
  }

  @Override
  public void clearCatalogUserConfigContext() {
    UserConfigContextHolder.clearContext();
  }

  private void assertCorrectConfigParamsValues(final UserConfigContextImpl userConfigContext) {
    Assert.notNull(userConfigContext.getUserTimeZone(), "[UserConfigService] userTimeZone config param is empty");
    Assert.hasText(userConfigContext.getUserDatePattern(), "[UserConfigService] userDatePattern config param is empty");
    Assert.isTrue(userConfigContext.getChartVisiblePointsNumber() != null && userConfigContext.getChartVisiblePointsNumber() > 0,
        "[UserConfigService] userChartNumObs config param is not a positive number greater than 0");
  }

  private Tenant getRequestTenant() {
    Tenant tenant = null;
    if (StringUtils.hasText(TenantUtils.getRequestTenant())) {
      final String requestTenant = TenantUtils.getRequestTenant();
      tenant = tenantService.find(new Tenant(requestTenant));
    }
    return tenant != null ? tenant : getDefaultTenant();
  }

  private Tenant getDefaultTenant() {
    final SearchFilter filter = new SearchFilter();
    filter.addAndParam("isDefault", Boolean.TRUE);
    return tenantService.search(filter).getContent().get(0);
  }

  private String getTimeZone(final User user, final Tenant tenant, final String instanceTimeZone) {
    if (StringUtils.hasText(getUserTimeZone(user))) {
      return getUserTimeZone(user);
    } else if (tenant != null && tenant.getVisualConfiguration() != null && StringUtils.hasText(tenant.getVisualConfiguration().getTimeZone())) {
      return tenant.getVisualConfiguration().getTimeZone();
    } else {
      return instanceTimeZone;
    }
  }

  private String getDateFormatPatter(final User user, final Tenant tenant, final String instanceDateFormatPattern) {
    if (StringUtils.hasText(getUserDateFormatPattern(user))) {
      return getUserDateFormatPattern(user);
    } else if (tenant != null && tenant.getVisualConfiguration() != null
        && StringUtils.hasText(tenant.getVisualConfiguration().getDateFormatPattern())) {
      return tenant.getVisualConfiguration().getDateFormatPattern();
    } else {
      return instanceDateFormatPattern;
    }
  }

  private Integer getChartVisiblePointsNumber(final User user, final Tenant tenant, final Integer instanceChartNumPoints) {
    if (getUserChartVisiblePointsNumber(user) != null) {
      return getUserChartVisiblePointsNumber(user);
    } else if (tenant != null && tenant.getVisualConfiguration() != null && tenant.getVisualConfiguration().getChartVisiblePointsNumber() != null
        && tenant.getVisualConfiguration().getChartVisiblePointsNumber() > 0) {
      return tenant.getVisualConfiguration().getChartVisiblePointsNumber();
    } else {
      return instanceChartNumPoints;
    }
  }

  private String getUserTimeZone(final User user) {
    if (user != null && user.getVisualConfiguration() != null && StringUtils.hasText(user.getVisualConfiguration().getTimeZone())) {
      return user.getVisualConfiguration().getTimeZone();
    }
    return null;
  }

  private String getUserDateFormatPattern(final User user) {
    if (user != null && user.getVisualConfiguration() != null && StringUtils.hasText(user.getVisualConfiguration().getDateFormatPattern())) {
      return user.getVisualConfiguration().getDateFormatPattern();
    }
    return null;
  }

  private Integer getUserChartVisiblePointsNumber(final User user) {
    if (user != null && user.getVisualConfiguration() != null && user.getVisualConfiguration().getChartVisiblePointsNumber() != null) {
      return user.getVisualConfiguration().getChartVisiblePointsNumber();
    }
    return null;
  }

}
