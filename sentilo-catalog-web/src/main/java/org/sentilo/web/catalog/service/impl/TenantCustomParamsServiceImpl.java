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

import org.sentilo.web.catalog.domain.MapParams;
import org.sentilo.web.catalog.domain.Tenant;
import org.sentilo.web.catalog.dto.TenantCustomParamsDTO;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.service.TenantCustomParamsService;
import org.sentilo.web.catalog.service.TenantService;
import org.sentilo.web.catalog.utils.TenantUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

@Service
public class TenantCustomParamsServiceImpl implements TenantCustomParamsService {

  @Autowired
  private TenantService tenantService;

  @Override
  public TenantCustomParamsDTO getTenantCustomParams() {
    // To get the parameters to apply to the displayed site there are two options: either apply the
    // tenant map configuration if it exists or apply the default tenant configuration
    final Tenant tenant = getRequestTenant();
    final MapParams mapParams = buildMapParams(tenant);

    return new TenantCustomParamsDTO(tenant, getTenantStyleClassUrl(tenant), mapParams);
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

  private MapParams buildMapParams(final Tenant tenant) {
    final MapParams mapParams = tenant.getMapParams();
    if (mapParams.isEmpty() && !tenant.getIsDefault()) {
      final MapParams defaultMapParams = getDefaultTenant().getMapParams();
      mapParams.setZoomLevel(mapParams.getZoomLevel() > 0 ? mapParams.getZoomLevel() : defaultMapParams.getZoomLevel());
      mapParams.setBgColor(StringUtils.hasText(mapParams.getBgColor()) ? mapParams.getBgColor() : defaultMapParams.getBgColor());
      mapParams.setCenter(!mapParams.centerIsEmpty() ? mapParams.getCenter() : defaultMapParams.getCenter());
    }

    return mapParams;
  }

  private String getTenantStyleClassUrl(final Tenant tenant) {
    return "tenant/" + tenant.getId() + "/css/" + tenant.getId() + ".css";
  }

}
