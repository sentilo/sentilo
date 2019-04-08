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
package org.sentilo.web.catalog.security.access.impl;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import org.sentilo.web.catalog.context.TenantContextHolder;
import org.sentilo.web.catalog.domain.ActiveSubscription;
import org.sentilo.web.catalog.domain.Alert;
import org.sentilo.web.catalog.domain.AlertRule;
import org.sentilo.web.catalog.domain.Application;
import org.sentilo.web.catalog.domain.Component;
import org.sentilo.web.catalog.domain.ComponentType;
import org.sentilo.web.catalog.domain.DocumentFile;
import org.sentilo.web.catalog.domain.FederationConfig;
import org.sentilo.web.catalog.domain.Provider;
import org.sentilo.web.catalog.domain.Sensor;
import org.sentilo.web.catalog.domain.SensorType;
import org.sentilo.web.catalog.domain.Tenant;
import org.sentilo.web.catalog.domain.TenantResource;
import org.sentilo.web.catalog.domain.User;
import org.sentilo.web.catalog.security.SecurityUtils;
import org.sentilo.web.catalog.security.access.AccessControlRepository;
import org.sentilo.web.catalog.security.access.ActionGrant;
import org.springframework.stereotype.Repository;

@Repository
public class AccessControlRepositoryImpl implements AccessControlRepository {

  private final Map<String, ActionGrant[]> acls = new HashMap<String, ActionGrant[]>();

  @SuppressWarnings("rawtypes")
  private final Class[] simpleTenantResources = {Provider.class, Application.class, Component.class, Sensor.class};

  public AccessControlRepositoryImpl() {
    init();
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.security.access.AccessControlRepository#getGrants(java.lang.Class,
   * java.lang.String)
   */
  @Override
  public ActionGrant[] getGrants(final Class<?> resourceClass, final String userRole) {
    final String resourceName = Arrays.asList(simpleTenantResources).contains(resourceClass) ? "TenantResource" : resourceClass.getSimpleName();
    final String accessRuleKey = buildAccessRuleKey(resourceName, userRole);
    return acls.get(accessRuleKey);
  }

  private void init() {
    if (TenantContextHolder.isEnabled()) {
      addMultitenantGrants();
    } else {
      addGrants();
    }
  }

  private void addMultitenantGrants() {
    addRoleActionGrants(Tenant.class.getSimpleName(), "SA", "L2,C2,R2,A2,D2");
    addRoleActionGrants(Tenant.class.getSimpleName(), "A", "R0,A0");
    addRoleActionGrants(User.class.getSimpleName(), "SA", "L2,C2,R2,A2,D2");
    addRoleActionGrants(User.class.getSimpleName(), "A", "L2,C2,R0,A0,D0");
    addRoleActionGrants(User.class.getSimpleName(), "U", "R0,A0");
    addRoleActionGrants(TenantResource.class.getSimpleName(), "A", "L2,C2,R1,A0,D0");
    addRoleActionGrants(TenantResource.class.getSimpleName(), "U", "L2,R1");
    addRoleActionGrants(Alert.class.getSimpleName(), "A", "L2,C2,R0,A0,D0");
    addRoleActionGrants(Alert.class.getSimpleName(), "U", "L2,R0");
    addRoleActionGrants(DocumentFile.class.getSimpleName(), "A", "L2,C2,R0,A0,D0");
    addRoleActionGrants(DocumentFile.class.getSimpleName(), "U", "L2,R0");
    addRoleActionGrants(AlertRule.class.getSimpleName(), "A", "L2,C2,R0,A0,D0");
    addRoleActionGrants(AlertRule.class.getSimpleName(), "U", "L2,R0");
    addRoleActionGrants(SensorType.class.getSimpleName(), "SA", "L2,C2,R2,A2,D2");
    addRoleActionGrants(SensorType.class.getSimpleName(), "A", "L2,R2");
    addRoleActionGrants(SensorType.class.getSimpleName(), "U", "L2,R2");
    addRoleActionGrants(ComponentType.class.getSimpleName(), "SA", "L2,C2,R2,A2,D2");
    addRoleActionGrants(ComponentType.class.getSimpleName(), "A", "L2,R2");
    addRoleActionGrants(ComponentType.class.getSimpleName(), "U", "L2,R2");
    addRoleActionGrants(ActiveSubscription.class.getSimpleName(), "A", "L2,R2");
    addRoleActionGrants(ActiveSubscription.class.getSimpleName(), "U", "L2,R2");
    if (SecurityUtils.isFederationEnabled()) {
      addRoleActionGrants(FederationConfig.class.getSimpleName(), "A", "L2,C2,R0,A0,D0");
    }
  }

  private void addGrants() {
    addRoleActionGrants(Tenant.class.getSimpleName(), "A", "R2,A2");
    addRoleActionGrants(User.class.getSimpleName(), "A", "L2,C2,R2,A2,D2");
    addRoleActionGrants(User.class.getSimpleName(), "U", "R2,A2");
    addRoleActionGrants(TenantResource.class.getSimpleName(), "A", "L2,C2,R2,A2,D2");
    addRoleActionGrants(TenantResource.class.getSimpleName(), "U", "L2,R2");
    addRoleActionGrants(Alert.class.getSimpleName(), "A", "L2,C2,R2,A2,D2");
    addRoleActionGrants(Alert.class.getSimpleName(), "U", "L2,R2");
    addRoleActionGrants(DocumentFile.class.getSimpleName(), "A", "L2,C2,R2,A2,D2");
    addRoleActionGrants(DocumentFile.class.getSimpleName(), "U", "L2,R2");
    addRoleActionGrants(AlertRule.class.getSimpleName(), "A", "L2,C2,R2,A2,D2");
    addRoleActionGrants(AlertRule.class.getSimpleName(), "U", "L2,R2");
    addRoleActionGrants(SensorType.class.getSimpleName(), "A", "L2,C2,R2,A2,D2");
    addRoleActionGrants(SensorType.class.getSimpleName(), "U", "L2,R2");
    addRoleActionGrants(ComponentType.class.getSimpleName(), "A", "L2,C2,R2,A2,D2");
    addRoleActionGrants(ComponentType.class.getSimpleName(), "U", "L2,R2");
    addRoleActionGrants(ActiveSubscription.class.getSimpleName(), "A", "L2,R2");
    addRoleActionGrants(ActiveSubscription.class.getSimpleName(), "U", "L2,R2");
    if (SecurityUtils.isFederationEnabled()) {
      addRoleActionGrants(FederationConfig.class.getSimpleName(), "A", "L2,C2,R2,A2,D2");
    }
  }

  private void addRoleActionGrants(final String resource, final String userRole, final String groupedGrants) {
    // For each grant into groupedGrants it follows the format: actionType+maximum_tenant_link

    final String accessRuleKey = buildAccessRuleKey(resource, userRole);
    final String[] shortDefGrants = groupedGrants.split(",");

    final ActionGrant[] actionGrants = new ActionGrant[shortDefGrants.length];
    int pos = 0;
    for (final String shortDefGrant : shortDefGrants) {
      actionGrants[pos] = new ActionGrant(shortDefGrant);
      pos++;
    }

    addRoleActionGrants(accessRuleKey, actionGrants);
  }

  private void addRoleActionGrants(final String key, final ActionGrant... grants) {
    acls.put(key, grants);
  }

  private static String buildAccessRuleKey(final String resource, final String userRole) {
    return resource + "-" + userRole;
  }
}
