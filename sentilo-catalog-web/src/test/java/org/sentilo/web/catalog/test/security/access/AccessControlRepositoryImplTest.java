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
package org.sentilo.web.catalog.test.security.access;

import org.junit.After;
import org.junit.Assert;
import org.junit.Test;
import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.web.catalog.domain.Alert;
import org.sentilo.web.catalog.domain.ComponentType;
import org.sentilo.web.catalog.domain.SensorType;
import org.sentilo.web.catalog.domain.Tenant;
import org.sentilo.web.catalog.domain.TenantResource;
import org.sentilo.web.catalog.domain.User;
import org.sentilo.web.catalog.security.access.ActionGrant;
import org.sentilo.web.catalog.security.access.impl.AccessControlRepositoryImpl;

public class AccessControlRepositoryImplTest {

  private AccessControlRepositoryImpl accessControlRepository;

  @After
  public void tearDown() {
    System.clearProperty(SentiloConstants.SENTILO_MULTITENANT_PROP_KEY);
  }

  @Test
  public void getMultitenantGrants() {
    System.setProperty(SentiloConstants.SENTILO_MULTITENANT_PROP_KEY, Boolean.TRUE.toString());
    accessControlRepository = new AccessControlRepositoryImpl();

    verifyGrants(Tenant.class, "SA", "L2,C2,R2,A2,D2");
    verifyGrants(Tenant.class, "A", "R0,A0");
    verifyGrants(User.class, "SA", "L2,C2,R2,A2,D2");
    verifyGrants(User.class, "A", "L2,C2,R0,A0,D0");
    verifyGrants(TenantResource.class, "A", "L2,C2,R1,A0,D0");
    verifyGrants(TenantResource.class, "U", "L2,R1");
    verifyGrants(Alert.class, "A", "L2,C2,R0,A0,D0");
    verifyGrants(Alert.class, "U", "L2,R0");
    verifyGrants(SensorType.class, "SA", "L2,C2,R2,A2,D2");
    verifyGrants(SensorType.class, "A", "L2,R2");
    verifyGrants(SensorType.class, "U", "L2,R2");
    verifyGrants(ComponentType.class, "SA", "L2,C2,R2,A2,D2");
    verifyGrants(ComponentType.class, "A", "L2,R2");
    verifyGrants(ComponentType.class, "U", "L2,R2");
  }

  @Test
  public void getNoMultitenantGrants() {
    System.setProperty(SentiloConstants.SENTILO_MULTITENANT_PROP_KEY, Boolean.FALSE.toString());
    accessControlRepository = new AccessControlRepositoryImpl();

    verifyGrants(Tenant.class, "A", "R2,A2");
    verifyGrants(User.class, "A", "L2,C2,R2,A2,D2");
    verifyGrants(TenantResource.class, "A", "L2,C2,R2,A2,D2");
    verifyGrants(TenantResource.class, "U", "L2,R2");
    verifyGrants(Alert.class, "A", "L2,C2,R2,A2,D2");
    verifyGrants(Alert.class, "U", "L2,R2");
    verifyGrants(SensorType.class, "A", "L2,C2,R2,A2,D2");
    verifyGrants(SensorType.class, "U", "L2,R2");
    verifyGrants(ComponentType.class, "A", "L2,C2,R2,A2,D2");
    verifyGrants(ComponentType.class, "U", "L2,R2");
  }

  private void verifyGrants(final Class<?> resourceClass, final String userRole, final String... sGrants) {
    final ActionGrant[] grants = accessControlRepository.getGrants(resourceClass, userRole);
    for (final String sGrant : sGrants) {
      verifyGrant(grants, sGrant.charAt(0), Character.getNumericValue(sGrant.charAt(1)));
    }
  }

  private void verifyGrant(final ActionGrant[] grants, final char action, final int linkBetweenUserAndResourceTenant) {
    boolean allowed = false;
    for (final ActionGrant grant : grants) {
      allowed = allowed | grant.isActionAllowed(action, linkBetweenUserAndResourceTenant);
    }

    Assert.assertTrue(allowed);
  }

}
