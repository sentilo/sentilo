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
package org.sentilo.web.catalog.admin.scheduler;

import java.util.Arrays;

import org.sentilo.web.catalog.admin.service.FederatedSynchronizationService;
import org.sentilo.web.catalog.domain.User;
import org.sentilo.web.catalog.security.CatalogUserDetails;
import org.sentilo.web.catalog.security.Role;
import org.sentilo.web.catalog.security.SecurityUtils;
import org.sentilo.web.catalog.utils.Constants;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;

/**
 * This job defines the process to import and synchronize into the catalog the resources from
 * federated Sentilo services.
 */
@Component
public class FederatedSynchronizationJob {

  final static int ONE_MINUTE = 60 * 1000; // 1 minute
  final static int FIXED_DELAY = 10 * ONE_MINUTE; // 10 minutes

  @Autowired
  private FederatedSynchronizationService service;

  @Scheduled(initialDelay = ONE_MINUTE, fixedDelay = FIXED_DELAY)
  public void syncFederatedServices() {
    if (SecurityUtils.isFederationEnabled()) {
      setUpContext();
      service.syncCatalogs();
    }
  }

  private void setUpContext() {
    registerAuthenticateUser();
  }

  /**
   * As some internal methods rely on fetching the SecurityContext and check that user is
   * authenticated (for audit actions purposes), we set a custom user to identify a batch process
   */
  private void registerAuthenticateUser() {
    final Role[] roles = {Role.ADMIN};
    final User user = new User(Constants.BATCH_USER);
    user.setActive(true);
    user.setRoles(Arrays.asList(roles));
    final CatalogUserDetails cud = new CatalogUserDetails(user);

    final UsernamePasswordAuthenticationToken result = new UsernamePasswordAuthenticationToken(cud, cud.getPassword(), cud.getAuthorities());
    SecurityContextHolder.getContext().setAuthentication(result);
  }

}
