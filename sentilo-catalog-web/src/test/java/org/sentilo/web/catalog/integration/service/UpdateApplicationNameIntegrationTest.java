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
package org.sentilo.web.catalog.integration.service;

import org.junit.runner.RunWith;
import org.sentilo.web.catalog.domain.Application;
import org.sentilo.web.catalog.domain.Permission;
import org.sentilo.web.catalog.domain.Provider;
import org.sentilo.web.catalog.service.ApplicationService;
import org.sentilo.web.catalog.service.PermissionService;
import org.sentilo.web.catalog.service.ProviderService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = "classpath:spring/test-mongodb-service-context.xml")
public class UpdateApplicationNameIntegrationTest {

  @Autowired
  private PermissionService permissionService;

  @Autowired
  private ApplicationService applicationService;

  @Autowired
  private ProviderService providerService;

  private final static Permission.Type type = Permission.Type.WRITE;

  /**
   * Este es el procedimiento para actualizar el nombre de la aplicación.
   *
   *
   */
  // @Test
  public void updateApplicationName() {

    final String oldName = "connecta-catalog";
    final String newName = "connecta-catalog";

    removeAllPermissions(oldName);
    addCatalogPermissions(newName);
    addPermissionsToAllApplications(newName);
    addPermissionsToAllProviders(newName);
  }

  private void addCatalogPermissions(final String source) {
    permissionService.create(new Permission(source));
  }

  private void addPermissionsToAllApplications(final String source) {
    for (final Application application : applicationService.findAll()) {
      if (notSameApplication(source, application)) {
        final Permission permission = new Permission(source, application.getId(), type);
        permissionService.create(permission);
      }
    }
  }

  private boolean notSameApplication(final String source, final Application application) {
    return !source.equals(application.getId());
  }

  private void addPermissionsToAllProviders(final String source) {
    for (final Provider provider : providerService.findAll()) {
      final Permission permission = new Permission(source, provider.getId(), type);
      permissionService.create(permission);
    }
  }

  private void removeAllPermissions(final String source) {
    for (final Permission permission : permissionService.findAll()) {
      if (source.equals(permission.getSource())) {
        permissionService.delete(permission);
      }
    }
  }
}
