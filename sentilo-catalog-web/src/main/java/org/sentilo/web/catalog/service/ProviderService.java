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
package org.sentilo.web.catalog.service;

import java.util.List;
import java.util.Map;
import java.util.Set;

import org.sentilo.web.catalog.domain.Provider;
import org.sentilo.web.catalog.domain.SectorResource.GrantType;

public interface ProviderService extends CrudService<Provider> {

  void deleteChildren(Provider provider);

  /**
   * Return a list with either all the providers or the ones authorized to the current tenant if
   * multitenant feature is enabled.
   *
   * @return
   */
  List<Provider> findAllowed();

  void addGrantedTenant(String providerId, String tenantId);

  void removeGrantedTenant(String providerId, String tenantId);

  Set<String> getGrantedTenantsIds(String providerId);

  void deleteFromTenant(String tenantId);

  boolean isProviderFromTenant(String providerId, String tenantId);

  void deleteFederatedResources(String federatedConfigId);

  void addProvidersToSector(final String sectorId, final Map<String, GrantType> providersGrants);

  void removeProvidersFromSector(final String sectorId, final List<String> providersIds);

}
