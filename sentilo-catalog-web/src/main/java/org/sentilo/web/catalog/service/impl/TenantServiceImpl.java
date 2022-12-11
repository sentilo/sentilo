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

import java.util.Collection;
import java.util.List;

import org.sentilo.web.catalog.domain.Tenant;
import org.sentilo.web.catalog.repository.TenantRepository;
import org.sentilo.web.catalog.service.ApplicationService;
import org.sentilo.web.catalog.service.ProviderService;
import org.sentilo.web.catalog.service.SectorService;
import org.sentilo.web.catalog.service.TenantService;
import org.sentilo.web.catalog.service.UserService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.stereotype.Service;

@Service
public class TenantServiceImpl extends AbstractBaseCrudServiceImpl<Tenant> implements TenantService {

  @Autowired
  private TenantRepository repository;

  @Autowired
  private UserService userService;

  @Autowired
  private ProviderService providerService;

  @Autowired
  private ApplicationService applicationService;

  @Autowired
  private SectorService sectorService;

  public TenantServiceImpl() {
    super(Tenant.class);
  }

  @Override
  public TenantRepository getRepository() {
    return repository;
  }

  @Override
  public String getEntityId(final Tenant entity) {
    return entity.getId();
  }

  @Override
  public List<Tenant> findPublicsButNotMe(final String id) {
    final Query query = new Query(Criteria.where("id").ne(id).and("isPublic").is(Boolean.TRUE));
    return getMongoOps().find(query, Tenant.class);
  }

  @Override
  public List<Tenant> findPublicTenants() {
    final Query query = new Query(Criteria.where("isPublic").is(Boolean.TRUE));
    return getMongoOps().find(query, Tenant.class);
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.web.catalog.service.impl.AbstractBaseCrudServiceImpl#doAfterDelete(org.sentilo.
   * web.catalog.domain.CatalogDocument)
   */
  @Override
  protected void doAfterDelete(final Tenant tenant) {
    deleteRelated(tenant);
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.service.impl.AbstractBaseCrudServiceImpl#doAfterDelete(java.util.
   * Collection )
   */
  @Override
  protected void doAfterDelete(final Collection<Tenant> tenants) {
    for (final Tenant tenant : tenants) {
      deleteRelated(tenant);
    }
  }

  private void deleteRelated(final Tenant tenant) {
    userService.deleteFromTenant(tenant.getId());
    providerService.deleteFromTenant(tenant.getId());
    applicationService.deleteFromTenant(tenant.getId());
    sectorService.deleteFromTenant(tenant.getId());
  }
}
