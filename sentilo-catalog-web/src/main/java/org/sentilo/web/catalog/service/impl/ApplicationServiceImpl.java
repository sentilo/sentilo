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

import org.sentilo.web.catalog.context.TenantContextHolder;
import org.sentilo.web.catalog.domain.Application;
import org.sentilo.web.catalog.repository.ApplicationRepository;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.service.ApplicationService;
import org.sentilo.web.catalog.service.PermissionService;
import org.sentilo.web.catalog.utils.Constants;
import org.sentilo.web.catalog.utils.IdentityKeyGenerator;
import org.sentilo.web.catalog.utils.TenantUtils;
import org.sentilo.web.catalog.validator.ResourceKeyValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

@Service
public class ApplicationServiceImpl extends AbstractBaseCrudServiceImpl<Application> implements ApplicationService {

  @Autowired
  private ApplicationRepository repository;

  @Autowired
  private PermissionService permissionService;

  @Autowired
  @Qualifier("appsAndProvidersKeyValidator")
  private ResourceKeyValidator customResourceKeyValidator;

  public ApplicationServiceImpl() {
    super(Application.class);
  }

  @Override
  protected void doAfterInit() {
    setResourceKeyValidator(customResourceKeyValidator);
    super.doAfterInit();
  }

  @Override
  public ApplicationRepository getRepository() {
    return repository;
  }

  @Override
  public String getEntityId(final Application entity) {
    return entity.getId();
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.service.ApplicationService#findAllowed()
   */
  @Override
  public List<Application> findAllowed() {
    final SearchFilter filter = new SearchFilter();
    if (TenantContextHolder.isEnabled()) {
      filter.addParam("tenantsAuth", TenantUtils.getCurrentTenant());
    }
    final Query query = buildQuery(filter);
    return getMongoOps().find(query, Application.class);
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.service.ApplicationService#deleteFromTenant(java.lang.String)
   */
  @Override
  public void deleteFromTenant(final String tenantId) {
    final SearchFilter filter = new SearchFilter();
    filter.addAndParam("tenantId", tenantId);
    final Query query = buildQuery(filter);
    final List<Application> applications = getMongoOps().find(query, Application.class);
    delete(applications);
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.service.ApplicationService#isApplicationFromTenant(java.lang.
   * String, java.lang.String)
   */
  @Override
  public boolean isApplicationFromTenant(final String applicationId, final String tenantId) {
    final Application application = findAndThrowErrorIfNotExist(new Application(applicationId));
    return tenantId.equals(application.getTenantId());
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.web.catalog.service.impl.AbstractBaseCrudServiceImpl#doBeforeCreate(org.sentilo
   * .web.catalog.domain.CatalogDocument)
   */
  protected void doBeforeCreate(final Application application) {
    // El identificador se informa por pantalla (es obligatorio). El nombre, en caso de no estar
    // informado, se rellena con el valor del identificador.
    if (!StringUtils.hasText(application.getName())) {
      application.setName(application.getId());
    }

    if (TenantContextHolder.isEnabled()) {
      // In a multitenant instance, to allow different tenants to have entities with the same id,
      // the proposed entity id filled in by user is modified by prepending the tenant id
      final String newId = application.getTenantId() + Constants.MULTITENANT_ENTITY_ID_PREPEND_TOKEN + application.getId();
      application.setId(newId);
    }

    checkIntegrityKey(application.getId());

    application.setToken(IdentityKeyGenerator.generateNewToken(application.getId()));

    // Create the related permissions
    permissionService.createRelated(application);
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.web.catalog.service.impl.AbstractBaseCrudServiceImpl#doAfterDelete(org.sentilo.
   * web.catalog.domain.CatalogDocument)
   */
  protected void doAfterDelete(final Application application) {
    deleteRelated(application);
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.service.impl.AbstractBaseCrudServiceImpl#doAfterDelete(java.util.
   * Collection )
   */
  protected void doAfterDelete(final Collection<Application> applications) {
    for (final Application application : applications) {
      deleteRelated(application);
    }
  }

  private void deleteRelated(final Application application) {
    permissionService.deleteRelated(application);
  }

}
