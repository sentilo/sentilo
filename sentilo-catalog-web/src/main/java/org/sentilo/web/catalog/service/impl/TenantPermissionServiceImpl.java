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

import java.util.Arrays;
import java.util.List;

import org.sentilo.web.catalog.domain.CatalogDocument;
import org.sentilo.web.catalog.domain.TenantPermission;
import org.sentilo.web.catalog.exception.builder.CompoundDuplicateKeyExceptionBuilder;
import org.sentilo.web.catalog.repository.TenantPermissionRepository;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.service.TenantPermissionService;
import org.sentilo.web.catalog.service.TenantResourceService;
import org.sentilo.web.catalog.utils.Constants;
import org.sentilo.web.catalog.utils.enums.EntityType;
import org.sentilo.web.catalog.validator.DefaultResourceKeyValidatorImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class TenantPermissionServiceImpl extends AbstractBaseCrudServiceImpl<TenantPermission> implements TenantPermissionService {

  @Autowired
  private TenantPermissionRepository repository;

  @Autowired
  private TenantResourceService tenantResourceService;

  public TenantPermissionServiceImpl() {
    super(TenantPermission.class);
  }

  @Override
  protected void doAfterInit() {
    setResourceKeyValidator(new DefaultResourceKeyValidatorImpl(getRepository(),
        new CompoundDuplicateKeyExceptionBuilder("error.tenantPermission.duplicate.key", Constants.PERMISSION_TOKEN_SPLITTER)));
    super.doAfterInit();
  }

  @Override
  public TenantPermissionRepository getRepository() {
    return repository;
  }

  @Override
  public String getEntityId(final TenantPermission tenantPermission) {
    return tenantPermission.getId();
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.service.TenantPermissionService#deleteRelated(org.sentilo.web
   * .catalog.domain .CatalogDocument)
   */
  @Override
  public void deleteRelated(final String tenantId) {
    final SearchFilter filter = new SearchFilter();
    filter.addParam("source", tenantId);
    filter.addParam("target", tenantId);

    delete(filter);
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.service.TenantPermissionService#deleteRelatedEntity(org.sentilo
   * .web.catalog.domain.CatalogDocument)
   */
  @Override
  public void deleteRelatedEntity(final CatalogDocument entity) {
    final SearchFilter filter = new SearchFilter();
    filter.addAndParam("entity", entity.getId());
    filter.addAndParam("entityType", EntityType.PROVIDER);

    delete(filter);
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.service.TenantPermissionService#findByEntity(org.sentilo.web.
   * catalog.domain.CatalogDocument)
   */
  @Override
  public TenantPermission findFromPermissionsByEntity(final String tenantId, final String entityId) {
    final SearchFilter filter = new SearchFilter();
    filter.addAndParam("entity", entityId);
    filter.addAndParam("target", tenantId);
    filter.addAndParam("entityType", EntityType.PROVIDER);

    return getMongoOps().findOne(buildQuery(filter), TenantPermission.class);
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.web.catalog.service.TenantPermissionService#changeMapVisibility(java.lang.String[],
   * boolean)
   */
  @Override
  public void changeMapVisibility(final String[] permissionIds, final boolean isMapVisible) {
    final List<String> values = Arrays.asList(permissionIds);
    updateMulti(values, "visible", isMapVisible);

    // Update component map visible values
    for (final String permissionId : permissionIds) {
      final TenantPermission tenantPermission = findAndThrowErrorIfNotExist(new TenantPermission(permissionId));
      if (isMapVisible) {
        tenantResourceService.addTenantVisibilityToProviderResources(tenantPermission.getEntity(), tenantPermission.getTarget());
      } else {
        tenantResourceService.removeTenantVisibilityFromProviderResources(tenantPermission.getEntity(), tenantPermission.getTarget());
      }
    }
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.web.catalog.service.TenantPermissionService#changeListVisibility(java.lang.String[]
   * , boolean)
   */
  @Override
  public void changeListVisibility(final String[] permissionIds, final boolean isListVisible) {
    final List<String> values = Arrays.asList(permissionIds);
    updateMulti(values, "listVisible", isListVisible);

    // Update component list visible values
    for (final String permissionId : permissionIds) {
      final TenantPermission tenantPermission = findAndThrowErrorIfNotExist(new TenantPermission(permissionId));
      if (isListVisible) {
        tenantResourceService.addTenantListVisibilityToProviderResources(tenantPermission.getEntity(), tenantPermission.getTarget());
      } else {
        tenantResourceService.removeTenantListVisibilityFromProviderResources(tenantPermission.getEntity(), tenantPermission.getTarget());
      }
    }
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.web.catalog.service.impl.AbstractBaseCrudServiceImpl#doAfterCreate(org.sentilo.web.
   * catalog.domain.CatalogDocument)
   */
  @Override
  protected void doAfterCreate(final TenantPermission permission) {
    // Once the permission is created, it must be propagated to entity resources to grant target
    // access to them
    tenantResourceService.addTenantGrantToProviderResources(permission);
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.web.catalog.service.impl.AbstractBaseCrudServiceImpl#doAfterDelete(org.sentilo.web.
   * catalog.domain.CatalogDocument)
   */
  @Override
  protected void doAfterDelete(final TenantPermission permission) {
    // Undo target grant access to entity resources
    tenantResourceService.removeTenantGrantFromProviderResources(permission.getEntity(), permission.getTarget());
  }

}
