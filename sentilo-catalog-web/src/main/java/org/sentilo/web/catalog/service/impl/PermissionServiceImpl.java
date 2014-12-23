/*
 * Sentilo
 * 
 * Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de Barcelona.
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

import java.util.List;

import org.sentilo.web.catalog.domain.CatalogDocument;
import org.sentilo.web.catalog.domain.Permission;
import org.sentilo.web.catalog.domain.Permissions;
import org.sentilo.web.catalog.exception.builder.CompoundDuplicateKeyExceptionBuilder;
import org.sentilo.web.catalog.repository.PermissionRepository;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.service.PermissionService;
import org.sentilo.web.catalog.utils.Constants;
import org.sentilo.web.catalog.validator.DefaultEntityKeyValidatorImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

@Service
public class PermissionServiceImpl extends AbstractBaseServiceImpl<Permission> implements PermissionService {

  @Autowired
  private PermissionRepository repository;

  @Value("${catalog.app.id}")
  private String catalogApplicationId;

  public PermissionServiceImpl() {
    super(Permission.class);
  }

  @Override
  protected void doAfterInit() {
    setEntityKeyValidator(new DefaultEntityKeyValidatorImpl(getRepository(), new CompoundDuplicateKeyExceptionBuilder(
        "error.permission.duplicate.key", Constants.PERMISSION_TOKEN_SPLITTER)));
    super.doAfterInit();
  }

  @Override
  public PermissionRepository getRepository() {
    return repository;
  }

  @Override
  public String getEntityId(final Permission entity) {
    return entity.getId();
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.sentilo.web.catalog.service.PermissionService#retrievePermissions()
   */
  @Override
  public Permissions retrievePermissions() {
    return new Permissions(findAll());
  }

  /*
   * (non-Javadoc)
   * 
   * @see
   * org.sentilo.web.catalog.service.PermissionService#deleteRelated(org.sentilo.web.catalog.domain
   * .CatalogDocument)
   */
  @Override
  public void deleteRelated(final CatalogDocument entity) {
    // Tenemos que eliminar todos aquellos permisos en donde el target o el source del permiso sea
    // la entidad informada
    // por parametro.
    final SearchFilter filter = new SearchFilter();
    filter.addParam("source", entity.getId());
    filter.addParam("target", entity.getId());

    getMongoOps().remove(buildQuery(filter), Permission.class);
  }

  /*
   * (non-Javadoc)
   * 
   * @see
   * org.sentilo.web.catalog.service.PermissionService#createRelated(org.sentilo.web.catalog.domain
   * .CatalogDocument)
   */
  @Override
  public void createRelated(final CatalogDocument entity) {
    createOwnPermission(entity);
    createCatalogPermission(entity);
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.sentilo.web.catalog.service.PermissionService#getActivePermissions(java.lang.String)
   */
  @Override
  public List<Permission> getActivePermissions(final String entityId) {
    final SearchFilter filter = new SearchFilter();
    filter.addAndParam("source", entityId);
    return super.search(filter).getContent();
  }

  private void createCatalogPermission(final CatalogDocument entity) {
    create(buildCatalogPermissionFor(entity));
  }

  private void createOwnPermission(final CatalogDocument entity) {
    create(new Permission(entity.getId()));
  }

  private Permission buildCatalogPermissionFor(final CatalogDocument entity) {
    return new Permission(catalogApplicationId, entity.getId(), Constants.CATALOG_PERMISSION_TYPE);
  }
}
