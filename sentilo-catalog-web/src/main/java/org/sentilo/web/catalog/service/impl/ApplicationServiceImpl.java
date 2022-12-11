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
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.sentilo.common.enums.SignalType;
import org.sentilo.web.catalog.context.TenantContextHolder;
import org.sentilo.web.catalog.domain.ActiveSubscription;
import org.sentilo.web.catalog.domain.Alert;
import org.sentilo.web.catalog.domain.Application;
import org.sentilo.web.catalog.domain.SectorGrant;
import org.sentilo.web.catalog.domain.SectorResource.GrantType;
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
import org.springframework.data.mongodb.core.query.Update;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

import com.mongodb.BasicDBObject;

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
   * org.sentilo.web.catalog.service.ProviderService#removeProvidersFromSector(java.lang.String,
   * java.util.List)
   */
  @Override
  @Transactional
  public void removeApplicationsFromSector(final String sectorId, final List<String> applicationsIds) {

    final Query query = buildQueryForIdInCollection(applicationsIds);
    final Query queryIn = buildQueryForParamInCollection("applicationId", applicationsIds);
    final Query queryInActiveSubscription = buildQueryForParamInCollection("entityId", applicationsIds);
    final Update update = new Update();
    update.pull("sectors", new BasicDBObject("sectorId", sectorId));

    getMongoOps().updateMulti(query, update, Application.class);
    getMongoOps().updateMulti(queryIn, update, Alert.class);
    getMongoOps().updateMulti(queryInActiveSubscription, update, ActiveSubscription.class);
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.web.catalog.service.ApplicationService#addApplicationsToSector(java.lang.String,
   * java.util.Map)
   */
  @Override
  @Transactional
  public void addApplicationsToSector(final String sectorId, final Map<String, GrantType> applicationsGrants) {

    final List<String> applicationsWithReadGrant =
        applicationsGrants.keySet().stream().filter(key -> applicationsGrants.get(key).equals(GrantType.R)).collect(Collectors.toList());

    final List<String> applicationsWithAdminGrant =
        applicationsGrants.keySet().stream().filter(key -> applicationsGrants.get(key).equals(GrantType.A)).collect(Collectors.toList());

    if (!CollectionUtils.isEmpty(applicationsWithReadGrant)) {
      addSectorToApplicationsAndChilds(applicationsWithReadGrant, sectorId, GrantType.R);
    }

    if (!CollectionUtils.isEmpty(applicationsWithAdminGrant)) {
      addSectorToApplicationsAndChilds(applicationsWithAdminGrant, sectorId, GrantType.A);
    }
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.web.catalog.service.impl.AbstractBaseCrudServiceImpl#doBeforeCreate(org.sentilo
   * .web.catalog.domain.CatalogDocument)
   */
  @Override
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
  @Override
  protected void doAfterDelete(final Application application) {
    doAfterDelete(Collections.singletonList(application));
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.service.impl.AbstractBaseCrudServiceImpl#doAfterDelete(java.util.
   * Collection )
   */
  @Override
  protected void doAfterDelete(final Collection<Application> applications) {
    for (final Application application : applications) {
      deleteRelated(application);
    }

    signalService.publishInternalSignal(SignalType.RELOAD_ENTITIES, Constants.MODULE_NAME);
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.web.catalog.service.impl.AbstractBaseCrudServiceImpl#doAfterCreate(org.sentilo.web.
   * catalog.domain.CatalogDocument)
   */
  @Override
  protected void doAfterCreate(final Application entity) {
    super.doAfterCreate(entity);
    signalService.publishInternalSignal(SignalType.RELOAD_ENTITIES, Constants.MODULE_NAME);
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.web.catalog.service.impl.AbstractBaseCrudServiceImpl#doAfterUpdate(org.sentilo.web.
   * catalog.domain.CatalogDocument)
   */
  @Override
  protected void doAfterUpdate(final Application entity) {
    super.doAfterUpdate(entity);
    signalService.publishInternalSignal(SignalType.RELOAD_ENTITIES, Constants.MODULE_NAME);
  }

  private void deleteRelated(final Application application) {
    // Should be removed all resources related to the provider: alerts, sensors, components,
    // permissions and tenantPermissions (these resources only in the case of a full delete)
    final SearchFilter childFilter = new SearchFilter();
    childFilter.addAndParam("applicationId", application.getId());
    final Query childQuery = buildQuery(childFilter);

    final SearchFilter entityChildFilter = new SearchFilter();
    entityChildFilter.addAndParam("entityId", application.getId());
    final Query entityChildQuery = buildQuery(childFilter);

    doDelete(childQuery, Alert.class);
    doDelete(entityChildQuery, ActiveSubscription.class);

    permissionService.deleteRelated(application);
  }

  private void addSectorToApplicationsAndChilds(final List<String> applicationsIds, final String sectorId, final GrantType sectorGrantType) {
    final Query queryIds = buildQueryForIdInCollection(applicationsIds);
    final Query queryIn = buildQueryForParamInCollection("applicationId", applicationsIds);
    final Query queryInActiveSubscription = buildQueryForParamInCollection("entityId", applicationsIds);
    final Update update = buildUpdate();
    update.push("sectors", new SectorGrant(sectorId, sectorGrantType));

    getMongoOps().updateMulti(queryIds, update, Application.class);
    getMongoOps().updateMulti(queryIn, update, Alert.class);
    getMongoOps().updateMulti(queryInActiveSubscription, update, ActiveSubscription.class);
  }

}
