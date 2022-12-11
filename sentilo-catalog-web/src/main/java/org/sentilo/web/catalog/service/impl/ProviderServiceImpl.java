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
import java.util.Set;
import java.util.stream.Collectors;

import org.sentilo.common.enums.SignalType;
import org.sentilo.web.catalog.context.TenantContextHolder;
import org.sentilo.web.catalog.domain.ActiveSubscription;
import org.sentilo.web.catalog.domain.Alert;
import org.sentilo.web.catalog.domain.AlertRule;
import org.sentilo.web.catalog.domain.Component;
import org.sentilo.web.catalog.domain.DocumentFile;
import org.sentilo.web.catalog.domain.Provider;
import org.sentilo.web.catalog.domain.SectorGrant;
import org.sentilo.web.catalog.domain.SectorResource.GrantType;
import org.sentilo.web.catalog.domain.Sensor;
import org.sentilo.web.catalog.repository.ProviderRepository;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.security.audit.Auditable;
import org.sentilo.web.catalog.security.audit.AuditingActionType;
import org.sentilo.web.catalog.service.PermissionService;
import org.sentilo.web.catalog.service.ProviderService;
import org.sentilo.web.catalog.service.TenantPermissionService;
import org.sentilo.web.catalog.utils.Constants;
import org.sentilo.web.catalog.utils.TenantUtils;
import org.sentilo.web.catalog.validator.ResourceKeyValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.data.mongodb.core.query.Update;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import com.mongodb.BasicDBObject;

@Service
public class ProviderServiceImpl extends AbstractBaseCrudServiceImpl<Provider> implements ProviderService {

  @Autowired
  private ProviderRepository repository;

  @Autowired
  private PermissionService permissionService;

  @Autowired
  private TenantPermissionService tenantPermissionService;

  @Autowired
  @Qualifier("appsAndProvidersKeyValidator")
  private ResourceKeyValidator customResourceKeyValidator;

  public ProviderServiceImpl() {
    super(Provider.class);
  }

  @Override
  protected void doAfterInit() {
    setResourceKeyValidator(customResourceKeyValidator);
    super.doAfterInit();
  }

  @Override
  public ProviderRepository getRepository() {
    return repository;
  }

  @Override
  public String getEntityId(final Provider entity) {
    return entity.getId();
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.service.ProviderService#findAllowed()
   */
  @Override
  public List<Provider> findAllowed() {
    final SearchFilter filter = new SearchFilter();
    if (TenantContextHolder.isEnabled()) {
      filter.addParam("tenantsAuth", TenantUtils.getCurrentTenant());
    }
    final Query query = buildQuery(filter);
    return getMongoOps().find(query, Provider.class);
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.web.catalog.service.ProviderService#deleteChildrens(org.sentilo.web.catalog.domain.
   * Provider)
   */
  @Override
  @Auditable(actionType = AuditingActionType.DELETE)
  @Transactional
  public void deleteChildren(final Provider provider) {
    deleteRelatedResources(provider, false);
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.service.ProviderService#addGrantedTenant(java.lang.String,
   * java.lang.String)
   */
  @Override
  public void addGrantedTenant(final String providerId, final String tenantId) {
    final Provider provider = findAndThrowErrorIfNotExist(new Provider(providerId));
    provider.getTenantsAuth().add(tenantId);
    provider.getTenantsListVisible().add(tenantId);
    getMongoOps().save(provider);
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.service.ProviderService#removeGrantedTenant(java.lang.String,
   * java.lang.String)
   */
  @Override
  public void removeGrantedTenant(final String providerId, final String tenantId) {
    final Provider provider = findAndThrowErrorIfNotExist(new Provider(providerId));
    provider.getTenantsAuth().remove(tenantId);
    provider.getTenantsListVisible().remove(tenantId);
    getMongoOps().save(provider);
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.service.ProviderService#getGrantedTenantsIds(java.lang.String)
   */
  @Override
  public Set<String> getGrantedTenantsIds(final String providerId) {
    return findAndThrowErrorIfNotExist(new Provider(providerId)).getTenantsAuth();
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.service.ProviderService#deleteFromTenant(java.lang.String)
   */
  @Override
  public void deleteFromTenant(final String tenantId) {
    // Remove all providers associated with tenant tenantId
    deleteProvidersThatMatchCondition("tenantId", tenantId);
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.service.ProviderService#isProviderFromTenant(java.lang.String,
   * java.lang.String)
   */
  @Override
  public boolean isProviderFromTenant(final String providerId, final String tenantId) {
    final Provider provider = findAndThrowErrorIfNotExist(new Provider(providerId));
    return tenantId.equals(provider.getTenantId());
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.service.ProviderService#deleteFederatedResources(java.lang.String)
   */
  @Override
  public void deleteFederatedResources(final String federatedConfigId) {
    // Removes all providers federated with federatedConfigId
    deleteProvidersThatMatchCondition("federatedServiceId", federatedConfigId);
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
  public void removeProvidersFromSector(final String sectorId, final List<String> providersIds) {
    // db.provider.update({_id:{$in:[....]} }, { $pull: { 'sectors': {'sectorId': '***' }} },
    // {multi:true})

    final Query query = buildQueryForIdInCollection(providersIds);
    final Query queryIn = buildQueryForParamInCollection("providerId", providersIds);
    final Query queryInActiveSubscription = buildQueryForParamInCollection("entityId", providersIds);
    final Update update = new Update();
    update.pull("sectors", new BasicDBObject("sectorId", sectorId));

    getMongoOps().updateMulti(query, update, Provider.class);
    getMongoOps().updateMulti(queryIn, update, Component.class);
    getMongoOps().updateMulti(queryIn, update, Sensor.class);
    getMongoOps().updateMulti(queryIn, update, Alert.class);
    getMongoOps().updateMulti(queryInActiveSubscription, update, ActiveSubscription.class);
    getMongoOps().updateMulti(queryIn, update, AlertRule.class);

  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.service.ProviderService#addProvidersToSector(java.lang.String,
   * java.util.Map)
   */
  @Override
  @Transactional
  public void addProvidersToSector(final String sectorId, final Map<String, GrantType> providersGrants) {

    // To add sector privileges to both provider and childs first step is split providersGrants into
    // two new collections: one for providers with read privilege and another with admin privilege

    final List<String> providersWithReadGrant =
        providersGrants.keySet().stream().filter(key -> providersGrants.get(key).equals(GrantType.R)).collect(Collectors.toList());

    final List<String> providersWithAdminGrant =
        providersGrants.keySet().stream().filter(key -> providersGrants.get(key).equals(GrantType.A)).collect(Collectors.toList());

    if (!CollectionUtils.isEmpty(providersWithReadGrant)) {
      addSectorToProvidersAndChilds(providersWithReadGrant, sectorId, GrantType.R);
    }

    if (!CollectionUtils.isEmpty(providersWithAdminGrant)) {
      addSectorToProvidersAndChilds(providersWithAdminGrant, sectorId, GrantType.A);
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
  protected void doBeforeCreate(final Provider provider) {
    checkIntegrityKey(provider.getId());

    // Default value of field name equals to id value if field name is empty (it isn't mandatory
    // that user filled in it)
    provider.setDefaultValues();

    permissionService.createRelated(provider);
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.service.impl.AbstractBaseCrudServiceImpl#doAfterCreate(java.util.
   * Collection)
   */
  @Override
  protected void doAfterCreate(final Collection<Provider> providers) {
    for (final Provider provider : providers) {
      permissionService.createRelated(provider);
    }
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.web.catalog.service.impl.AbstractBaseCrudServiceImpl#doAfterDelete(org.sentilo.
   * web.catalog.domain.CatalogDocument)
   */
  @Override
  protected void doAfterDelete(final Provider provider) {
    doAfterDelete(Collections.singletonList(provider));
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.web.catalog.service.impl.AbstractBaseCrudServiceImpl#delete(java.util.Collection)
   */
  @Override
  protected void doAfterDelete(final Collection<Provider> providers) {
    for (final Provider provider : providers) {
      deleteRelatedResources(provider);
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
  protected void doAfterCreate(final Provider entity) {
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
  protected void doAfterUpdate(final Provider entity) {
    super.doAfterUpdate(entity);
    signalService.publishInternalSignal(SignalType.RELOAD_ENTITIES, Constants.MODULE_NAME);
  }

  private void deleteProvidersThatMatchCondition(final String fieldName, final String fieldValue) {
    final SearchFilter filter = new SearchFilter();
    filter.addAndParam(fieldName, fieldValue);
    final Query query = buildQuery(filter);
    final List<Provider> providers = getMongoOps().find(query, Provider.class);
    if (!CollectionUtils.isEmpty(providers)) {
      delete(providers);
    }
  }

  private void deleteRelatedResources(final Provider provider) {
    deleteRelatedResources(provider, true);
  }

  private void deleteRelatedResources(final Provider provider, final boolean fullDelete) {
    // Should be removed all resources related to the provider: alerts, sensors, components,
    // permissions and tenantPermissions (these resources only in the case of a full delete)
    final SearchFilter childFilter = new SearchFilter();
    childFilter.addAndParam("providerId", provider.getId());
    final Query childQuery = buildQuery(childFilter);

    final SearchFilter entityChildFilter = new SearchFilter();
    entityChildFilter.addAndParam("entityId", provider.getId());
    final Query entityChildQuery = buildQuery(childFilter);

    doDelete(childQuery, Alert.class);
    doDelete(childQuery, Sensor.class);
    doDelete(childQuery, Component.class);
    doDelete(childQuery, AlertRule.class);
    doDelete(entityChildQuery, DocumentFile.class);
    doDelete(entityChildQuery, ActiveSubscription.class);

    if (fullDelete) {
      // Also delete all tenant permissions related to it
      tenantPermissionService.deleteRelatedEntity(provider);

      // And finally remove related permissions over other entities
      permissionService.deleteRelated(provider);
    }
  }

  private void addSectorToProvidersAndChilds(final List<String> providersIds, final String sectorId, final GrantType sectorGrantType) {
    final Query queryIds = buildQueryForIdInCollection(providersIds);
    final Query queryIn = buildQueryForParamInCollection("providerId", providersIds);
    final Query queryInActiveSubscription = buildQueryForParamInCollection("entityId", providersIds);
    final Update update = buildUpdate();
    update.push("sectors", new SectorGrant(sectorId, sectorGrantType));

    getMongoOps().updateMulti(queryIds, update, Provider.class);
    getMongoOps().updateMulti(queryIn, update, Component.class);
    getMongoOps().updateMulti(queryIn, update, Sensor.class);
    getMongoOps().updateMulti(queryIn, update, Alert.class);
    getMongoOps().updateMulti(queryInActiveSubscription, update, ActiveSubscription.class);
    getMongoOps().updateMulti(queryIn, update, AlertRule.class);
  }
}
