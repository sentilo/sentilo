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
package org.sentilo.web.catalog.admin.service.impl;

import java.util.AbstractSet;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.sentilo.common.converter.DefaultStringMessageConverter;
import org.sentilo.common.converter.StringMessageConverter;
import org.sentilo.common.domain.CatalogComponent;
import org.sentilo.common.domain.CatalogResponseMessage;
import org.sentilo.common.domain.CatalogSensor;
import org.sentilo.common.domain.MutableCatalogElement;
import org.sentilo.common.enums.SensorState;
import org.sentilo.common.rest.RESTClient;
import org.sentilo.common.rest.RequestContext;
import org.sentilo.common.rest.impl.RESTClientImpl;
import org.sentilo.web.catalog.admin.service.FederatedSynchronizationService;
import org.sentilo.web.catalog.admin.support.FederatedResourcesDelta;
import org.sentilo.web.catalog.admin.support.RemoteFederatedResources;
import org.sentilo.web.catalog.converter.CatalogDocumentConverter;
import org.sentilo.web.catalog.domain.CatalogDocument;
import org.sentilo.web.catalog.domain.Component;
import org.sentilo.web.catalog.domain.Contact;
import org.sentilo.web.catalog.domain.FederationConfig;
import org.sentilo.web.catalog.domain.Provider;
import org.sentilo.web.catalog.domain.Sensor;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.service.ComponentService;
import org.sentilo.web.catalog.service.CrudService;
import org.sentilo.web.catalog.service.FederationConfigService;
import org.sentilo.web.catalog.service.ProviderService;
import org.sentilo.web.catalog.service.SensorService;
import org.sentilo.web.catalog.service.impl.AbstractBaseCrudServiceImpl;
import org.sentilo.web.catalog.utils.TenantUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;

@org.springframework.stereotype.Component
public class FederatedSynchronizationServiceImpl implements FederatedSynchronizationService {

  private static final Logger LOGGER = LoggerFactory.getLogger(FederatedSynchronizationServiceImpl.class);

  @Autowired
  private FederationConfigService fcService;

  @Autowired
  private ProviderService providerService;

  @Autowired
  private ComponentService componentService;

  @Autowired
  private SensorService sensorService;

  private RESTClient restClient;

  @Override
  public void syncCatalogs() {
    final SearchFilter activeFcfilter = new SearchFilter();
    activeFcfilter.addAndParam("active", Boolean.TRUE);
    final List<FederationConfig> fConfigs = fcService.search(activeFcfilter).getContent();

    LOGGER.debug("Found {} active federated services configured", fConfigs.size());
    for (final FederationConfig fConfig : fConfigs) {
      syncFederatedService(fConfig);
    }
  }

  private void syncFederatedService(final FederationConfig fConfig) {
    LOGGER.info("Start process to synchronize catalog from the federated instance [{}] ", fConfig.getName());
    // The steps to follow are:
    // 1. Get the authorized resources from the federated instance (providers, components and
    // sensors)
    // 2. Calculate delta between these resources and the existing ones in local catalog
    // 3. Save in local catalog the resources found in the delta process
    try {
      final RemoteFederatedResources remoteResources = getRemoteFederatedResources(fConfig);
      syncProviders(fConfig, remoteResources);
      syncComponents(fConfig, remoteResources);
      syncSensors(fConfig, remoteResources);

      fConfig.setLastSyncTime(new Date());
      fcService.update(fConfig);
      LOGGER.info("Synchronization process with federated instance [{}] has successfully finished.", fConfig.getName());
    } catch (final Exception e) {
      LOGGER.warn("Synchronization process with federated instance [{}] has failed. It will be retried later", fConfig.getName(), e);
    }
  }

  private void syncSensors(final FederationConfig fConfig, final RemoteFederatedResources remoteResources) throws Exception {
    final SearchFilter filter = buildFederatedSearchFilter(fConfig);
    final Map<String, CatalogSensor> remoteSensors = remoteResources.getSensors();
    final Map<String, Sensor> localSensors = toMap(sensorService.search(filter).getContent());

    final FederatedResourcesDelta<CatalogSensor, Sensor> deltaResult = FederatedResourcesDelta.build(fConfig, remoteSensors, localSensors);

    insertFederatedResources(fConfig, deltaResult, sensorService);
    updateFederatedResources(fConfig, deltaResult, sensorService);
    deleteFederatedResources(fConfig, deltaResult, sensorService);
  }

  private void syncComponents(final FederationConfig fConfig, final RemoteFederatedResources remoteResources) throws Exception {
    final SearchFilter filter = buildFederatedSearchFilter(fConfig);
    final Map<String, CatalogComponent> remoteComponents = remoteResources.getComponents();
    final Map<String, Component> localComponents = toMap(componentService.search(filter).getContent());

    final FederatedResourcesDelta<CatalogComponent, Component> deltaResult =
        FederatedResourcesDelta.build(fConfig, remoteComponents, localComponents);

    insertFederatedResources(fConfig, deltaResult, componentService);
    updateFederatedResources(fConfig, deltaResult, componentService);
    deleteFederatedResources(fConfig, deltaResult, componentService);
  }

  private void syncProviders(final FederationConfig fConfig, final RemoteFederatedResources remoteResources) {
    final SearchFilter filter = buildFederatedSearchFilter(fConfig);
    final Set<String> remoteProviders = remoteResources.getProviders();
    final Set<String> localProviders = toMap(providerService.search(filter).getContent()).keySet();

    // Providers can only be created or deleted, but not updated by the federation process because
    // there
    // isn't metadata fields to update
    final AbstractSet<String> providersIdsToInsert = com.google.common.collect.Sets.difference(remoteProviders, localProviders);
    final AbstractSet<String> providersIdsToDelete = com.google.common.collect.Sets.difference(localProviders, remoteProviders);

    insertFederatedProviders(fConfig, providersIdsToInsert);
    deleteFederatedProviders(fConfig, providersIdsToDelete);
  }

  private void insertFederatedProviders(final FederationConfig fConfig, final AbstractSet<String> providersIdsToInsert) {

    LOGGER.info("Start process to insert new providers associated with the federated service {}.", fConfig.getId());
    final Collection<Provider> providersToInsert = new ArrayList<Provider>();

    for (final String remoteProviderId : providersIdsToInsert) {
      final Provider provider = new Provider(remoteProviderId);
      provider.setDescription("Federated resource from service " + fConfig.getId());

      final Contact contact = new Contact();
      contact.setEmail(fConfig.getSourceContactMail());
      contact.setName(fConfig.getSourceContactName());
      provider.setContact(contact);

      setDefaultFederatedFields(provider, fConfig, providerService);
      TenantUtils.copyTenantFields(provider, fConfig);
      provider.setDefaultValues();

      providersToInsert.add(provider);
    }

    LOGGER.debug("Number of providers to insert: {}", providersToInsert.size());
    providerService.insertAll(providersToInsert);
    LOGGER.info("Process to insert new providers associated with the federated service {} has successfully finished. ", fConfig.getId());
  }

  private void deleteFederatedProviders(final FederationConfig fConfig, final AbstractSet<String> providersIdsToDelete) {
    LOGGER.info("Start process to delete providers associated with the federated service {}.", fConfig.getId());
    final Collection<Provider> providersToDelete = new ArrayList<Provider>();

    for (final String resourceId : providersIdsToDelete) {
      providersToDelete.add(new Provider(resourceId));
    }

    LOGGER.debug("Number of providers to delete: {}", providersToDelete.size());
    providerService.delete(providersToDelete);
    LOGGER.info("Process to delete providers associated with the federated service {} has successfully finished. ", fConfig.getId());
  }

  private <S extends MutableCatalogElement, T extends CatalogDocument> void insertFederatedResources(final FederationConfig fConfig,
      final FederatedResourcesDelta<S, T> deltaResult, final CrudService<T> service) throws Exception {
    LOGGER.info("Start process to insert new resources associated with the federated service {}.", fConfig.getId());
    final Collection<T> resourcesToInsert = new ArrayList<T>();

    for (final String remoteResourceId : deltaResult.getResourcesToInsert()) {
      final S remoteResource = deltaResult.getRemoteResources().get(remoteResourceId);
      final T localResource = buildNewInstance(remoteResourceId, ((AbstractBaseCrudServiceImpl<T>) service).getType());
      CatalogDocumentConverter.copyProperties(remoteResource, localResource);

      // Set default federated field values
      setDefaultFederatedFields(localResource, fConfig, service);

      resourcesToInsert.add(localResource);
    }

    LOGGER.debug("Number of resources to insert: {}", resourcesToInsert.size());
    service.insertAll(resourcesToInsert);
    LOGGER.info("Process to insert new resources associated with the federated service {} has successfully finished. ", fConfig.getId());
  }

  private <S extends MutableCatalogElement, T extends CatalogDocument> void updateFederatedResources(final FederationConfig fConfig,
      final FederatedResourcesDelta<S, T> deltaResult, final CrudService<T> service) {
    LOGGER.info("Start process to update resources associated with the federated service {}.", fConfig.getId());
    final Collection<T> resourcesToUpdate = new ArrayList<T>();

    for (final String remoteResourceId : deltaResult.getResourcesToUpdate()) {
      final S remoteResource = deltaResult.getRemoteResources().get(remoteResourceId);
      final T localResource = deltaResult.getLocalResources().get(remoteResourceId);
      CatalogDocumentConverter.copyProperties(remoteResource, localResource);

      resourcesToUpdate.add(localResource);
    }

    LOGGER.debug("Number of resources to update: {}", resourcesToUpdate.size());
    service.updateAll(resourcesToUpdate);
    LOGGER.info("Process to update resources associated with the federated service {} has successfully finished. ", fConfig.getId());
  }

  private <S extends MutableCatalogElement, T extends CatalogDocument> void deleteFederatedResources(final FederationConfig fConfig,
      final FederatedResourcesDelta<S, T> deltaResult, final CrudService<T> service) {
    LOGGER.info("Start process to delete resources associated with the federated service {}.", fConfig.getId());
    final Collection<T> resourcesToDelete = new ArrayList<T>();

    for (final String resourceId : deltaResult.getResourcesToDelete()) {
      final T localResource = deltaResult.getLocalResources().get(resourceId);
      resourcesToDelete.add(localResource);
    }

    LOGGER.debug("Number of resources to delete: {}", resourcesToDelete.size());
    service.delete(resourcesToDelete);
    LOGGER.info("Process to delete resources associated with the federated service {} has successfully finished. ", fConfig.getId());
  }

  private RemoteFederatedResources getRemoteFederatedResources(final FederationConfig fConfig) throws Exception {
    final StringMessageConverter converter = new DefaultStringMessageConverter();
    final RequestContext rc = new RequestContext("/catalog");
    rc.setIdentityToken(fConfig.getAppClientToken());
    rc.setHost(fConfig.getSourceEndpoint());

    final String strResponse = getRestClient().get(rc);
    final CatalogResponseMessage crm = (CatalogResponseMessage) converter.unmarshal(strResponse, CatalogResponseMessage.class);
    return new RemoteFederatedResources(fConfig, crm.getProviders());
  }

  private RESTClient getRestClient() throws Exception {
    if (restClient == null) {
      restClient = new RESTClientImpl();
      ((RESTClientImpl) restClient).afterPropertiesSet();
    }
    return restClient;
  }

  private <T extends CatalogDocument> Map<String, T> toMap(final List<T> sourceList) {
    final Map<String, T> result = new HashMap<String, T>();
    for (final T item : sourceList) {
      result.put(item.getId(), item);
    }
    return result;
  }

  private SearchFilter buildFederatedSearchFilter(final FederationConfig fConfig) {
    final SearchFilter filter = new SearchFilter();
    filter.addAndParam("federatedServiceId", fConfig.getId());
    return filter;
  }

  private <T extends CatalogDocument> void setDefaultFederatedFields(final T localResource, final FederationConfig fConfig,
      final CrudService<T> service) {

    try {
      final Class<T> clazz = ((AbstractBaseCrudServiceImpl<T>) service).getType();
      clazz.getMethod("setFederatedResource", Boolean.class).invoke(localResource, Boolean.TRUE);
      clazz.getMethod("setFederatedServiceId", String.class).invoke(localResource, fConfig.getId());

      if (localResource instanceof Sensor) {
        ((Sensor) localResource).setState(SensorState.online);
      }
    } catch (final Exception e) {
      LOGGER.warn("An error has ocurred trying to set default field values to resource with id {}. ", localResource.getId(), e);
    }
  }

  private <T extends CatalogDocument> T buildNewInstance(final String resourceId, final Class<T> clazz) throws Exception {
    return clazz.getConstructor(String.class).newInstance(resourceId);
  }

}
