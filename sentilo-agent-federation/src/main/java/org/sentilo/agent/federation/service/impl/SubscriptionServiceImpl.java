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
package org.sentilo.agent.federation.service.impl;

import java.util.List;
import java.util.stream.Collectors;

import org.sentilo.agent.federation.domain.FederationConfig;
import org.sentilo.agent.federation.service.LocalPlatformService;
import org.sentilo.agent.federation.service.RemotePlatformService;
import org.sentilo.agent.federation.service.SubscriptionService;
import org.sentilo.common.domain.AuthorizedProvider;
import org.sentilo.platform.client.core.domain.Subscription;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class SubscriptionServiceImpl implements SubscriptionService {

  private static final Logger LOGGER = LoggerFactory.getLogger(SubscriptionServiceImpl.class);

  @Autowired
  private RemotePlatformService remotePlatformService;

  @Autowired
  private LocalPlatformService localPlatformService;

  public void synchronize(final FederationConfig fConfig) {
    if (fConfig.isActive()) {
      synchronizeActiveService(fConfig);
    } else {
      disableService(fConfig);
    }
  }

  private void synchronizeActiveService(final FederationConfig fConfig) {
    LOGGER.info("Start process to synchronize subscriptons with federation server {}", fConfig.getId());

    final List<String> remoteProvidersIds = getRemoteProvidersIds(fConfig);
    final List<String> currentProvidersIdsWithSubscriptions = getCurrentProvidersIdsWithSubscriptions(fConfig);

    final List<String> remoteProvidersWithoutSubscription = subtract(remoteProvidersIds, currentProvidersIdsWithSubscriptions);
    final List<String> remoteSubscriptionsToDelete = subtract(currentProvidersIdsWithSubscriptions, remoteProvidersIds);

    remotePlatformService.createSubscriptions(remoteProvidersWithoutSubscription, fConfig);
    remotePlatformService.deleteSubscriptions(remoteSubscriptionsToDelete, fConfig);

    LOGGER.info("Process finished for federation server {}", fConfig.getId());
  }

  private void disableService(final FederationConfig fConfig) {
    LOGGER.info("Start process to disconnect and clean service {} from remote instance", fConfig.getId());
    final boolean success = remotePlatformService.deleteSubscriptions(fConfig);
    if (success) {
      localPlatformService.deleteFederatedConfig(fConfig);
      LOGGER.info("Disable process finished for federation server {}", fConfig.getId());
    } else {
      LOGGER.info("Disable process has not finished successfully. It will be retried later");
    }

  }

  private List<String> getRemoteProvidersIds(final FederationConfig fConfig) {
    final List<AuthorizedProvider> remoteAuthProviders = remotePlatformService.getPermissions(fConfig);

    final List<String> remoteProvidersIds = remoteAuthProviders.stream().map(AuthorizedProvider::getProvider).collect(Collectors.toList());

    return remoteProvidersIds;
  }

  private List<String> getCurrentProvidersIdsWithSubscriptions(final FederationConfig fConfig) {
    final List<Subscription> currentRemoteSubscriptions = remotePlatformService.getSubscriptions(fConfig);

    final List<String> currentProvidersIdsWithSubscriptions =
        currentRemoteSubscriptions.stream().map(s -> s.getProvider().replaceAll("\\*", "")).collect(Collectors.toList());

    return currentProvidersIdsWithSubscriptions;
  }

  private List<String> subtract(final List<String> s1, final List<String> s2) {
    return s1.stream().filter(item -> !s2.contains(item)).collect(Collectors.toList());
  }

}
