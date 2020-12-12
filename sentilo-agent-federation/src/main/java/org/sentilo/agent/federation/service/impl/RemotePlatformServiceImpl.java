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

import java.util.Collections;
import java.util.List;

import org.sentilo.agent.federation.domain.FederationConfig;
import org.sentilo.agent.federation.service.RemotePlatformService;
import org.sentilo.common.converter.DefaultStringMessageConverter;
import org.sentilo.common.converter.StringMessageConverter;
import org.sentilo.common.domain.AuthorizedProvider;
import org.sentilo.common.domain.CatalogResponseMessage;
import org.sentilo.common.enums.SubscribeType;
import org.sentilo.common.exception.RESTClientException;
import org.sentilo.common.rest.RESTClient;
import org.sentilo.common.rest.RequestContext;
import org.sentilo.platform.client.core.domain.SubscribeInputMessage;
import org.sentilo.platform.client.core.domain.Subscription;
import org.sentilo.platform.client.core.domain.SubscriptionParams;
import org.sentilo.platform.client.core.domain.SubscriptionsOutputMessage;
import org.sentilo.platform.client.core.domain.factory.SubscribeInputMessageFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

@Service
public class RemotePlatformServiceImpl implements RemotePlatformService {

  private static final Logger LOGGER = LoggerFactory.getLogger(RemotePlatformServiceImpl.class);

  private final StringMessageConverter converter = new DefaultStringMessageConverter();

  @Autowired
  @Qualifier("remoteRestClient")
  private RESTClient remoteRestClient;

  @Value("${federation.subscription.endpoint}")
  private String endpoint;

  @Value("${federation.subscription.secret.key.callback}")
  private String secretKey;

  @Value("${federation.subscription.max.retries}")
  private int maxRetries;

  @Value("${federation.subscription.max.delay}")
  private int maxDelay;

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.agent.federation.service.RemotePlatformService#getPermissions(org.sentilo.agent.
   * federation.domain.FederationConfig)
   */
  public List<AuthorizedProvider> getPermissions(final FederationConfig fConfig) {
    final String path = String.format("catalog");
    final RequestContext rc = new RequestContext(path);
    rc.setHost(fConfig.getSourceEndpoint());
    rc.setIdentityToken(fConfig.getAppClientToken());
    final String response = remoteRestClient.get(rc);
    final List<AuthorizedProvider> result = ((CatalogResponseMessage) converter.unmarshal(response, CatalogResponseMessage.class)).getProviders();
    return result != null ? result : Collections.emptyList();
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.agent.federation.service.RemotePlatformService#getSubscriptions(org.sentilo.agent.
   * federation.domain.FederationConfig)
   */
  public List<Subscription> getSubscriptions(final FederationConfig fConfig) {
    final String path = String.format("subscribe");
    final RequestContext rc = new RequestContext(path);
    rc.setHost(fConfig.getSourceEndpoint());
    rc.setIdentityToken(fConfig.getAppClientToken());
    final String response = remoteRestClient.get(rc);
    final List<Subscription> result =
        ((SubscriptionsOutputMessage) converter.unmarshal(response, SubscriptionsOutputMessage.class)).getSubscriptions();
    return result != null ? result : Collections.emptyList();
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.agent.federation.service.RemotePlatformService#createSubscriptions(java.util.List,
   * org.sentilo.agent.federation.domain.FederationConfig)
   */
  public void createSubscriptions(final List<String> providersIds, final FederationConfig fConfig) {
    providersIds.stream().forEach(providerId -> createSubscription(providerId, fConfig));
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.agent.federation.service.RemotePlatformService#deleteSubscriptions(java.util.List,
   * org.sentilo.agent.federation.domain.FederationConfig)
   */
  public void deleteSubscriptions(final List<String> providersIds, final FederationConfig fConfig) {
    providersIds.forEach(providerId -> deleteSubscription(providerId, fConfig));
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.agent.federation.service.RemotePlatformService#deleteSubscriptions(org.sentilo.
   * agent. federation.domain.FederationConfig)
   */
  public boolean deleteSubscriptions(final FederationConfig fConfig) {
    boolean success = false;
    try {
      final String path = String.format("subscribe");
      final RequestContext rc = new RequestContext(path);
      rc.setHost(fConfig.getSourceEndpoint());
      rc.setIdentityToken(fConfig.getAppClientToken());
      remoteRestClient.delete(rc);
      success = true;
    } catch (final RESTClientException e) {
      LOGGER.warn("An error has ocurred trying to delete all remote subscriptions from remote Sentilo instance {}. It will be retried later",
          fConfig.getId());
    }
    return success;
  }

  private void createSubscription(final String providerId, final FederationConfig fConfig) {
    try {
      final SubscriptionParams params = new SubscriptionParams(String.format("%s%s", endpoint, fConfig.getId()), secretKey, maxRetries, maxDelay);
      final SubscribeInputMessage message = SubscribeInputMessageFactory.buildSubscription(SubscribeType.DATA, params, providerId);

      final String path = String.format("subscribe/%s/%s", SubscribeType.DATA.name().toLowerCase(), providerId);
      final RequestContext rc = new RequestContext(path, converter.marshal(message.getSubscriptionParams()));
      rc.setHost(fConfig.getSourceEndpoint());
      rc.setIdentityToken(fConfig.getAppClientToken());
      remoteRestClient.put(rc);
    } catch (final RESTClientException e) {
      LOGGER.warn("An error has ocurred trying to create subscription for provider {} into remote Sentilo instance {}. It will be retried later",
          providerId, fConfig.getId(), e);
    }
  }

  private void deleteSubscription(final String providerId, final FederationConfig fConfig) {
    try {
      final String path = String.format("subscribe/%s/%s", SubscribeType.DATA.name().toLowerCase(), providerId);
      final RequestContext rc = new RequestContext(path);
      rc.setHost(fConfig.getSourceEndpoint());
      rc.setIdentityToken(fConfig.getAppClientToken());
      remoteRestClient.delete(rc);
    } catch (final RESTClientException e) {
      LOGGER.warn("An error has ocurred trying to delete subscription for provider {} into remote Sentilo instance {}. It will be retried later",
          providerId, fConfig.getId());
    }
  }

}
