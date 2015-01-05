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

import org.sentilo.platform.client.core.PlatformTemplate;
import org.sentilo.platform.client.core.domain.SubscribeInputMessage;
import org.sentilo.platform.client.core.domain.Subscription;
import org.sentilo.platform.client.core.domain.SubscriptionsOutputMessage;
import org.sentilo.web.catalog.domain.Application;
import org.sentilo.web.catalog.domain.Provider;
import org.sentilo.web.catalog.service.ApplicationService;
import org.sentilo.web.catalog.service.ProviderService;
import org.sentilo.web.catalog.service.SubscriptionService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class SubscriptionServiceImpl implements SubscriptionService {

  @Autowired
  private PlatformTemplate platformTemplate;

  @Autowired
  private ApplicationService applicationService;

  @Autowired
  private ProviderService providerService;

  @Override
  public List<Subscription> getSubscriptionsActivesByApplication(final String entityId) {
    final Application app = applicationService.find(new Application(entityId));
    return getSubscription(app.getToken());
  }

  @Override
  public List<Subscription> getSubscriptionsActivesByProvider(final String entityId) {
    final Provider provider = providerService.find(new Provider(entityId));
    return getSubscription(provider.getToken());
  }

  private List<Subscription> getSubscription(final String idToken) {
    final SubscribeInputMessage message = new SubscribeInputMessage();
    message.setIdentityToken(idToken);
    final SubscriptionsOutputMessage response = platformTemplate.getSubscribeOps().get(message);
    return response.getSubscriptions();
  }

}
