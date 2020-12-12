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
package org.sentilo.web.catalog.scheduler;

import java.util.ArrayList;
import java.util.List;

import org.sentilo.platform.client.core.domain.Subscription;
import org.sentilo.web.catalog.domain.ActiveSubscription;
import org.sentilo.web.catalog.domain.Application;
import org.sentilo.web.catalog.domain.Provider;
import org.sentilo.web.catalog.domain.TenantResource;
import org.sentilo.web.catalog.service.ActiveSubscriptionsService;
import org.sentilo.web.catalog.service.ApplicationService;
import org.sentilo.web.catalog.service.PlatformService;
import org.sentilo.web.catalog.service.ProviderService;
import org.sentilo.web.catalog.utils.enums.EntityType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

@Component
public class ActiveSubscriptionsSynchronizationJob {

  private static final Logger LOGGER = LoggerFactory.getLogger(ActiveSubscriptionsSynchronizationJob.class);

  private static final long INITIAL_DELAY = 60000; // Run 1 minute after application starts
  private static final long FIXED_DELAY = 600000; // Every 10 minutes

  @Autowired
  private ProviderService providerService;

  @Autowired
  private ApplicationService applicationService;

  @Autowired
  private PlatformService platformService;

  @Autowired
  private ActiveSubscriptionsService activeSubscriptionsService;

  @Scheduled(initialDelay = INITIAL_DELAY, fixedDelay = FIXED_DELAY)
  public void syncActiveSubscriptions() {
    LOGGER.info("Init sync process for replace active subscriptions");

    final List<ActiveSubscription> activeSubscriptions = new ArrayList<ActiveSubscription>();

    // Get active subscriptions for all providers
    final List<Provider> providers = providerService.findAll();
    for (final Provider provider : providers) {
      activeSubscriptions.addAll(getEntitySubscriptions(provider, EntityType.PROVIDER));
    }

    // Get active subscriptions for all applications
    final List<Application> applications = applicationService.findAll();
    for (final Application application : applications) {
      activeSubscriptions.addAll(getEntitySubscriptions(application, EntityType.APPLICATION));
    }

    // update all active subscriptions in catalog
    activeSubscriptionsService.replaceActiveSubscriptions(activeSubscriptions);

    LOGGER.info("Ended sync process for current active subscriptions. Number of subscriptions: {}", activeSubscriptions.size());
  }

  private List<ActiveSubscription> getEntitySubscriptions(final TenantResource entity, final EntityType entityType) {
    final List<Subscription> subscriptions = platformService.getActiveSubscriptions(entity.getId());

    final List<ActiveSubscription> list = new ArrayList<ActiveSubscription>();
    if (!CollectionUtils.isEmpty(subscriptions)) {
      for (final Subscription subscription : subscriptions) {
        final ActiveSubscription activeSubscription = new ActiveSubscription(entity, entityType, subscription);
        list.add(activeSubscription);
      }
    }
    return list;
  }

}
