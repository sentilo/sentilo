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
package org.sentilo.agent.federation.job;

import java.util.List;

import org.sentilo.agent.federation.domain.FederationConfig;
import org.sentilo.agent.federation.service.LocalPlatformService;
import org.sentilo.agent.federation.service.SubscriptionService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

@Component
public class SubscriptionsJob {

  private static final Logger LOGGER = LoggerFactory.getLogger(SubscriptionsJob.class);

  final static int INITIAL_DELAY = 30 * 1000; // 30 seconds
  final static int FIXED_DELAY = 10 * 60 * 1000; // 10 minutes

  @Autowired
  private SubscriptionService subscriptionService;

  @Autowired
  private LocalPlatformService federationConfigService;

  @Scheduled(initialDelay = INITIAL_DELAY, fixedDelay = FIXED_DELAY)
  public void run() {
    LOGGER.info("Running sentilo's federation process to synchronize subscriptions");
    final List<FederationConfig> federatedConfigs = federationConfigService.getFederatedConfigs();
    federatedConfigs.forEach(federatedConfig -> {
      subscriptionService.synchronize(federatedConfig);
    });
  }
}
