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
package org.sentilo.web.catalog.test.scheduler;

import static org.mockito.Matchers.anyListOf;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.sentilo.platform.client.core.domain.Subscription;
import org.sentilo.web.catalog.domain.ActiveSubscription;
import org.sentilo.web.catalog.domain.Application;
import org.sentilo.web.catalog.domain.Provider;
import org.sentilo.web.catalog.scheduler.ActiveSubscriptionsSynchronizationJob;
import org.sentilo.web.catalog.service.ActiveSubscriptionsService;
import org.sentilo.web.catalog.service.ApplicationService;
import org.sentilo.web.catalog.service.PlatformService;
import org.sentilo.web.catalog.service.ProviderService;

public class ActiveSubscriptionsSynchronizationJobTest {

  @InjectMocks
  private ActiveSubscriptionsSynchronizationJob job;

  @Mock
  private ProviderService providerService;

  @Mock
  private ApplicationService applicationService;

  @Mock
  private PlatformService platformService;

  @Mock
  private ActiveSubscriptionsService activeSubscriptionsService;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void syncActiveSubscriptionsTest1() {

    final List<Application> aMockList = new ArrayList<Application>();
    aMockList.add(Mockito.mock(Application.class));
    when(applicationService.findAll()).thenReturn(aMockList);

    final List<Provider> pMockList = new ArrayList<Provider>();
    pMockList.add(Mockito.mock(Provider.class));
    when(providerService.findAll()).thenReturn(pMockList);

    job.syncActiveSubscriptions();

    verify(providerService).findAll();
    verify(applicationService).findAll();
    verify(platformService, atLeast(2)).getActiveSubscriptions(anyString());
    verify(activeSubscriptionsService).replaceActiveSubscriptions(anyListOf(ActiveSubscription.class));
  }

  @Test
  public void syncActiveSubscriptionsTest2() {

    final List<Application> aMockList = new ArrayList<Application>();
    aMockList.add(Mockito.mock(Application.class));
    when(applicationService.findAll()).thenReturn(aMockList);

    final List<Provider> pMockList = new ArrayList<Provider>();
    pMockList.add(Mockito.mock(Provider.class));
    when(providerService.findAll()).thenReturn(pMockList);

    final List<Subscription> subscriptions = new ArrayList<Subscription>();
    subscriptions.add(Mockito.mock(Subscription.class));
    subscriptions.add(Mockito.mock(Subscription.class));
    subscriptions.add(Mockito.mock(Subscription.class));
    when(platformService.getActiveSubscriptions(anyString())).thenReturn(subscriptions);

    job.syncActiveSubscriptions();

    verify(providerService).findAll();
    verify(applicationService).findAll();
    verify(platformService, atLeast(2)).getActiveSubscriptions(anyString());
    verify(activeSubscriptionsService).replaceActiveSubscriptions(anyListOf(ActiveSubscription.class));
  }
}
