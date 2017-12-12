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

import java.util.List;

import org.sentilo.common.converter.DefaultStringMessageConverter;
import org.sentilo.common.converter.StringMessageConverter;
import org.sentilo.common.domain.PlatformMetricsMessage;
import org.sentilo.common.exception.RESTClientException;
import org.sentilo.common.rest.RESTClient;
import org.sentilo.common.rest.RequestContext;
import org.sentilo.platform.client.core.domain.Subscription;
import org.sentilo.platform.client.core.domain.SubscriptionsOutputMessage;
import org.sentilo.web.catalog.domain.PlatformAdminInputMessage;
import org.sentilo.web.catalog.domain.PlatformStatsMessage;
import org.sentilo.web.catalog.service.PlatformService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class PlatformServiceImpl implements PlatformService {

  @Autowired
  private RESTClient restClient;

  private final StringMessageConverter parser = new DefaultStringMessageConverter();

  @Override
  public PlatformStatsMessage getCurrentPlatformStats() {
    final RequestContext rc = new RequestContext("admin/stats");
    final String response = restClient.get(rc);
    return (PlatformStatsMessage) parser.unmarshal(response, PlatformStatsMessage.class);
  }

  @Override
  public PlatformMetricsMessage getPlatformActivity() {
    final RequestContext rc = new RequestContext("admin/activity");
    final String response = restClient.get(rc);
    return (PlatformMetricsMessage) parser.unmarshal(response, PlatformMetricsMessage.class);
  }

  @Override
  public PlatformMetricsMessage getPlatformPerformance() {
    final RequestContext rc = new RequestContext("admin/performance");
    final String response = restClient.get(rc);
    return (PlatformMetricsMessage) parser.unmarshal(response, PlatformMetricsMessage.class);
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.web.catalog.service.PlatformService#saveResources(org.sentilo.web.catalog.domain.
   * PlatformAdminInputMessage)
   */
  public void saveResources(final PlatformAdminInputMessage message) {
    final RequestContext rc = new RequestContext("admin/save", parser.marshal(message));
    restClient.post(rc);
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.web.catalog.service.PlatformService#deleteResources(org.sentilo.web.catalog.domain.
   * PlatformAdminInputMessage)
   */
  public void deleteResources(final PlatformAdminInputMessage message) {
    final RequestContext rc = new RequestContext("admin/delete", parser.marshal(message));
    restClient.put(rc);
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.service.PlatformService#getActiveSubscriptions(java.lang.String)
   */
  public List<Subscription> getActiveSubscriptions(final String entity) {
    final String path = String.format("admin/subscriptions/%s", entity);
    final RequestContext rc = new RequestContext(path);
    final String response = restClient.get(rc);
    final SubscriptionsOutputMessage som = (SubscriptionsOutputMessage) parser.unmarshal(response, SubscriptionsOutputMessage.class);
    return som.getSubscriptions();
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.service.PlatformService#isPlatformRunning()
   */
  public boolean isPlatformRunning() {
    boolean isRunning = true;

    try {
      final RequestContext rc = new RequestContext("admin/ping");
      rc.setIdentityToken("******");
      restClient.get(rc);
    } catch (final RESTClientException rce) {
      // Expected error is an Invalid token, i.e. status = 401
      isRunning = rce.getStatus() == 401 ? true : false;
    }

    return isRunning;
  }
}
