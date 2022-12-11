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
package org.sentilo.platform.client.core;

import javax.annotation.PostConstruct;

import org.sentilo.common.rest.RESTClient;
import org.sentilo.platform.client.core.service.AlarmServiceOperations;
import org.sentilo.platform.client.core.service.CatalogServiceOperations;
import org.sentilo.platform.client.core.service.DataServiceOperations;
import org.sentilo.platform.client.core.service.OrderServiceOperations;
import org.sentilo.platform.client.core.service.SubscribeServiceOperations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.Assert;

@Component
public class PlatformTemplate implements PlatformClientOperations {

  @Autowired
  private RESTClient client;
  @Autowired
  private DataServiceOperations dataOps;
  @Autowired
  private AlarmServiceOperations alarmOps;
  @Autowired
  private OrderServiceOperations orderOps;
  @Autowired
  private SubscribeServiceOperations subscribeOps;
  @Autowired
  private CatalogServiceOperations catalogOps;

  @PostConstruct
  public void init() {
    Assert.notNull(getClient(), "RESTClient is required");
  }

  public RESTClient getClient() {
    return client;
  }

  public DataServiceOperations getDataOps() {
    return dataOps;
  }

  public AlarmServiceOperations getAlarmOps() {
    return alarmOps;
  }

  public OrderServiceOperations getOrderOps() {
    return orderOps;
  }

  public SubscribeServiceOperations getSubscribeOps() {
    return subscribeOps;
  }

  public CatalogServiceOperations getCatalogOps() {
    return catalogOps;
  }

}
