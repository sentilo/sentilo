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
package org.sentilo.platform.client.core.service.impl;

import org.sentilo.platform.client.core.domain.OrderInputMessage;
import org.sentilo.platform.client.core.domain.OrdersOutputMessage;
import org.sentilo.platform.client.core.parser.OrderMessageConverter;
import org.sentilo.platform.client.core.service.OrderServiceOperations;
import org.sentilo.platform.client.core.utils.RequestUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

@Service
public class DefaultOrderServiceOperationsImpl extends AbstractServiceOperationsImpl implements OrderServiceOperations {

  private final Logger logger = LoggerFactory.getLogger(DefaultOrderServiceOperationsImpl.class);

  private OrderMessageConverter converter = new OrderMessageConverter();

  @Override
  public void publish(final OrderInputMessage message) {
    logger.debug("Publishing order message {}", message);
    getRestClient().put(RequestUtils.buildPath(message), converter.marshall(message), message.getIdentityToken());
    logger.debug("Order published ");
  }

  @Override
  public OrdersOutputMessage getLastOrders(final OrderInputMessage message) {
    logger.debug("Retrieving last orders  {}", message);
    final String response = getRestClient().get(RequestUtils.buildPath(message), RequestUtils.buildParameters(message), message.getIdentityToken());
    logger.debug("Retrieved last orders");
    return converter.unmarshall(response);
  }
}
