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
package org.sentilo.platform.server.converter;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.sentilo.common.domain.OrderMessage;
import org.sentilo.common.domain.SensorOrdersMessage;
import org.sentilo.platform.common.domain.Order;
import org.sentilo.platform.common.domain.OrderInputMessage;
import org.sentilo.platform.server.dto.OrdersMessage;
import org.sentilo.platform.server.dto.SensorsOrdersMessage;
import org.sentilo.platform.server.request.SentiloRequest;
import org.sentilo.platform.server.request.SentiloResource;
import org.sentilo.platform.server.response.SentiloResponse;

public class OrderConverter extends PlatformJsonMessageConverter {

  public OrderInputMessage parseRequest(final SentiloRequest request) {

    final OrderInputMessage inputMessage = (OrderInputMessage) readInternal(OrderInputMessage.class, request);

    final String providerId = request.getResourcePart(0);
    final String sensorId = request.getResourcePart(1);

    if (inputMessage != null) {
      inputMessage.setProviderId(providerId);
      inputMessage.setSensorId(sensorId);
      inputMessage.setSender(request.getEntitySource());
    }

    return inputMessage;
  }

  public OrderInputMessage parseGetRequest(final SentiloRequest request) {
    final SentiloResource resource = request.getResource();
    final String providerId = resource.getResourcePart(0);
    final String sensorId = resource.getResourcePart(1);
    final String from = request.getRequestParameter("from");
    final String to = request.getRequestParameter("to");
    final String limit = request.getRequestParameter("limit");

    return new OrderInputMessage(providerId, sensorId, parseDate(from), parseDate(to), parseInteger(limit));
  }

  public void writeResponse(final SentiloRequest request, final SentiloResponse response, final List<Order> orders) {
    // transformar a objeto de tipo SensorsOrderMessage o OrdersMessage, depende del caso de la
    // petición
    final Object message = parseOrdersListToMessage(request, orders);
    writeInternal(message, response);
  }

  private Object parseOrdersListToMessage(final SentiloRequest request, final List<Order> orders) {
    final SentiloResource resource = request.getResource();

    if (resource.getParts().length == 1) {
      return parseOrdersListToSensorsOrderMessage(orders);
    } else {
      return parseOrdersListToOrdersMessage(orders);
    }
  }

  private SensorsOrdersMessage parseOrdersListToSensorsOrderMessage(final List<Order> ordersList) {
    final SensorsOrdersMessage sensorsOrdersMessage = new SensorsOrdersMessage();
    final Map<String, SensorOrdersMessage> sensorsOrders = new HashMap<String, SensorOrdersMessage>();

    for (final Order order : ordersList) {
      final OrderMessage orderMessage = parseOrderToOrderMessage(order);
      SensorOrdersMessage sensorOrdersMessage = sensorsOrders.get(order.getSensor());
      if (sensorOrdersMessage == null) {
        sensorOrdersMessage = new SensorOrdersMessage();
        sensorOrdersMessage.setSensor(order.getSensor());
        sensorsOrders.put(sensorOrdersMessage.getSensor(), sensorOrdersMessage);
        sensorsOrdersMessage.addSensorOrders(sensorOrdersMessage);
      }
      sensorOrdersMessage.addOrderMessage(orderMessage);
    }

    return sensorsOrdersMessage;
  }

  private OrdersMessage parseOrdersListToOrdersMessage(final List<Order> ordersList) {
    final OrdersMessage orders = new OrdersMessage();
    for (final Order order : ordersList) {
      orders.addOrder(parseOrderToOrderMessage(order));
    }

    return orders;
  }

  private OrderMessage parseOrderToOrderMessage(final Order order) {
    final OrderMessage message = new OrderMessage();
    message.setOrder(order.getMessage());
    message.setSender(order.getSender());
    message.setTimestamp(timestampToString(order.getTimestamp()));
    message.setTime(order.getTimestamp());
    return message;
  }

}
