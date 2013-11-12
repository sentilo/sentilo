/*
 * Sentilo
 *   
 * Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de  Barcelona.
 *   
 * This program is licensed and may be used, modified and redistributed under the
 * terms  of the European Public License (EUPL), either version 1.1 or (at your 
 * option) any later version as soon as they are approved by the European 
 * Commission.
 *   
 * Alternatively, you may redistribute and/or modify this program under the terms
 * of the GNU Lesser General Public License as published by the Free Software 
 * Foundation; either  version 3 of the License, or (at your option) any later 
 * version. 
 *   
 * Unless required by applicable law or agreed to in writing, software distributed
 * under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR 
 * CONDITIONS OF ANY KIND, either express or implied. 
 *   
 * See the licenses for the specific language governing permissions, limitations 
 * and more details.
 *   
 * You should have received a copy of the EUPL1.1 and the LGPLv3 licenses along 
 * with this program; if not, you may find them at: 
 *   
 *   https://joinup.ec.europa.eu/software/page/eupl/licence-eupl
 *   http://www.gnu.org/licenses/ 
 *   and 
 *   https://www.gnu.org/licenses/lgpl.txt
 */
package org.sentilo.platform.server.parser;

import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.http.HttpStatus;
import org.sentilo.platform.common.domain.Order;
import org.sentilo.platform.common.domain.OrderInputMessage;
import org.sentilo.platform.common.exception.PlatformException;
import org.sentilo.platform.server.dto.OrdersMessage;
import org.sentilo.platform.server.dto.SensorsOrdersMessage;
import org.sentilo.platform.server.request.SentiloRequest;
import org.sentilo.platform.server.request.SentiloResource;
import org.sentilo.platform.server.response.SentiloResponse;

import org.sentilo.common.domain.OrderMessage;
import org.sentilo.common.domain.SensorOrdersMessage;

public class OrderParser extends PlatformJsonMessageConverter {
		
	public OrderInputMessage parseRequest(SentiloRequest request) throws PlatformException{		
						
		OrderInputMessage inputMessage = (OrderInputMessage)readInternal(OrderInputMessage.class, request);		
				
		String providerId = request.getResourcePart(0);
		String sensorId = request.getResourcePart(1);
		
		inputMessage.setProviderId(providerId);
		inputMessage.setSensorId(sensorId);	
		inputMessage.setSender(request.getEntitySource());
		
		return inputMessage;		
	}	
	
	public OrderInputMessage parseGetRequest(SentiloRequest request) throws PlatformException{				
		SentiloResource resource = request.getResource();
		String providerId = resource.getResourcePart(0);
		String sensorId = resource.getResourcePart(1);		
		String from = request.getRequestParameter("from"); 
		String to = request.getRequestParameter("to");
		String limit = request.getRequestParameter("limit");
				
		return new OrderInputMessage(providerId, sensorId, parseDate(from), parseDate(to), parseInteger(limit));				
	}
	
	public void writeResponse(SentiloRequest request, SentiloResponse response, List<Order> orders) throws PlatformException{
		//transformar a objeto de tipo SensorsOrderMessage o OrdersMessage, depende del caso de la petición
		Object message = parseOrdersListToMessage(request, orders);
		
		try{
			writeInternal(message, response);
		}catch(IOException ex){
			throw new PlatformException(HttpStatus.SC_INTERNAL_SERVER_ERROR, ex);
		}				
	}
	
	private Object parseOrdersListToMessage(SentiloRequest request, List<Order> orders){
		SentiloResource resource = request.getResource();
		
		if(resource.getParts().length==1){						
			return parseOrdersListToSensorsOrderMessage(orders);
		}else{			
			return parseOrdersListToOrdersMessage(orders);			
		}
	}
	
	private SensorsOrdersMessage parseOrdersListToSensorsOrderMessage(List<Order> ordersList){
		SensorsOrdersMessage sensorsOrdersMessage = new SensorsOrdersMessage();
		Map<String, SensorOrdersMessage> sensorsOrders = new HashMap<String, SensorOrdersMessage>();
		
		for(Order order : ordersList){
			OrderMessage orderMessage = parseOrderToOrderMessage(order);
			SensorOrdersMessage sensorOrdersMessage = sensorsOrders.get(order.getSensor());
			if(sensorOrdersMessage == null){
				sensorOrdersMessage = new SensorOrdersMessage();
				sensorOrdersMessage.setSensor(order.getSensor());
				sensorsOrders.put(sensorOrdersMessage.getSensor(), sensorOrdersMessage);
				sensorsOrdersMessage.addSensorOrders(sensorOrdersMessage);
			}			
			sensorOrdersMessage.addOrderMessage(orderMessage);
		}
		
		return sensorsOrdersMessage;				
	}
			
	private OrdersMessage parseOrdersListToOrdersMessage(List<Order> ordersList){
		OrdersMessage orders = new OrdersMessage();
		for(Order order : ordersList){
			orders.addOrder(parseOrderToOrderMessage(order));
		}
		
		return orders;
	}
	
	private OrderMessage parseOrderToOrderMessage(Order order){
		OrderMessage message = new OrderMessage();
		message.setOrder(order.getMessage());	
		message.setSender(order.getSender());
		message.setTimestamp(timestampToString(order.getTimestamp()));		
		return message;
	}
	
	
	
}
