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
package org.sentilo.web.demo.websocket;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;

import org.apache.catalina.websocket.MessageInbound;
import org.apache.catalina.websocket.StreamInbound;
import org.apache.catalina.websocket.WebSocketServlet;
import org.apache.catalina.websocket.WsOutbound;
import org.sentilo.web.demo.common.domain.Alarm;
import org.sentilo.web.demo.common.domain.Event;
import org.sentilo.web.demo.common.domain.ObservationData;
import org.sentilo.web.demo.common.domain.Order;
import org.sentilo.web.demo.common.exception.DemoWebException;
import org.sentilo.web.demo.common.utils.Constants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Repository;


/**
 * 
 * S'encarrega del manegament dels websockets entre l'aplicació i les pantalles.
 *
 */
@WebServlet(urlPatterns = "/subscribe")
@Repository
public class SubscribeWebSocketServlet extends WebSocketServlet {

	private static final long serialVersionUID = 3577246051725688774L;
	
	private static final Logger logger = LoggerFactory.getLogger(WebSocketServlet.class);
	
	private static final List<WebSocketConnection> sensor_conn = new ArrayList<WebSocketConnection>();
	private static final List<WebSocketConnection> alarm_conn = new ArrayList<WebSocketConnection>();
	private static final List<WebSocketConnection> order_conn = new ArrayList<WebSocketConnection>();

	private static List<Event> events = new ArrayList<Event>();

	@Override
	protected StreamInbound createWebSocketInbound(String arg0,
			HttpServletRequest request) {
		
		final String type = request.getParameter(Constants.WEBSOCKET_TYPE);
    	if(Constants.WEBSOCKET_TYPE_SENSOR.equals(type)){
            final String connectionId = request.getParameter(Constants.WEBSOCKET_SENS_ID) + "_" + request.getParameter(Constants.WEBSOCKET_PROV_ID);
            return new WebSocketConnection(connectionId, type);
    	}else if(Constants.WEBSOCKET_TYPE_ALARM.equals(type)){
            final String connectionId = request.getParameter(Constants.WEBSOCKET_ALARM_ID);
            return new WebSocketConnection(connectionId, type);
    	}else{
            final String connectionId = request.getParameter(Constants.WEBSOCKET_PROV_ID);
            return new WebSocketConnection(connectionId, type);
    	}

	}
	
	/**
	 * Métode que envia les dades d'una observació a tots els websockets que estiguin
	 * subscrits per al sensor i proveidor que ha generat la dada.
	 * 
	 * @param observation 
	 * 		La dada a enviar als websocket
	 * @return 
	 * 		retorna si hi ha algú subscrit
	 * @throws DemoWebException
	 */
	public boolean sendMessage(ObservationData observation) throws DemoWebException{
		final String connectionId = observation.getSensorId() + "_" + observation.getProviderId();
		boolean hasActiveConn = false;
		for(WebSocketConnection wsc: sensor_conn){

			if(connectionId.equals(wsc.getConnectionId())){
				try {
					wsc.sendMessage(observation.getValue());
					saveEvent(observation);
					hasActiveConn = true;
				} catch (IOException e) {
					logger.error("Problema de comunicación con webSocket: " + e.getMessage(), e.getCause());
					new DemoWebException("Problema de comunicación con webSocket: " + e.getMessage(), e.getCause());
				}
			}
		}
		return hasActiveConn;
	}
	
	/**
	 * Métode que envia les dades d'una alarma a tots els websockets que estiguin
	 * subscrits per a aquest tipus d'alarma
	 * 
	 * @param alarm 
	 * 		La alarma a enviar als websocket
	 * @return 
	 * 		retorna si hi ha algú subscrit
	 * @throws DemoWebException
	 */
	public boolean sendMessage(Alarm alarm) throws DemoWebException{
		final String connectionId = alarm.getAlarmId();
		boolean hasActiveConn = false;
		for(WebSocketConnection wsc: alarm_conn){

			if(connectionId.equals(wsc.getConnectionId())){
				try {
					wsc.sendMessage(alarm.getMessage());
					saveEvent(alarm);
					hasActiveConn = true;
				} catch (IOException e) {
					logger.error("Problema de comunicación con webSocket: " + e.getMessage(), e.getCause());
					new DemoWebException("Problema de comunicación con webSocket: " + e.getMessage(), e.getCause());
				}
			}
		}
		return hasActiveConn;
	}
	
	/**
	 * Métode que envia les dades d'una ordre a tots els websockets que estiguin
	 * subscrits per a les ordres d'un proveidor.
	 * 
	 * @param order 
	 * 		L'ordrea enviar als websocket
	 * @return 
	 * 		retorna si hi ha algú subscrit
	 * @throws DemoWebException
	 */
	public boolean sendMessage(Order order) throws DemoWebException{
		final String connectionId = order.getProviderId();
		boolean hasActiveConn = false;
		for(WebSocketConnection wsc: order_conn){

			if(connectionId.equals(wsc.getConnectionId())){
				try {
					wsc.sendMessage(order.getValue());
					saveEvent(order);
					hasActiveConn = true;
				} catch (IOException e) {
					logger.error("Problema de comunicación con webSocket: " + e.getMessage(), e.getCause());
					new DemoWebException("Problema de comunicación con webSocket: " + e.getMessage(), e.getCause());
				}
			}
		}
		return hasActiveConn;
	}
	
	private static void saveEvent(Event event){
		if (events.size()==20){
			events = events.subList(1, events.size()-1);
			events.add(event);
		}else{
			events.add(event);
		}
	}
	
	
	/**
	 * Retorna els 5 últims esdeveniments que han succeit 
	 * 
	 * @return els 5 útlims esdeveniments
	 */
	public static List<Event> getLast5Events() {
		if (events.size()>5){
			return events.subList(events.size()-5, events.size());
		}
		return events;
	}
	
	/**
	 * Retorna els 20 últims esdeveniments que han succeit 
	 * 
	 * @return els 20 útlims esdeveniments
	 */
	public static List<Event> getLast20Events() {
		return events;
	}

		private static class WebSocketConnection extends MessageInbound {
	    	
	        private final String connectionId;
	        private final String type;
	 
	        private WebSocketConnection(String connectionId, String type) {
	            this.connectionId = connectionId;
	            this.type = type;
	        }	     
	 
	        @Override
	        protected void onOpen(WsOutbound outbound) {
	        	if(Constants.WEBSOCKET_TYPE_SENSOR.equals(type)){
	        		sensor_conn.add(this);
	        	}else if(Constants.WEBSOCKET_TYPE_ALARM.equals(type)){
	        		alarm_conn.add(this);
	        	}else{
	        		order_conn.add(this);
	        	}
	            logger.info("Conexión abierta");
	        }
	 
	        @Override
	        protected void onClose(int status) {
	        	if(Constants.WEBSOCKET_TYPE_SENSOR.equals(type)){
	        		sensor_conn.remove(this);
	        	}else if(Constants.WEBSOCKET_TYPE_ALARM.equals(type)){
	        		alarm_conn.remove(this);
	        	}else{
	        		order_conn.remove(this);
	        	}
	        	logger.info("Conexión cerrada");
	        }
	 
	        @Override
	        protected void onBinaryMessage(ByteBuffer byteBuffer) throws IOException {
	        	logger.warn("No se soportan mensajes binarios");
	            throw new UnsupportedOperationException("No se soportan mensajes binarios");
	        }
	 
	        @Override
	        protected void onTextMessage(CharBuffer charBuffer) throws IOException {
	            final String user = charBuffer.toString();
	            logger.debug("Mensaje recibido: {}", user);
	        }
	        
	        public void sendMessage(String data) throws IOException{
	            logger.info("Writting to WS: {}", data);
	            getWsOutbound().writeTextMessage(CharBuffer.wrap(data));
	            logger.info("Writting to WS OK: {}", data);
	        }

			public String getConnectionId() {
				return connectionId;
			}
	        
	    }
	
	
}
