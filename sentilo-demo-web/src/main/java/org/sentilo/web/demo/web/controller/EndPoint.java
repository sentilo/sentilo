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
package org.sentilo.web.demo.web.controller;

import javax.servlet.http.HttpServletResponse;

import org.sentilo.web.demo.business.service.RemoteService;
import org.sentilo.web.demo.common.domain.Alarm;
import org.sentilo.web.demo.common.domain.EndpointMessage;
import org.sentilo.web.demo.common.domain.ObservationData;
import org.sentilo.web.demo.common.domain.Order;
import org.sentilo.web.demo.common.exception.DemoWebException;
import org.sentilo.web.demo.common.utils.ConvertUtils;
import org.sentilo.web.demo.websocket.SubscribeWebSocketServlet;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;


/**
 * 
 * Controlador que gestiona les dades que es publiquen des de la plataforma 
 * com a resultat d'una subscripció de l'aplicació.
 *
 */
@Controller
@RequestMapping("/publish")
public class EndPoint {
	
	private final Logger logger = LoggerFactory.getLogger(EndPoint.class);
	
	ConvertUtils convertUtils =  new ConvertUtils();
	
	@Autowired
	private SubscribeWebSocketServlet webSocketServlet;
	
	@Autowired
	private RemoteService remoteService;
	
	/**
	 * Rep una observació i la publica als websockets pertinents. 
	 * En cas de que no hi hagi cap websocket actiu, es desubscriu.
	 * 
	 * @param message El missatge que conté l'observació
	 */
	@RequestMapping(value = "/observations", method = RequestMethod.POST)
	public @ResponseBody String observations(@RequestBody String message, HttpServletResponse response) {
		
		EndpointMessage endpointMessage = (EndpointMessage)convertUtils.readInternal(EndpointMessage.class, message);
		logger.info("Recived DATA: " + endpointMessage);
		ObservationData observation = endpointMessage.getObservation();
		String returnMessage = "OK";
			
		try {
			boolean active = webSocketServlet.sendMessage(observation);
			if(!active){
	    		remoteService.unsubscribe(observation);
			}
			response.setStatus(HttpServletResponse.SC_OK);
		} catch (DemoWebException e) {
			logger.error(e.getMessage());
			response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
			returnMessage = e.getLocalizedMessage();
		}	
		
		return returnMessage;
		
	}
	
	/**
	 * Rep una alarma i la publica als websockets pertinents. 
	 * En cas de que no hi hagi cap websocket actiu, es desubscriu.
	 * 
	 * @param message El missatge que conté l'alarma
	 */
	@RequestMapping(value = "/alarm", method = RequestMethod.POST)
	public @ResponseBody String alarm(@RequestBody String message, HttpServletResponse response) {
		
		EndpointMessage endpointMessage = (EndpointMessage)convertUtils.readInternal(EndpointMessage.class, message);
		logger.info("Recived ALARM: " + endpointMessage);
		Alarm alarm = endpointMessage.getAlarm();
		String returnMessage = "OK";
	
		try {
			boolean active = webSocketServlet.sendMessage(alarm);
			if(!active){
				remoteService.unsubscribe(alarm);
			}
			response.setStatus(HttpServletResponse.SC_OK);
		} catch (DemoWebException e) {
			logger.error(e.getMessage());
			response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
			returnMessage = e.getLocalizedMessage();
		}
		return returnMessage;
	}
	
	/**
	 * Rep una ordre i la publica als websockets pertinents. 
	 * En cas de que no hi hagi cap websocket actiu, es desubscriu.
	 * 
	 * @param message El missatge que conté l'ordre
	 */
	@RequestMapping(value = "/order", method = RequestMethod.POST)
	public @ResponseBody String  order(@RequestBody String message, HttpServletResponse response) {
		
		EndpointMessage endpointMessage = (EndpointMessage)convertUtils.readInternal(EndpointMessage.class, message);		
		logger.info("Recived ORDER: " + endpointMessage);
		Order order = endpointMessage.getOrder();
		String returnMessage = "OK";

		try {
			boolean active = webSocketServlet.sendMessage(order);
			if(!active){
				remoteService.unsubscribe(order);
			}
			response.setStatus(HttpServletResponse.SC_OK);
		} catch (DemoWebException e) {
			logger.error(e.getMessage());
			response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
			returnMessage = e.getLocalizedMessage();
		}
		return returnMessage;
	}
}
