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

import javax.validation.Valid;

import org.sentilo.web.demo.business.service.RemoteService;
import org.sentilo.web.demo.common.domain.Order;
import org.sentilo.web.demo.common.utils.Constants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;


@Controller
@RequestMapping("/order") 
public class OrderController {
	
	@Autowired
	private RemoteService remoteService;
	
	private final Logger logger = LoggerFactory.getLogger(OrderController.class);
	
	/**
	 * Redirigeix a la pàgina de publicació d'una ordre nova
	 * Preinforma les dades.
	 * 
	 * @param model
	 * 			el model
	 * @return identificador de la vista a mostrar
	 */
	@RequestMapping(value = "/new", method = RequestMethod.GET)
	public String newAlarm(Model model) {
		
		Order order = new Order();
		order.setProviderId(Constants.DEFAULT_PROVIDER_ID);
		order.setTokenAuth(Constants.TOKEN_AUTH);
		order.setMessage(Constants.ORDER_MESSAGE);
		
		model.addAttribute(Constants.MODEL_ORDER, order);
		return Constants.VIEW_OR_CREATE;
	}
	

	/**
	 * Publica una ordre a la plataforma
	 * 
	 * @param order
	 * 			L'ordre a publicar
	 * @param result
	 * 			el resultat del binding
	 * @param model
	 * 			el model
	 * @return identificador de la vista a mostrar
	 */
	@RequestMapping(value = "/publishOrder", method = RequestMethod.POST)
	public String registerAndSend(@Valid Order order, BindingResult result, Model model) {
		if (result.hasErrors()) {
			return Constants.VIEW_OR_CREATE;
		}
		remoteService.publishOrder(order);
		logger.debug("Publishing new order" + order.getId());
		
		model.addAttribute(Constants.MODEL_ORDER, order);
		model.addAttribute(Constants.MODEL_MSG_SUCCESS, Constants.MODEL_CODE_ORDER_PUB);
		return Constants.VIEW_OR_CREATE;
	}
	
	/**
	 * Redirigeix a la pàgina de subscripció d'ordres
	 * 
	 * @param model
	 * 			el model
	 * @return identificador de la vista a mostrar
	 */
	@RequestMapping(value = "/subscribe", method = RequestMethod.GET)
	public String toSubscribe(Model model) {

		Order order = new Order();
		order.setProviderId(Constants.DEFAULT_PROVIDER_ID);
		order.setTokenAuth(Constants.TOKEN_AUTH);
		
		model.addAttribute(Constants.MODEL_ORDER, order);
		
		return Constants.VIEW_OR_SELECT_SUBSCRIPTION;
	}
	
	/**
	 * Es subscriu a una alarma
	 * 
	 * @param order 
	 * 			tipus d'ordre a subscirure's
	 * @param result
	 * 			el resultat del binding
	 * @param model
	 * 			el model
	 * @return identificador de la vista a mostrar
	 */
	@RequestMapping(value = "/subscribe", method = RequestMethod.POST)
	public String subscribeData(@Valid Order order, BindingResult result, Model model) {
		if (result.hasErrors()) {
			return Constants.VIEW_OR_SELECT_SUBSCRIPTION;
		}

		remoteService.subscribeOrder(order);
		logger.debug("Subscribe order: "+ order.getId());
		model.addAttribute(Constants.MODEL_ORDER, order);
		model.addAttribute(Constants.MODEL_URL, remoteService.getWsUrl());
			
		return Constants.VIEW_OR_SUBSCRIBE;
	}
	

}
