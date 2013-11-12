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
import org.sentilo.web.demo.common.domain.Alarm;
import org.sentilo.web.demo.common.utils.Constants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;


/**
 * 
 * Controlador per a manegar les alarmes. 
 * Es tracta tant la part de publicació d'alarmes com la de recuperació.
 * 
 */
@Controller
@RequestMapping("/alarm") 
public class AlarmController {
	
	private final Logger logger = LoggerFactory.getLogger(AlarmController.class);
	
	@Autowired
	private RemoteService remoteService;
	
	/**
	 * Redirigeix a la pàgina d'alta d'una alarma nova
	 * Preinforma les dades.
	 * 
	 * @param model
	 * 			el model
	 * @return identificador de la vista a mostrar
	 */
	@RequestMapping(value = "/new", method = RequestMethod.GET)
	public String newAlarm(Model model) {

		Alarm alarm = new Alarm();
		alarm.setAlarmId(Constants.ALARM_ID);
		alarm.setMessage(Constants.ALARM_MESSAGE);
		alarm.setTokenAuth(Constants.TOKEN_AUTH);
		
		model.addAttribute(Constants.MODEL_ALARM, alarm);
		return Constants.VIEW_AL_CREATE;
	}
	

	/**
	 * Publica una alarma a la plataforma
	 * 
	 * @param alarm
	 * 			L'alarma a publicar
	 * @param result
	 * 			el resultat del binding
	 * @param model
	 * 			el model
	 * @return identificador de la vista a mostrar
	 */
	@RequestMapping(value = "/publishAlarm", method = RequestMethod.POST)
	public String registerAndSend(@Valid Alarm alarm, BindingResult result, Model model) {
		if (result.hasErrors()) {
			return Constants.VIEW_AL_CREATE;
		}
		remoteService.publishAlarm(alarm);
		logger.debug("Publishing new alarm" + alarm.getAlarmId());
		
		model.addAttribute(Constants.MODEL_ALARM, alarm);
		model.addAttribute(Constants.MODEL_MSG_SUCCESS, Constants.MODEL_CODE_ALARM_PUB);
		return Constants.VIEW_AL_CREATE;
	}
	
	/**
	 * Redirigeix a la pàgina de subscripció d'alarmes
	 * 
	 * @param model
	 * 			el model
	 * @return identificador de la vista a mostrar
	 */
	@RequestMapping(value = "/subscribe", method = RequestMethod.GET)
	public String toSubscribe(Model model) {

		Alarm alarm = new Alarm();
		alarm.setAlarmId(Constants.ALARM_ID);
		alarm.setTokenAuth(Constants.TOKEN_AUTH);
		
		model.addAttribute(Constants.MODEL_ALARM, alarm);
		
		return Constants.VIEW_AL_SELECT_SUBSCRIPTION;
	}
	
	/**
	 * Es subscriu a una alarma
	 * 
	 * @param alarm 	
	 * 			tipus d'alarma a subscirure's
	 * @param result
	 * 			el resultat del binding
	 * @param model
	 * 			el model
	 * @return identificador de la vista a mostrar
	 */
	@RequestMapping(value = "/subscribe", method = RequestMethod.POST)
	public String subscribeData(@Valid Alarm alarm, BindingResult result, Model model) {
		if (result.hasErrors()) {
			return Constants.VIEW_AL_SELECT_SUBSCRIPTION;
		}

		remoteService.subscribeAlarm(alarm);
		logger.debug("Subscribe alarm: "+ alarm.getId());
		model.addAttribute(Constants.MODEL_ALARM, alarm);
		model.addAttribute(Constants.MODEL_URL, remoteService.getWsUrl());
			
		return Constants.VIEW_AL_SUBSCRIBE;
	}

}
