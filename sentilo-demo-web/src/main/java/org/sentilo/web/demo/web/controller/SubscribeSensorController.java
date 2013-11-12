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

import java.util.List;

import javax.validation.Valid;

import org.sentilo.web.demo.business.service.RemoteService;
import org.sentilo.web.demo.common.domain.Event;
import org.sentilo.web.demo.common.domain.ExternalSensor;
import org.sentilo.web.demo.common.domain.ObservationData;
import org.sentilo.web.demo.common.utils.Constants;
import org.sentilo.web.demo.websocket.SubscribeWebSocketServlet;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;



/**
 * 
 * Controlador per a manegar la recuperació de dades immediata i la subscripció.
 * Les dades obtingudes mitjançant la subscripció es maneguen a través de l'endpoint: 
 * {@link org.sentilo.web.demo.rest.EndPoint}
 */
@Controller
@RequestMapping("/sensordata")
public class SubscribeSensorController {
	
	private final Logger logger = LoggerFactory.getLogger(SubscribeSensorController.class);
	
	@Autowired
	private RemoteService remoteService;

	
	@ModelAttribute(Constants.MODEL_SENSOR_DATA_TYPES)
	public ExternalSensor.DataType[] getSensorDataTypes(Model model) {
		return ExternalSensor.DataType.values();
	}
	
	@RequestMapping(value = "/lastMessages", method = RequestMethod.POST)
	public @ResponseBody List<Event> last5(Model model) {
		return SubscribeWebSocketServlet.getLast5Events();
	}

	
	/**
	 * Redirigeix a la pàgina de subscripció i recuperació de dades
	 * 
	 * @param model
	 * 			el model
	 * @return identificador de la vista a mostrar
	 */
	@RequestMapping(value = "/subscribe", method = RequestMethod.GET)
	public String toSubscribe(Model model) {

		ExternalSensor externalsensor =  new ExternalSensor();

		externalsensor.setProviderId(Constants.DEFAULT_PROVIDER_ID);
		externalsensor.setId(Constants.DEFAULT_SENSOR_ID);
		externalsensor.setTokenAuth(Constants.TOKEN_AUTH);
		externalsensor.setNumber(Constants.EXT_SENSOR_NUM_REC);
		
		model.addAttribute(Constants.MODEL_EXTERNAL_SENSOR, externalsensor);
		return Constants.VIEW_RD_SELECT_SENSOR;
	}
	
	/**
	 * Recupera la darrera dada d'un sensor d'un altre proveidor
	 * 
	 * @param externalsensor 
	 * 			Les dades que identifiquen el sensor
	 * @param result
	 * 			el resultat del binding
	 * @param model
	 * 			el model
	 * @return identificador de la vista a mostrar
	 */
	@RequestMapping(value = "/recoverData", method = RequestMethod.POST)
	public String recoverData(@Valid ExternalSensor externalsensor, BindingResult result, Model model) {
		if (result.hasErrors()) {
			return Constants.VIEW_RD_SELECT_SENSOR;
		}
		List<ObservationData> observations = remoteService.recoverData(externalsensor);
		logger.debug("Recover data from sensor: "+ externalsensor.getId());
		if(!observations.isEmpty()){
			model.addAttribute(Constants.MODEL_EXTERNAL_SENSOR, externalsensor);
			model.addAttribute(Constants.MODEL_OBSERVATION, observations);
		}
		return Constants.VIEW_RD_LAST_DATA;
	}
	
	/**
	 * Es subscriu a un sensor d'un altre proveidor
	 * 
	 * @param externalsensor 
	 * 			Les dades que identifiquen el sensor
	 * @param result
	 * 			el resultat del binding
	 * @param model
	 * 			el model
	 * @return identificador de la vista a mostrar
	 */
	@RequestMapping(value = "/subscribeData", method = RequestMethod.POST)
	public String subscribeData(@Valid ExternalSensor externalsensor, BindingResult result, Model model) {
		if (result.hasErrors()) {
			return Constants.VIEW_RD_SELECT_SENSOR;
		}

		remoteService.subscribe(externalsensor);
		logger.debug("Subscribe sensor: "+ externalsensor.getId());
		model.addAttribute(Constants.MODEL_EXTERNAL_SENSOR, externalsensor);
		model.addAttribute(Constants.MODEL_URL, remoteService.getWsUrl());
			
		return Constants.VIEW_RD_SUBSCRIBE_DATA;
	}

}
