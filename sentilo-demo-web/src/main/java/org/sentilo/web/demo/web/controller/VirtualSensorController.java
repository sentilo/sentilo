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
import org.sentilo.web.demo.common.domain.ObservationData;
import org.sentilo.web.demo.common.domain.VirtualSensor;
import org.sentilo.web.demo.common.utils.Constants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;


/**
 * 
 * Controlador per a manegar els sensors virtuals. Tant del seu registre com l'enviament de dades.
 *
 */
@Controller
@RequestMapping("/virtualsensor")
public class VirtualSensorController {
		
	private final Logger logger = LoggerFactory.getLogger(VirtualSensorController.class);
	
	@Autowired
	private RemoteService remoteService;
	
	@ModelAttribute(Constants.MODEL_SENSOR_DATA_TYPES)
	public VirtualSensor.DataType[] getSensorDataTypes(Model model) {
		return VirtualSensor.DataType.values();
	}
	
	@ModelAttribute(Constants.MODEL_SENSOR_TYPES)
	public VirtualSensor.Type[] getSensorTypes(Model model) {
		return VirtualSensor.Type.values();
	}
	
	@ModelAttribute(Constants.MODEL_COMPONENT_TYPES)
	public VirtualSensor.ComponentType[] getComponentTypes(Model model) {
		return VirtualSensor.ComponentType.values();
	}
	
	
	/**
	 * Redirigeix a la pàgina de registre i enviament d'un sensor virutal.
	 * Preinforma les dades.
	 * 
	 * @param model
	 * 			el model
	 * @return identificador de la vista a mostrar
	 */
	@RequestMapping(value = "/new", method = RequestMethod.GET)
	public String newSensor(Model model) {

		VirtualSensor sensor = new VirtualSensor();
		sensor.setProviderId(Constants.DEFAULT_PROVIDER_ID);
		sensor.setSensorId(Constants.DEFAULT_SENSOR_ID);
		sensor.setDescription(Constants.SENSOR_DESCRIPTION);
		sensor.setUnit(Constants.SENSOR_UNIT);
		sensor.setTokenAuth(Constants.TOKEN_AUTH);
		sensor.setNumOfIterations(Constants.SENSOR_NUM_OF_ITERATIONS);
		sensor.setFreq(Constants.SENSOR_FREQ);	
		sensor.setValue(Constants.SENSOR_VALUE);
		sensor.setComponentId(Constants.DEFAULT_COMPONENT_ID);
		sensor.setComponentType(VirtualSensor.ComponentType.generic);
		
		model.addAttribute(Constants.MODEL_SENSOR, sensor);
		return Constants.VIEW_VS_CREATE;
	}
	
	/**
	 * Registra el sensor i a la pàgina d'enviament de dades.
	 * 
	 * @param sensor 
	 * 			el sensor virtual que defineix les dades a enviar
	 * @param result
	 * 			el resultat del binding
	 * @param model
	 * 			el model
	 * @return identificador de la vista a mostrar
	 */
	@RequestMapping(value = "/registerAndSend", method = RequestMethod.POST)
	public String registerAndSend(@Valid VirtualSensor virtualSensor, BindingResult result, Model model) {
		if (result.hasErrors()) {
			return Constants.VIEW_VS_CREATE;
		}
		remoteService.createSensor(virtualSensor);
		logger.debug("Creating new virtual sensor " + virtualSensor.getSensorId());
		
		model.addAttribute(Constants.MODEL_SENSOR, virtualSensor);
		return Constants.VIEW_VS_SEND_DATA;
	}
	
	/**
	 * Redirigeix a la pàgina d'enviament de dades.
	 * 
	 * @param sensor 
	 * 			el sensor virtual que defineix les dades a enviar
	 * @param result
	 * 			el resultat del binding
	 * @param model
	 * 			el model
	 * @return identificador de la vista a mostrar
	 */
	@RequestMapping(value = "/toSend", method = RequestMethod.POST)
	public String toSend(@Valid VirtualSensor virtualSensor, BindingResult result, Model model) {
		if (result.hasErrors()) {
			return Constants.VIEW_VS_CREATE;
		}
		
		model.addAttribute(Constants.MODEL_SENSOR, virtualSensor);
		return Constants.VIEW_VS_SEND_DATA;
	}
	
	
	/**
	 * Envia una dada del sensor virtual
	 * 
	 * @param sensorId 
	 * 			id sensor per el que s'envia la dada
	 * @param providerId
	 * 			id sensor per el que s'envia la dada
	 * @param tokenAuth
	 * 			token d'autorització del proveidor
	 * @param value
	 * 			el valor de la dada
	 * @param model
	 * 			el model
	 * @return identificador de la vista a mostrar
	 */
	@RequestMapping(value = "/send/{sensorId}/{providerId}/{tokenAuth}/{value}", method = RequestMethod.POST)
	public @ResponseBody String sendData(@PathVariable String sensorId, @PathVariable String providerId, 
									@PathVariable String tokenAuth,	@PathVariable String value, Model model){

		ObservationData observation =  new ObservationData(sensorId, providerId, value);
		observation.setTokenAuth(tokenAuth);
		try{
			remoteService.sendData(observation);
		}catch(Throwable th){
			return th.getMessage();
		}
		
		logger.debug("Sending data for sensor " + sensorId);
		return Constants.RETURN_OK;
	}

	public RemoteService getRemoteService() {
		return remoteService;
	}

	public void setRemoteService(RemoteService remoteService) {
		this.remoteService = remoteService;
	}


}
