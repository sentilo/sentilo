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
package org.sentilo.web.catalog.controller.api;

import java.util.List;

import org.sentilo.common.domain.AlertsOwnersResponseMessage;
import org.sentilo.common.domain.AuthorizedProvider;
import org.sentilo.common.domain.CatalogDeleteInputMessage;
import org.sentilo.common.domain.CatalogInputMessage;
import org.sentilo.common.domain.CatalogResponseMessage;
import org.sentilo.web.catalog.converter.ApiConverter;
import org.sentilo.web.catalog.converter.ApiConverterContext;
import org.sentilo.web.catalog.domain.Alarm;
import org.sentilo.web.catalog.domain.Alarm.Type;
import org.sentilo.web.catalog.domain.Component;
import org.sentilo.web.catalog.domain.Permissions;
import org.sentilo.web.catalog.domain.Provider;
import org.sentilo.web.catalog.domain.Sensor;
import org.sentilo.web.catalog.dto.CredentialsDTO;
import org.sentilo.web.catalog.service.AlarmService;
import org.sentilo.web.catalog.service.ApplicationService;
import org.sentilo.web.catalog.service.ComponentService;
import org.sentilo.web.catalog.service.PermissionService;
import org.sentilo.web.catalog.service.ProviderService;
import org.sentilo.web.catalog.service.SensorService;
import org.sentilo.web.catalog.utils.CatalogUtils;
import org.sentilo.web.catalog.validator.ApiValidationResults;
import org.sentilo.web.catalog.validator.ApiValidator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataAccessException;
import org.springframework.stereotype.Controller;
import org.springframework.util.Assert;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;


/**
 * El acceso a los metodos de esta API esta securizado mediante user/password. Solo la plataforma podra invocar a esta API. 
 */

@Controller
@RequestMapping("/api")
public class ApiController {
	private final Logger logger = LoggerFactory.getLogger(ApiController.class);
	
	@Autowired
	private PermissionService permissionService;

	@Autowired
	private ApplicationService applicationService;

	@Autowired
	private ProviderService providerService;
	
	@Autowired
	private SensorService sensorService;
	
	@Autowired
	private AlarmService alarmService;
	
	@Autowired
	private ComponentService componentService;
	
	@Autowired
	private ApiValidator validator;

	@RequestMapping(value = "/permissions", method = RequestMethod.GET)
	public @ResponseBody Permissions getPermissions() {		
		return permissionService.retrievePermissions();
	}

	@RequestMapping(value = "/credentials", method = RequestMethod.GET)
	public @ResponseBody CredentialsDTO getAuthorizations() {
		logger.debug("Catalog API: get credentials");
		CredentialsDTO credentials = new CredentialsDTO();
		credentials.addAllApplications(applicationService.findAll());
		credentials.addAllProviders(providerService.findAll());
		logger.debug("Catalog API: returning {} credentials",credentials.getCredentials().size());
		return credentials;
	}

	@RequestMapping(value = "/alerts", method = RequestMethod.GET)
	public @ResponseBody AlertsOwnersResponseMessage getAlarms() {
		logger.debug("Catalog API: get alerts owners");
		List<Alarm> alarms = alarmService.findAll();
		AlertsOwnersResponseMessage alertsOwners = new AlertsOwnersResponseMessage();
		for(Alarm alarm : alarms){
			if(alarm.getType().equals(Type.EXTERNAL)){
				alertsOwners.addAlert(alarm.getId(), alarm.getClientApplication());
			}else{
				alertsOwners.addAlert(alarm.getId(), alarm.getProviderId());
			}
		}
		logger.debug("Catalog API: returning {} alerts",alertsOwners.getAlerts().size());
		return alertsOwners;				
	}
	
	
	@RequestMapping(value = "/provider/{providerId}", method = RequestMethod.POST)
	public @ResponseBody CatalogResponseMessage registerSensors(@RequestBody CatalogInputMessage message, @PathVariable String providerId) {		
		logger.debug("Catalog API: register sensors");
		Assert.notNull(message.getSensors());
		try{
			ApiConverterContext context = new ApiConverterContext(message, sensorService, componentService, providerId);
			
			// El registro de los sensores debe llevar a cabo dos cosas:
			// 1. Alta de los componentes a los cuales estan asociados los sensores, en caso de que no existan
			// 2. Alta de los sensores asociandolos a los componentes pertinentes.
			List<Component> components = ApiConverter.buildComponentsFromCatalogComponents(context);
			List<Sensor> sensors = ApiConverter.buildSensorsFromCatalogSensors(context);	
						
			ApiValidationResults validationResults = validator.validateSensorsAndComponents(sensors, components, false);
			
			if(validationResults.hasErrors()){
				logger.debug("Catalog API: sensors not registered. Found validation errors");
				return new CatalogResponseMessage(CatalogResponseMessage.BAD_REQUEST, validationResults.toString());
			}else{				
				componentService.insertAll(components);
				sensorService.insertAll(sensors);
				logger.debug("Catalog API: inserted {} components and {} sensors", components.size(), sensors.size());
			}								
		}catch(DataAccessException dae){
			logger.error("Error inserting data into database. {}", dae);			
			return new CatalogResponseMessage(dae.getLocalizedMessage());			
		}catch(Exception e){
			logger.error("Unexpected error inserting data into system. {}", e);			
			return new CatalogResponseMessage(e.getLocalizedMessage());			
		}
		return new CatalogResponseMessage();
	}
	
	
	@RequestMapping(value = "/provider/{providerId}", method = RequestMethod.PUT)
	public @ResponseBody CatalogResponseMessage updateComponentOrSensors(@RequestBody CatalogInputMessage message, @PathVariable String providerId) {				
		logger.debug("Catalog API: update sensors or components");
		try{
			ApiConverterContext context = new ApiConverterContext(message, sensorService, componentService, providerId, true);
			//1. Construimos la lista de componentes o sensores a actualizar.			
			List<Component> components = ApiConverter.buildComponentsFromCatalogComponents(context);
			List<Sensor> sensors = ApiConverter.buildSensorsFromCatalogSensors(context);
			
			//2. Validamos que los datos a persistir son correctos.
			ApiValidationResults validationResults = validator.validateSensorsAndComponents(sensors, components, true);
			
			//3. En caso de errores de validacion, rechazamos los cambios e informamos del error. En caso contrario actualizamos. 
			if(validationResults.hasErrors()){
				logger.debug("Catalog API: resources not updated. Found validation errors");
				return new CatalogResponseMessage(CatalogResponseMessage.BAD_REQUEST, validationResults.toString());
			}else{
				componentService.updateAll(components);
				sensorService.updateAll(sensors);
				logger.debug("Catalog API: updated {} components and {} sensors", components.size(), sensors.size());
			}	
										
		}catch(DataAccessException dae){
			logger.error("Error updating data into database. {}", dae);			
			return new CatalogResponseMessage(dae.getLocalizedMessage());			
		}catch(Exception e){
			logger.error("Unexpected error updating data into system. {}", e);			
			return new CatalogResponseMessage(e.getLocalizedMessage());			
		}
		return new CatalogResponseMessage();
	}
	
	@RequestMapping(value = "/authorized/provider/{entityId}", method = RequestMethod.GET)
	public @ResponseBody CatalogResponseMessage getAuthorizedProviders(@PathVariable String entityId, @RequestParam(required=false) String type) {				
		logger.debug("Catalog API: getting authorized sensors and providers for entity {} ", entityId);
		List<AuthorizedProvider> authorizedProviders = null;
		try{			
			authorizedProviders = permissionService.getAuthorizedProviders(entityId, type);
		}catch(DataAccessException dae){
			logger.error("Error searching authorized providers. {}", dae);			
			return new CatalogResponseMessage(dae.getLocalizedMessage());			
		}
		logger.debug("Catalog API: returning {}  authorized providers ", authorizedProviders.size());
		return new CatalogResponseMessage(authorizedProviders);
	}
	
	@RequestMapping(value = "/delete/provider/{providerId}", method = RequestMethod.PUT)
	public @ResponseBody CatalogResponseMessage deleteProviderChilds(@RequestBody(required=false) CatalogDeleteInputMessage message, @PathVariable String providerId) {				
		logger.debug("Catalog API: deleting {} resources ", providerId);
		try{
			if(message== null || (CatalogUtils.arrayIsEmpty(message.getSensorsIds()) && CatalogUtils.arrayIsEmpty(message.getComponentsIds()))){							
				providerService.deleteChilds(new Provider(providerId));
				logger.debug("Catalog API: deleted all resources");
			}else if(!CatalogUtils.arrayIsEmpty(message.getSensorsIds())){				
				sensorService.deleteSensors(message.getSensorsIds());
				logger.debug("Catalog API: deleted {} sensors", message.getSensorsIds().length);
			}else if(!CatalogUtils.arrayIsEmpty(message.getComponentsIds())){
				componentService.deleteComponents(message.getComponentsIds());
				logger.debug("Catalog API: deleted {} components", message.getComponentsIds().length);
			}
		}catch(Exception e){
			logger.error("Error deleting childs from provider {} . {}", providerId ,e);			
			return new CatalogResponseMessage(e.getLocalizedMessage());
		}
		return new CatalogResponseMessage();
	}
	
	
}
