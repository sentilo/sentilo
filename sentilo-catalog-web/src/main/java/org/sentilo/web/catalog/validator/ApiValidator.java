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
package org.sentilo.web.catalog.validator;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.sentilo.web.catalog.domain.Component;
import org.sentilo.web.catalog.domain.ComponentType;
import org.sentilo.web.catalog.domain.Sensor;
import org.sentilo.web.catalog.domain.SensorType;
import org.sentilo.web.catalog.exception.DuplicateKeyException;
import org.sentilo.web.catalog.exception.builder.CompoundDuplicateKeyExceptionBuilder;
import org.sentilo.web.catalog.service.ComponentTypesService;
import org.sentilo.web.catalog.service.SensorService;
import org.sentilo.web.catalog.service.SensorTypesService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.CollectionUtils;
import org.springframework.validation.BindingResult;
import org.springframework.validation.DataBinder;
import org.springframework.validation.Validator;

@org.springframework.stereotype.Component
public class ApiValidator {
	
	@Autowired
	private SensorTypesService sensorTypesService;
	
	@Autowired
	private ComponentTypesService componentTypesService;
	
	@Autowired
	private SensorService sensorService;
	
	@Autowired
    private Validator validator;
	
	public ApiValidationResults validateSensorsAndComponents(List<Sensor> sensors, List<Component> components, boolean isUpdateAction){				
		ApiValidationResults results = new ApiValidationResults();
		List<SensorType> sensorTypes = sensorTypesService.findAll();
		List<ComponentType> componentTypes = componentTypesService.findAll();		
		
		if(!CollectionUtils.isEmpty(sensors)){
			for(Sensor sensor: sensors){
				validate(results, sensor, sensor.getSensorId(), "Sensor");								
				validateSensorTypes(results, sensor, sensorTypes);
			}
			
			if(!isUpdateAction && !results.hasErrors()){
				validateSensorKeys(results, sensors);				
			}
		}
		
		if(!CollectionUtils.isEmpty(components)){
			for(Component component: components){
				validate(results, component, component.getName(), "Component");
				validateComponentTypes(results, component, componentTypes);
			}
		}
				
		return results;
	}
	
	private void validateSensorKeys(ApiValidationResults results, List<Sensor> sensors) {
		SensorEntityKeyValidatorImpl keyValidator = 
			new SensorEntityKeyValidatorImpl(sensorService, new CompoundDuplicateKeyExceptionBuilder("error.sensor.duplicate.key"));
		
		// Constraints to validate:
		// 1. all sensor names are different
		// 2. Not exists a sensor with the same in the catalog
		validateUnicitySensorKeys(results, sensors);
		
		if(!results.hasErrors()){
			for(Sensor sensor: sensors){								
				validateSensorKey(results, keyValidator,sensor);				
			}
		}						
	}
	
	private void validateUnicitySensorKeys(ApiValidationResults results, List<Sensor> sensors){
		Map<String, List<Sensor>> sensorsByName = new HashMap<String, List<Sensor>>();
		// Group by name ... 
		for (Sensor sensor : sensors) {
		   String key = sensor.getSensorId();
		   if (sensorsByName.get(key) == null) {
			   sensorsByName.put(key, new ArrayList<Sensor>());
		   }
		   sensorsByName.get(key).add(sensor);
		}
		
		// ... and count keys elements
		Iterator<String> it = sensorsByName.keySet().iterator();
		while(it.hasNext()){
			String sensorName = it.next();			
			if(sensorsByName.get(sensorName).size()>1){
				String errorMessage = String.format("There are more than one sensors with de id %s .", sensorName);
				results.addErrorMessage(errorMessage);
			}
		}
	}

	private void validateSensorKey(ApiValidationResults results, SensorEntityKeyValidatorImpl keyValidator ,Sensor sensor) {
		try{
			keyValidator.checkIntegrityKey(sensor.getId());
		}catch(DuplicateKeyException dke){
			String errorMessage = String.format("Sensor %s : Sensor with the same id already exists.", sensor.getSensorId());
			results.addErrorMessage(errorMessage);
		}		
	}

	private void validateSensorTypes(ApiValidationResults results, Sensor sensor, List<SensorType> sensorTypes) {
		if(!sensorTypes.contains(new SensorType(sensor.getType()))){
			String errorMessage = String.format("Sensor %s : an invalid value was specified for type.", sensor.getSensorId());				
			results.addErrorMessage(errorMessage);
		}
	}
	
	private void validateComponentTypes(ApiValidationResults results, Component component, List<ComponentType> componentTypes) {
		if(!componentTypes.contains(new ComponentType(component.getComponentType()))){
			String errorMessage = String.format("Component %s : an invalid value was specified for componentType.", component.getName());				
			results.addErrorMessage(errorMessage);
		}
	}

	private void validate(ApiValidationResults results, Object obj, String objId, String objName){
		DataBinder binder = new DataBinder(obj);
		binder.setValidator(validator);
		binder.validate();
		
		BindingResult result = binder.getBindingResult();
		
		if(result.hasErrors()){
			String errorMessage = String.format("%s %s has required fields not filled in.",objName,objId);				
			results.addErrorMessage(errorMessage);				
		}
	}

	public void setSensorTypesService(SensorTypesService sensorTypesService) {
		this.sensorTypesService = sensorTypesService;
	}

	public void setComponentTypesService(ComponentTypesService componentTypesService) {
		this.componentTypesService = componentTypesService;
	}

	public void setSensorService(SensorService sensorService) {
		this.sensorService = sensorService;
	}

	public void setValidator(Validator validator) {
		this.validator = validator;
	}
}
