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
package org.sentilo.web.catalog.test.validator;

import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.sentilo.web.catalog.domain.Sensor;
import org.sentilo.web.catalog.validator.ApiValidationResults;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.validation.BindingResult;
import org.springframework.validation.DataBinder;
import org.springframework.validation.Validator;


@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations="classpath:spring/test-validation-context.xml")
public class ValidateApiTest {
	
	@Autowired
	private Validator validator;
	
	@Test
	public void validateNokSensors(){
		List<Sensor> sensors = getSensorsWithErrors();
		ApiValidationResults result = validateSensorsAndComponents(sensors);
		assertTrue(result.hasErrors());
		System.out.println(result.toString());
	}
	
	@Test
	public void validateOkSensors(){
		List<Sensor> sensors = getSensors();
		ApiValidationResults result = validateSensorsAndComponents(sensors);
		assertTrue(!result.hasErrors());
	}
	
	private ApiValidationResults validateSensorsAndComponents(List<Sensor> sensors){				
		ApiValidationResults results = new ApiValidationResults();
		
		for(Sensor sensor: sensors){
			validate(results, sensor, sensor.getSensorId(), "Sensor");
		}
							
		return results;
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
	
	private List<Sensor> getSensorsWithErrors(){
		List<Sensor> sensors = new ArrayList<Sensor>();
		
		String providerId= "provider1";
		
		String componentId = "provider1"+"."+"component1";
		Sensor sensor = new Sensor(providerId,componentId, "sensor1");
		sensor.setDescription("Desc dels ensor1");		
		sensor.setUnit("C");
		sensor.setDataType(Sensor.DataType.BOOLEAN);
		sensor.setCreatedAt(new Date());
		sensors.add(sensor);
		
		Sensor sensor2 = new Sensor(providerId,componentId, "sensor2");
		sensor2.setDescription("Desc del sensor2");
		sensor2.setType("temperature");		
		sensor2.setDataType(Sensor.DataType.BOOLEAN);
		sensor2.setCreatedAt(new Date());
		sensors.add(sensor2);
				
		return sensors;
	}
	
	private List<Sensor> getSensors(){
		List<Sensor> sensors = new ArrayList<Sensor>();
		
		String providerId= "provider1";
		
		String componentId = "provider1"+"."+"component1";
		Sensor sensor = new Sensor(providerId,componentId, "sensor1");
		sensor.setDescription("Desc dels ensor1");		
		sensor.setUnit("C");
		sensor.setType("temperature");
		sensor.setDataType(Sensor.DataType.BOOLEAN);
		sensor.setCreatedAt(new Date());
		sensors.add(sensor);				
				
		return sensors;
	}
}
