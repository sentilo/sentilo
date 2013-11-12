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
package org.sentilo.platform.client.integration;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.List;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.sentilo.common.domain.AuthorizedProvider;
import org.sentilo.common.domain.CatalogComponent;
import org.sentilo.common.domain.CatalogSensor;
import org.sentilo.platform.client.core.PlatformClientOperations;
import org.sentilo.platform.client.core.domain.CatalogDeleteInputMessage;
import org.sentilo.platform.client.core.domain.CatalogInputMessage;
import org.sentilo.platform.client.core.domain.CatalogOutputMessage;
import org.sentilo.platform.client.core.exception.PlatformClientAccessException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;


@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations="classpath:spring/sentilo-platform-client-integration.xml")
public class CatalogServiceOperationsIntegrationTest {
	static String PROVIDER_ID = "testApp_provider";
	static String APP_ID = "testApp";
	static String tokenApp ="646967a9f99ae76cfb836026d0015c4b80f8c0e1efbd3d261250156efd8fb96f";
	static String tokenProv = "563093ec5252147edc8860c2d667be5db0c010325b6953ed5b323724bcc00e05";
	
	@Autowired
	protected PlatformClientOperations platformTemplate;
	
	@Test(expected=PlatformClientAccessException.class)
	public void registerWithoutSensors() throws Exception{
		CatalogInputMessage message = new CatalogInputMessage(PROVIDER_ID);				
		platformTemplate.getCatalogOps().registerSensors(message);								
	}
	
	@Test
	public void doRegisterGetAndDelete(){
		deleteSensors();
		getSensors(0,0);
		registerSensors();
		getSensors(1,5);
		getSensors(1,2,"temperature");
		getSensors(0,0,"unknownType");
		deleteSensors();
		getSensors(0,0);
	}
	
	@Test
	public void doRegisterUpdateGetAndDelete(){
		deleteSensors();
		getSensors(0,0);
		registerSensors();
		getSensors(1,5);
		getSensors(1,2,"temperature");
		getSensors(1,3,"wind");
		getSensors(0,0,"noise");		
		updateSensors();
		getSensors(1,2,"temperature");
		getSensors(1,1,"wind");
		getSensors(1,2,"noise");						
		getSensors(0,0,"unknownType");
		validateComponentState("generic", null);
		updateComponents();
		validateComponentState("meteo", "New desc");
		deleteSensors();
		getSensors(0,0);
	}
	
	
	private void registerSensors(){
		CatalogInputMessage message = new CatalogInputMessage(PROVIDER_ID, buildSensorsToRegister());
		message.setIdentityToken(tokenProv);
		platformTemplate.getCatalogOps().registerSensors(message);					
	}
	
	private void updateSensors(){
		CatalogInputMessage message = new CatalogInputMessage(PROVIDER_ID, buildSensorsToUpdate());
		message.setIdentityToken(tokenProv);
		platformTemplate.getCatalogOps().updateComponents(message);					
	}
	
	private void updateComponents(){
		CatalogInputMessage message = new CatalogInputMessage(PROVIDER_ID);
		message.setComponents(buildComponentsToUpdate());
		message.setIdentityToken(tokenProv);
		platformTemplate.getCatalogOps().updateComponents(message);					
	}
	
	private void validateComponentState(String type, String desc){
		CatalogInputMessage message = new CatalogInputMessage();
		message.setIdentityToken(tokenApp);	
		CatalogOutputMessage outputMessage = platformTemplate.getCatalogOps().getSensors(message);
		CatalogSensor sensor = outputMessage.getProviders().get(0).getSensors().get(0);
		assertEquals("Expected component type " + type + " but found " + sensor.getComponentType(), type, sensor.getComponentType());
		if(desc == null){
			assertNull(sensor.getComponentDesc());
		}else{
			assertEquals("Expected component desc " + desc + " but found " + sensor.getComponentDesc(), desc, sensor.getComponentDesc());
		}
	}
		
	private void getSensors(int expectedProvidersSize, int expectedSensorsSize){
		getSensors(expectedProvidersSize, expectedSensorsSize, null);
	}
	
	
	private void getSensors(int expectedProvidersSize, int expectedSensorsSize, String type){
		CatalogInputMessage message = new CatalogInputMessage();
		message.setIdentityToken(tokenApp);	
		if(StringUtils.hasText(type)){
			message.setType(type);
		}
		
		CatalogOutputMessage outputMessage = platformTemplate.getCatalogOps().getSensors(message);
		assertNotNull(outputMessage);
		if(expectedSensorsSize == 0){
			assertTrue(CollectionUtils.isEmpty(outputMessage.getProviders()));
		}else{
			assertTrue("Expected "+expectedProvidersSize + " providers but found " +outputMessage.getProviders().size() ,outputMessage.getProviders().size() == expectedProvidersSize);
			int sensorsSize = 0;
			for(AuthorizedProvider provider : outputMessage.getProviders()){
				sensorsSize += provider.getSensors().size();
			}
			
			assertTrue("Expected "+expectedSensorsSize + " sensors but found " +sensorsSize ,sensorsSize == expectedSensorsSize);
		}
						
	}
		
	private void deleteSensors(){
		CatalogDeleteInputMessage message = new CatalogDeleteInputMessage(PROVIDER_ID);
		message.setIdentityToken(tokenProv);
		platformTemplate.getCatalogOps().deleteProvider(message);		
	}
	
	private List<CatalogSensor> buildSensorsToRegister(){
		List<CatalogSensor> sensors = new ArrayList<CatalogSensor>();
		CatalogSensor sensor0 = buildSensor("TEST_REC0122", "TESTAPP_COMPONENT", PROVIDER_ID, "sensor 122", "number",  "wind", "km/h");
		CatalogSensor sensor1 = buildSensor("TEST_REC0123", "TESTAPP_COMPONENT", PROVIDER_ID, "sensor 123", "number", "43.39950387509218 5.1809202294998613",  "temperature", "C");
		CatalogSensor sensor2 = buildSensor("TEST_REC0124", "TESTAPP_COMPONENT", PROVIDER_ID, "sensor 124", "number", "43.39950387509218 5.1809202294998613",  "temperature", "C");		
		CatalogSensor sensor3 = buildSensor("TEST_REC0125", "TESTAPP_COMPONENT", PROVIDER_ID, "sensor 125", "number",  "wind", "km/h");
		CatalogSensor sensor4 = buildSensor("TEST_REC0126", "TESTAPP_COMPONENT", PROVIDER_ID, "sensor 126", "number",  "wind", "km/h");
		sensors.add(sensor0);
		sensors.add(sensor1);
		sensors.add(sensor2);
		sensors.add(sensor3);
		sensors.add(sensor4);
		
		return sensors;
	}
	
	private List<CatalogSensor> buildSensorsToUpdate(){
		List<CatalogSensor> sensors = new ArrayList<CatalogSensor>();
		CatalogSensor sensor0 = buildSensor("TEST_REC0122", "TESTAPP_COMPONENT", PROVIDER_ID, null, null,  "noise", "db");				
		CatalogSensor sensor3 = buildSensor("TEST_REC0125", "TESTAPP_COMPONENT", PROVIDER_ID, null, null,  "noise", "db");
		CatalogSensor sensor4 = buildSensor("TEST_REC0126", "TESTAPP_COMPONENT", PROVIDER_ID, "desc del sensor 126", null,  null, null);
		sensors.add(sensor0);		
		sensors.add(sensor3);
		sensors.add(sensor4);
		
		return sensors;
	}
	
	private List<CatalogComponent> buildComponentsToUpdate(){
		List<CatalogComponent> components = new ArrayList<CatalogComponent>();
		CatalogComponent component = new CatalogComponent();
		component.setComponent("TESTAPP_COMPONENT");
		component.setComponentType("meteo");
		component.setComponentDesc("New desc");
		
		components.add(component);
		
							
		return components;
	}
	
	private CatalogSensor buildSensor(String sensor, String component, String provider, String description, String dataType, String type, String unit){
		return buildSensor(sensor, component, provider, description, dataType, null, type, unit);
	}
	
	private CatalogSensor buildSensor(String sensor, String component, String provider, String description, String dataType, String location, 
			String type, String unit) {		
		CatalogSensor catalogSensor = new CatalogSensor();
		catalogSensor.setSensor(sensor);
		catalogSensor.setComponent(component);
		catalogSensor.setProvider(provider);
		catalogSensor.setDescription(description);
		catalogSensor.setDataType(dataType);
		catalogSensor.setLocation(location);
		catalogSensor.setType(type);
		catalogSensor.setUnit(unit);
		
		return catalogSensor;
	}	
}

