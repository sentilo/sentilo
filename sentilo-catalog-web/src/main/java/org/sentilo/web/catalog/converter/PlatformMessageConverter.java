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
package org.sentilo.web.catalog.converter;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.sentilo.common.domain.CatalogComponent;
import org.sentilo.common.domain.CatalogElement;
import org.sentilo.common.domain.CatalogSensor;
import org.sentilo.web.catalog.domain.Component;
import org.sentilo.web.catalog.domain.Sensor;
import org.sentilo.web.catalog.domain.Sensor.DataType;
import org.sentilo.web.catalog.service.ComponentService;
import org.sentilo.web.catalog.service.SensorService;
import org.sentilo.web.catalog.utils.CatalogUtils;
import org.sentilo.web.catalog.utils.Constants;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;


public abstract class PlatformMessageConverter {				
	
		
	public static List<CatalogSensor> convertToCatalogSensorList(List<Sensor> sensors, List<Component> components){
		List<CatalogSensor>  catalogSensors = new ArrayList<CatalogSensor>();
		
		for(Sensor sensor: sensors){
			CatalogSensor catalogSensor = convertToCatalogSensor(sensor);
			int pos = components.indexOf(new Component(sensor.getComponentId())); 
			if(pos!=-1){
				Component component = components.get(pos);
				catalogSensor.setComponent(component.getName());
				if(component.getLocation()!=null){
					catalogSensor.setLocation(CatalogUtils.locationToString(component.getLocation()));
				}
				if(StringUtils.hasText(component.getDescription())){
					catalogSensor.setComponentDesc(component.getDescription());
				}
				if(StringUtils.hasText(component.getComponentType())){
					catalogSensor.setComponentType(component.getComponentType());
				}
			}
			catalogSensors.add(catalogSensor);
		}
		
		return catalogSensors; 
	}		

	public static List<Sensor> buildSensorsFromCatalogSensors(List<CatalogSensor> catalogSensors, SensorService sensorService, String providerId, boolean isUpdateAction){
		List<Sensor> sensors = new ArrayList<Sensor>();
		
		if(!CollectionUtils.isEmpty(catalogSensors)){
			for(CatalogSensor catalogSensor: catalogSensors){
				Sensor sensor = (isUpdateAction?buildSensorToUpdate(catalogSensor, sensorService, providerId):buildNewSensor(catalogSensor, providerId));
				sensors.add(sensor);				
			}
		}
		
		return sensors;
	}
	
			
	
	public static List<Component> buildComponentsFromCatalogComponents(List<? extends CatalogElement> resources, ComponentService componentService, String providerId, boolean isUpdateAction){
		List<Component> components = new ArrayList<Component>();
		
		if(!CollectionUtils.isEmpty(resources)){
			for(CatalogElement resource: resources){
				Component component = (isUpdateAction?buildComponentToUpdate(resource, componentService, providerId):buildNewComponent(resource, providerId));
				if(component!=null && !components.contains(component)){
					components.add(component);
				}
			}
		}
		
		return components;
	}
	
	private static Sensor buildNewSensor(CatalogSensor catalogSensor, String providerId){				
		String componentId = providerId+"."+catalogSensor.getComponent();
		Sensor sensor = new Sensor(providerId,componentId, catalogSensor.getSensor());
		sensor.setDescription(catalogSensor.getDescription());
		sensor.setType(catalogSensor.getType().toLowerCase());
		sensor.setUnit(catalogSensor.getUnit());
		sensor.setDataType(parseDataTypeValue(catalogSensor.getDataType()));
		sensor.setCreatedAt(new Date());
		sensor.setUpdateAt(new Date());
		return sensor;
	}
	
	private static Sensor buildSensorToUpdate(CatalogSensor catalogSensor, SensorService sensorService, String providerId){				
		
		Sensor sensor = null;
		String component = catalogSensor.getComponent();
		String componentId = Component.buildId(providerId, component);
		
		if(StringUtils.hasText(componentId)){			
			sensor = sensorService.find(new Sensor(Sensor.buildId(componentId, catalogSensor.getSensor())));
			if(sensor != null){
				if(CatalogUtils.stringIsNotEmptyOrNull(catalogSensor.getDataType())){
					sensor.setDataType(parseDataTypeValue(catalogSensor.getDataType()));
				}
				
				if(CatalogUtils.stringIsNotEmptyOrNull(catalogSensor.getDescription())){
					sensor.setDescription(catalogSensor.getDescription());
				}
				
				if(CatalogUtils.stringIsNotEmptyOrNull(catalogSensor.getType())){
					sensor.setType(catalogSensor.getType().toLowerCase());
				}
				
				if(CatalogUtils.stringIsNotEmptyOrNull(catalogSensor.getUnit())){
					sensor.setUnit(catalogSensor.getUnit());
				}
				
				sensor.setUpdateAt(new Date());
			}			
		}
		
		
		return sensor;
	}
	
	private static CatalogSensor convertToCatalogSensor(Sensor sensor) {
		CatalogSensor catalogSensor = new CatalogSensor();
		catalogSensor.setSensor(sensor.getSensorId());
		catalogSensor.setType(sensor.getType());			
		catalogSensor.setDataType(sensor.getDataType().name());
		catalogSensor.setUnit(sensor.getUnit());	
		
		if(StringUtils.hasText(sensor.getDescription())){
			catalogSensor.setDescription(sensor.getDescription());
		}
		
		return catalogSensor;
	}
	
	private static DataType parseDataTypeValue(String dataTypeValue){
		DataType dataType = null;
		try{			
			if(StringUtils.hasText(dataTypeValue)){
				dataType = DataType.valueOf(dataTypeValue.toUpperCase());				
			}else{
				// Por defecto se fija el tipo del dato a numérico.
				dataType = DataType.NUMBER;
			}
		}catch(IllegalArgumentException e){						
		}				
		
		return dataType;
	}
	
	private static Component buildNewComponent(CatalogElement resource, String providerId){		
		
		CatalogSensor catalogSensor = (CatalogSensor) resource;
		
		// Convenciones para el registro de los componentes:
		// 1. En caso de que el nombre del componente no venga informado, por defecto se considera que el componente tiene el mismo nombre que el sensor 		
		//    De esta manera se cubre el caso de componentes con un único sensor.
		// 2. Ídem para el caso del tipo del componente: si no viene informado se fija el tipo a GENERIC
		if(!CatalogUtils.stringIsNotEmptyOrNull(catalogSensor.getComponent())){
			catalogSensor.setComponent(catalogSensor.getSensor());
		}
		
		if(!CatalogUtils.stringIsNotEmptyOrNull(catalogSensor.getComponentType())){
			catalogSensor.setComponentType(Constants.DEFAULT_COMPONENT_TYPE);
		}
		
		
				
		Component component = new Component();
		component.setProviderId(providerId);
		component.setName(catalogSensor.getComponent());
		component.setComponentType(catalogSensor.getComponentType().toLowerCase());
												
		if(StringUtils.hasText(catalogSensor.getLocation())){
			component.setLocation(CatalogUtils.convertStringLocation(catalogSensor.getLocation()));
			component.setMobile(Constants.STATIC);
		}else{
			component.setMobile(Constants.MOBILE);
		}
		
		if(CatalogUtils.stringIsNotEmptyOrNull(catalogSensor.getComponentDesc())){
			component.setDescription(catalogSensor.getComponentDesc());
		}
						
		component.setId(Component.buildId(providerId, component.getName()));
		component.setCreatedAt(new Date());
		component.setUpdateAt(new Date());
		
		return component;
	}
	
	private static Component buildComponentToUpdate(CatalogElement resource, ComponentService componentService, String providerId){
		
		CatalogComponent catalogComponent = (CatalogComponent) resource;
		
		Component component = componentService.find(new Component(Component.buildId(providerId, catalogComponent.getComponent())));
		
		if(component!=null){			
			if(CatalogUtils.stringIsNotEmptyOrNull(catalogComponent.getComponentDesc())){
				component.setDescription(catalogComponent.getComponentDesc());
			}
			
			if(CatalogUtils.stringIsNotEmptyOrNull(catalogComponent.getComponentType())){
				component.setComponentType(catalogComponent.getComponentType().toLowerCase());
			}
			
			if(CatalogUtils.stringIsNotEmptyOrNull(catalogComponent.getLocation())){
				component.setLocation(CatalogUtils.convertStringLocation(catalogComponent.getLocation()));
				component.setMobile(Constants.STATIC);
			}
			
			component.setUpdateAt(new Date());
		}
		
		return component;
	}		
		
	private PlatformMessageConverter(){
		//this prevents even the native class from calling this ctor as well :
	    throw new AssertionError();
	}
}
