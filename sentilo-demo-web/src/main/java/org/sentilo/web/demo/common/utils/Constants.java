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
package org.sentilo.web.demo.common.utils;

public final class Constants {
	
	public static final String TOKEN_AUTH = "8495b3140f4074baefaa3c2da4defe33331a5c7a0ad19b0eb34efab27112a24a";
	public static final String DEFAULT_PROVIDER_ID="app_demo_provider";
	public static final String DEFAULT_COMPONENT_ID="comp_demo";
	public static final String DEFAULT_SENSOR_ID= "appdemo_sensor_test";
	public static final String ENDPOINT_ALARM = "/alarm";
	public static final String ENDPOINT_SENSOR = "/observations";
	public static final String ENDPOINT_ORDER = "/order";
	
	public static final String SENSOR_DESCRIPTION = "Sensor de prova";
	public static final String SENSOR_UNIT = "m";
	
	public static final String SENSOR_NUM_OF_ITERATIONS = "1";
	public static final String SENSOR_FREQ = "5000";	
	public static final String SENSOR_VALUE = "prefix";
	
	public static final int STATIC = 0;
	public static final int MOBILE = 1;
	
	public static final String EXT_SENSOR_NUM_REC="1";
	
	public static final String ALARM_ID = "11";
	public static final String ALARM_MESSAGE = "Alarma de prova";
	
	public static final String ORDER_MESSAGE= "Stop";
	
	public static final String WEBSOCKET_TYPE = "type";
	public static final String WEBSOCKET_SENS_ID = "id";
	public static final String WEBSOCKET_PROV_ID = "providerId";
	public static final String WEBSOCKET_ALARM_ID = "alarmId";
	public static final String WEBSOCKET_TYPE_SENSOR = "sensor";
	public static final String WEBSOCKET_TYPE_ALARM = "alarm";
	public static final String WEBSOCKET_TYPE_ORDER = "order";
	
	public static final String MODEL_OBSERVATION = "observation";
	public static final String MODEL_SENSOR = "virtualSensor";
	public static final String MODEL_SENSOR_DATA_TYPES = "sensorDataTypes";
	public static final String MODEL_SENSOR_TYPES = "sensorTypes";
	public static final String MODEL_COMPONENT_TYPES = "componentTypes";
	public static final String MODEL_EXTERNAL_SENSOR = "externalSensor";
	public static final String MODEL_ALARM = "alarm";
	public static final String MODEL_ORDER = "order";
	public static final String MODEL_MSG_SUCCESS = "success";
	public static final String MODEL_MSG_ERROR = "error";
	public static final String MODEL_WS_ADDRESS = "wsAddress";
	public static final String MODEL_URL = "urlws";
	
	public static final String MODEL_CODE_ALARM_PUB ="alarm.publish.success";
	public static final String MODEL_CODE_ORDER_PUB ="order.publish.success";
	
	public static final String VIEW_HOME = "home";
	public static final String VIEW_VS_CREATE = "virtualSensor/vs_create";
	public static final String VIEW_VS_SEND_DATA = "virtualSensor/vs_send_data";
	public static final String VIEW_RD_SELECT_SENSOR = "recoverData/rd_select_sensor";
	public static final String VIEW_RD_LAST_DATA = "recoverData/rd_last_data";
	public static final String VIEW_RD_SUBSCRIBE_DATA = "recoverData/rd_subscribe_data";
	
	public static final String VIEW_AL_CREATE = "alarms/al_create";
	public static final String VIEW_AL_SELECT_SUBSCRIPTION = "alarms/al_select_subscription";
	public static final String VIEW_AL_SUBSCRIBE = "alarms/al_subscribe";

	public static final String VIEW_OR_CREATE = "orders/or_create";
	public static final String VIEW_OR_SELECT_SUBSCRIPTION = "orders/or_select_subscription";
	public static final String VIEW_OR_SUBSCRIBE = "orders/or_subscribe";
	
	public static final String RETURN_OK = "OK";
	
	public static final String EVENT_DATA ="eventData";
	public static final String EVENT_ALARM ="eventAlarm";
	public static final String EVENT_ORDER ="eventOrder";
	
	public static final String VALIDATION_ENTITY_NAME_REGEXP = "[0-9a-zA-Z-_]+";
	
	private Constants(){
		//this prevents even the native class from calling this ctor as well :
	    throw new AssertionError();
	}

}
