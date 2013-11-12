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
package org.sentilo.web.catalog.utils;

import org.sentilo.web.catalog.domain.Permission;

public final class Constants {

	public static final String MODEL_ALARMS = "alarms";
	public static final String MODEL_ALARM = "alarm";
	public static final String MODEL_ALARM_NOTIFICATION_TYPES = "alarmNotificationTypes";
	public static final String MODEL_ALARM_TRIGGERS = "alarmTriggers";
	public static final String MODEL_ALARM_TYPES = "alarmTypes";
	public static final String MODEL_APPLICATION = "application";
	public static final String MODEL_APPLICATIONS = "applications";
	public static final String MODEL_APPLICATION_PROVIDER = "applicationProvider";
	public static final String MODEL_COMPONENT = "component";
	public static final String MODEL_COMPONENTS = "components";
	public static final String MODEL_COMPONENT_ICON = "componentIcon";
	public static final String MODEL_COMPONENT_TYPES = "componentTypes";
	public static final String MODEL_COMPONENT_COMPONENTS = "componentComponents";
	public static final String MODEL_COMPONENT_SENSORS = "componentSensors";
	public static final String MODEL_MESSAGE_KEY = "message";
	public static final String MODEL_CONFIRMATION_MESSAGE_KEY = "confirmationMessage";
	public static final String MODEL_ERROR_MESSAGE = "errorMessage";
	public static final String MODEL_OPENED_TAB = "openedTab";
	public static final String MODEL_PERMISSIONS = "permissions";
	public static final String MODEL_PERMISSION_TYPES = "permissionTypes";
	public static final String MODEL_PROVIDER = "provider";
	public static final String MODEL_PROVIDERS = "providers";
	public static final String MODEL_PROVIDER_ID = "providerId";
	public static final String MODEL_PROVIDER_TYPES = "providerTypes";
	public static final String MODEL_SENSORS = "sensors";
	public static final String MODEL_SENSOR = "sensor";
	public static final String MODEL_SENSOR_DATA_TYPES = "sensorDataTypes";
	public static final String MODEL_SENSOR_LAST_OBSERVATION = "sensorLastObservation";
	public static final String MODEL_SENSOR_TYPES = "sensorTypes";
	public static final String MODEL_SENSOR_TYPE = "sensorType";
	public static final String MODEL_COMPONENT_TYPE = "componentType";
	public static final String MODEL_MODE = "mode";

	public static final String MODEL_USERS = "users";
	public static final String MODEL_USER = "user";

	public static final String MODEL_DATE_UPDATED = "dateUpdated";

	public static final String VIEW_NEW_COMPONENT = "component/component_new";
	public static final String VIEW_COMPONENT_DETAIL = "component/component_detail";
	public static final String VIEW_COMPONENT_LIST = "component/component_list";
	public static final String VIEW_ADD_COMPONENTS_TO_COMPONENT = "component/component_add_components";
	public static final String VIEW_ADD_SENSORS_TO_COMPONENT = "component/component_add_sensors";

	public static final String VIEW_PUBLIC_COMPONENT_MAP = "component/public/component_map";
	public static final String VIEW_PUBLIC_COMPONENT_DETAIL = "component/public/component_detail";
	public static final String VIEW_PUBLIC_COMPONENT_LIST = "component/public/component_list";

	public static final String VIEW_USER_LIST = "user/user_list";
	public static final String VIEW_NEW_USER = "user/user_new";
	public static final String VIEW_USER_DETAIL = "user/user_detail";

	public static final String VIEW_LOGIN = "login";
	public static final String VIEW_LOGIN_DENIED = "login_denied";
	public static final String VIEW_SUCCESS = "success";
	public static final String VIEW_HOME = "home";
	public static final String VIEW_APPLICATION_LIST = "application/application_list";
	public static final String VIEW_NEW_APPLICATION = "application/application_new";
	public static final String VIEW_APPLICATION_DETAIL = "application/application_detail";
	public static final String VIEW_ADD_APPLICATION_PERMISSIONS = "application/application_permissions_add";
	public static final String VIEW_PROVIDER_LIST = "provider/provider_list";
	public static final String VIEW_NEW_PROVIDER = "provider/provider_new";
	public static final String VIEW_PROVIDER_DETAIL = "provider/provider_detail";
	public static final String VIEW_SENSOR_LIST = "sensor/sensor_list";
	public static final String VIEW_NEW_SENSOR = "sensor/sensor_new";
	public static final String VIEW_SENSOR_DETAIL = "sensor/sensor_detail";
	public static final String VIEW_ALARM_LIST = "alarm/alarm_list";
	public static final String VIEW_NEW_ALARM = "alarm/alarm_new";
	public static final String VIEW_ALARM_DETAIL = "alarm/alarm_detail";

	public static final String VIEW_SENSOR_TYPE_LIST = "sensorType/sensor_type_list";
	public static final String VIEW_NEW_SENSOR_TYPE = "sensorType/sensor_type_new";
	public static final String VIEW_SENSOR_TYPE_DETAIL = "sensorType/sensor_type_detail";

	public static final String VIEW_COMPONENT_TYPE_LIST = "componentType/component_type_list";
	public static final String VIEW_NEW_COMPONENT_TYPE = "componentType/component_type_new";
	public static final String VIEW_COMPONENT_TYPE_DETAIL = "componentType/component_type_detail";

	public static final String VIEW_STATS = "stats/stats";

	public static final String MODE_EDIT = "edit";
	public static final String MODE_CREATE = "create";
	public static final Object MODE_DETAIL = "detail";
	public static final Object MODE_DATA = "data";

	public static final String MODEL_ACTIVE_MENU = "activeMenu";

	public static final String VALIDATION_ENTITY_NAME_REGEXP = "[0-9a-zA-Z-_]+";

	public static final String DATE_FORMAT = "dd/MM/yyyy";
	public static final String DATETIME_FORMAT = "dd/MM/yyyy hh:mm";	

	public static final int STATIC = 0;
	public static final int MOBILE = 1;

	public static final int TAB_1 = 1;
	public static final int TAB_2 = 2;
	public static final int TAB_3 = 3;

	public static final String ORIGIN_SENSOR = "sensor";
	public static final String ORIGIN_PROVIDER = "provider";

	public static final String MENU_PROVIDER = "/provider";
	public static final String MENU_SENSOR = "/sensor";
	public static final String MENU_SENSOR_TYPE = "/sensorType";
	public static final String MENU_COMPONENT_TYPE = "/componentType";
	public static final String MENU_USER = "/user";
	public static final String MENU_STATS = "/stats";
	public static final String MENU_APPLICATION = "/application";
	public static final String MENU_COMPONENT = "/component";
	public static final String MENU_ALARM = "/alarm";
	public static final String MENU_COMPONENT_MAP = "/componentMap";

	public static final Permission.Type CATALOG_PERMISSION_TYPE = Permission.Type.WRITE;
	
	public static final String DEFAULT_COMPONENT_TYPE = "generic";
	
	private Constants(){
		//this prevents even the native class from calling this ctor as well :
	    throw new AssertionError();
	}
}
