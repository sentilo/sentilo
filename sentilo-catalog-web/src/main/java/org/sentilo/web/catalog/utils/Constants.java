/*
 * Sentilo
 *  
 * Original version 1.4 Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de Barcelona.
 * Modified by Opentrends adding support for multitenant deployments and SaaS. Modifications on version 1.5 Copyright (C) 2015 Opentrends Solucions i Sistemes, S.L.
 * 
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
import org.sentilo.web.catalog.domain.TenantPermission;

public abstract class Constants {

  public static final String MODEL_ALERTS = "alerts";
  public static final String MODEL_ALERT = "alert";
  public static final String MODEL_ALERT_NOTIFICATION_TYPES = "alertNotificationTypes";
  public static final String MODEL_ALERT_TRIGGERS = "alertTriggers";
  public static final String MODEL_ALERT_TYPES = "alertTypes";
  public static final String MODEL_APPLICATION = "application";
  public static final String MODEL_APPLICATIONS = "applications";
  public static final String MODEL_APPLICATION_PROVIDER = "applicationProvider";
  public static final String MODEL_COMPONENT = "component";
  public static final String MODEL_COMPONENTS = "components";
  public static final String MODEL_COMPONENT_ICON = "componentIcon";
  public static final String MODEL_COMPONENT_TYPES = "componentTypes";
  public static final String MODEL_COMPONENT_TYPES_EDIT = "componentTypesEdit";
  public static final String MODEL_COMPONENT_TYPE_ICONS = "componentTypeIcons";
  public static final String MODEL_COMPONENT_COMPONENTS = "componentComponents";
  public static final String MODEL_COMPONENT_SENSORS = "componentSensors";
  public static final String MODEL_MESSAGE_KEY = "message";
  public static final String MODEL_CONFIRMATION_MESSAGE_KEY = "confirmationMessage";
  public static final String MODEL_CONFIRMATION_MESSAGE_ARGS_KEY = "confirmationMessageArgs";
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
  public static final String MODEL_SENSOR_STATES = "sensorStates";
  public static final String MODEL_SENSOR_SUBSTATES = "sensorSubstates";
  public static final String MODEL_COMPONENT_TYPE = "componentType";
  public static final String MODEL_MODE = "mode";
  public static final String MODEL_USERS = "users";
  public static final String MODEL_USER = "user";
  public static final String MODEL_ENERGY_TYPES = "energyTypes";
  public static final String MODEL_CONNECTIVITY_TYPES = "connectivityTypes";
  public static final String MODEL_MAP_TYPE = "mapType";
  public static final String MODEL_ALERT_RULE = "alertRule";
  public static final String MODEL_ALERT_RULE_TRIGGERS = "alertRuleTriggers";
  public static final String MODEL_ALERT_RULE_CONFIRMED_SENSORS = "alertRuleConfirmedSensors";
  public static final String MODEL_TENANT = "tenant";
  public static final String MODEL_TENANTS = "tenants";
  public static final String MODEL_TENANT_ID = "tenantId";
  public static final String MODEL_TENANT_PERMISSION = "tenantPermission";
  public static final String MODEL_TENANT_PERMISSIONS = "permissions";
  public static final String MODEL_TENANT_CUSTOM_PARAMS = "tenantCustomParams";
  public static final String MODEL_DOCUMENT_FILE = "documentFile";
  public static final String MODEL_ENTITY_ID = "entityId";
  public static final String MODEL_ENTITY = "entity";

  public static final String MODEL_DATE_UPDATED = "dateUpdated";

  public static final String VIEW_ERROR_ACCESS_NOT_ALLOWED = "accessNotAllowed";

  public static final String VIEW_NEW_COMPONENT = "component/component_new";
  public static final String VIEW_COMPONENT_DETAIL = "component/component_detail";
  public static final String VIEW_COMPONENT_LIST = "component/component_list";
  public static final String VIEW_ADD_COMPONENTS_TO_COMPONENT = "component/component_add_components";

  public static final String VIEW_PUBLIC_ROUTE_MAP = "component/public/route_map";
  public static final String VIEW_PUBLIC_TRAFFIC_MAP = "component/public/traffic_map";

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
  public static final String VIEW_ALERT_LIST = "alert/alert_list";
  public static final String VIEW_NEW_ALERT = "alert/alert_new";
  public static final String VIEW_ALERT_DETAIL = "alert/alert_detail";
  public static final String VIEW_TENANT_LIST = "tenant/tenant_list";
  public static final String VIEW_NEW_TENANT = "tenant/tenant_new";
  public static final String VIEW_TENANT_DETAIL = "tenant/tenant_detail";
  public static final String VIEW_ADD_TENANT_PERMISSIONS = "tenant/tenant_permissions_add";
  public static final String VIEW_EDIT_TENANT_PERMISSIONS = "tenant/tenant_permissions_edit";

  public static final String VIEW_SENSOR_TYPE_LIST = "sensorType/sensor_type_list";
  public static final String VIEW_NEW_SENSOR_TYPE = "sensorType/sensor_type_new";
  public static final String VIEW_SENSOR_TYPE_DETAIL = "sensorType/sensor_type_detail";

  public static final String VIEW_COMPONENT_TYPE_LIST = "componentType/component_type_list";
  public static final String VIEW_NEW_COMPONENT_TYPE = "componentType/component_type_new";
  public static final String VIEW_COMPONENT_TYPE_DETAIL = "componentType/component_type_detail";

  public static final String VIEW_ALERT_RULE_LIST = "alertRule/alertRule_list";
  public static final String VIEW_ALERT_RULE_DETAIL = "alertRule/alertRule_detail";
  public static final String VIEW_NEW_ALERT_RULE = "alertRule/alertRule_new";

  public static final String VIEW_STATS = "stats/stats";

  public static final String VIEW_PROVIDER_DOCUMENT_FILES_LIST = "provider/provider_documents_list";
  public static final String VIEW_PROVIDER_ADD_DOCUMENT_FILE = "provider/provider_documents_add";

  public static final String MODE_EDIT = "edit";
  public static final String MODE_CREATE = "create";
  public static final Object MODE_DETAIL = "detail";
  public static final Object MODE_DATA = "data";

  public static final String MODEL_ACTIVE_MENU = "activeMenu";

  public static final String VALIDATION_ENTITY_NAME_REGEXP = "[0-9a-zA-Z-_]+";
  public static final String VALIDATION_FILENAME_REGEXP = "[0-9a-zA-Z-_.]+";

  public static final String VALIDATION_SUCCESS = "SUCCESS";
  public static final String VALIDATION_FAIL = "FAIL";

  public static final String DATE_FORMAT = "dd/MM/yyyy";
  public static final String DATETIME_FORMAT = "dd/MM/yyyy HH:mm";

  public static final int STATIC = 0;
  public static final int MOBILE = 1;

  public static final int TAB_1 = 1;
  public static final int TAB_2 = 2;
  public static final int TAB_3 = 3;
  public static final int TAB_4 = 4;
  public static final int TAB_5 = 5;

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
  public static final String MENU_ALERT = "/alert";
  public static final String MENU_COMPONENT_MAP = "/componentMap";
  public static final String MENU_ALERT_RULE = "/alertRule";
  public static final String MENU_TENANT = "/tenant";

  public static final Permission.Type CATALOG_PERMISSION_TYPE = Permission.Type.ADMIN;
  public static final TenantPermission.Type CATALOG_TENANT_PERMISSION_TYPE = TenantPermission.Type.WRITE;

  public static final String DEFAULT_COMPONENT_TYPE = "generic";

  public static final String DEFAULT_KEY_TOKEN_SPLITTER = ".";
  public static final String PERMISSION_TOKEN_SPLITTER = "@";

  public static final String COMMA_TOKEN_SPLITTER = ",";

  public static final String LOCATION_TOKEN_SPLITTER = COMMA_TOKEN_SPLITTER;
  public static final String LOCATION_TOKEN_DIVIDER = " ";

  public static final String ASC = "asc";
  public static final String DESC = "desc";

  public static final String ENERGY_TYPES_KEY = "energy.types.list";
  public static final String CONNECTIVITY_TYPES_KEY = "connectivity.types.list";

  // Excel view constants
  public static final String NAME_PROP = "name";
  public static final String ID_PROP = "id";
  public static final String DESCRIPTION_PROP = "description";
  public static final String CREATED_AT_PROP = "createdAt";
  public static final String SENSOR_ID_PROP = "sensorId";
  public static final String PROVIDER_ID_PROP = "providerId";
  public static final String PUBLIC_ACCESS_PROP = "publicAccess";
  public static final String TYPE_PROP = "type";
  public static final String COMPONENT_TYPE_PROP = "componentType";
  public static final String SENSOR_TYPE_PROP = "sensorType";
  public static final String USER_NAME_PROP = "userName";
  public static final String EMAIL_PROP = "email";
  public static final String LOCATION_PROP = "location";
  public static final String TARGET_PROP = "target";
  public static final String EXPRESSION_PROP = "expression";
  public static final String CONTENT_TYPE_PROP = "contentType";
  public static final String CREATED_BY_PROP = "createdBy";

  public static final String MESSAGE_KEYS_PREFIX = "keysPrefix";
  public static final String LIST_COLUMN_NAMES = "listColumnNames";
  public static final String RESULT_LIST = "resultList";

  public static final String AUDIT_LOGGER_NAME = "AUDIT";

  public static final String SYNC_FIELD = "synchronized";

  private Constants() {
    // this prevents even the native class from calling this ctor as well :
    throw new AssertionError();
  }
}
