/*
 * Sentilo
 * 
 * Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de Barcelona.
 * 
 * This program is licensed and may be used, modified and redistributed under the terms of the
 * European Public License (EUPL), either version 1.1 or (at your option) any later version as soon
 * as they are approved by the European Commission.
 * 
 * Alternatively, you may redistribute and/or modify this program under the terms of the GNU Lesser
 * General Public License as published by the Free Software Foundation; either version 3 of the
 * License, or (at your option) any later version.
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied.
 * 
 * See the licenses for the specific language governing permissions, limitations and more details.
 * 
 * You should have received a copy of the EUPL1.1 and the LGPLv3 licenses along with this program;
 * if not, you may find them at:
 * 
 * https://joinup.ec.europa.eu/software/page/eupl/licence-eupl http://www.gnu.org/licenses/ and
 * https://www.gnu.org/licenses/lgpl.txt
 */
package org.sentilo.common.utils;

public final class SentiloConstants {

  public static final Integer NUM_MAXIM_ELEMENTS = 10;
  public static final Integer DEFAULT_NUM_ELEMENTS = 1;

  public static final String SLASH = "/";

  public static final String IDENTITY_KEY_HEADER = "IDENTITY_KEY";
  public static final String HMAC_HEADER = "Sentilo-Content-Hmac";
  public static final String DATE_HEADER = "Sentilo-Date";

  public static final String SENTILO_INTERNAL_TOKEN = "#@#";

  // Path tokens constants
  public static final String API_TOKEN = "api";
  public static final String PROVIDER_TOKEN = "provider";
  public static final String SENSOR_TOKEN = "sensor";
  public static final String ENTITY_TOKEN = "entity";
  public static final String PERMISSIONS_TOKEN = "permissions";
  public static final String CREDENTIALS_TOKEN = "credentials";
  public static final String AUTHORIZED_TOKEN = "authorized";
  public static final String DELETE_TOKEN = "delete";
  public static final String OWNERS_TOKEN = "owners";
  public static final String ALERT_TOKEN = "alert";
  public static final String ALARM_TOKEN = "alarm";
  public static final String DATA_TOKEN = "data";
  public static final String ORDER_TOKEN = "order";
  public static final String SUBSCRIBE_TOKEN = "subscribe";
  public static final String CATALOG_TOKEN = "catalog";
  public static final String LOCATION_TOKEN = "location";

  // Internal error messages and codes.
  public static final String INTERNAL_ERROR_MESSAGE_TEMPLATE =
      "Something went wrong. Please try again. If the problem persists, contact your support site and provide the following error code: %s";

  public static final String SENTILO_ACCESS_ERROR = "SIE00";
  public static final String CATALOG_API_ERROR = "SIE01";
  public static final String CATALOG_ALERT_API_ERROR = "SIE02";
  public static final String JSON_UNMARSHAL_ERROR = "SIE03";
  public static final String JSON_MARSHAL_ERROR = "SIE04";
  public static final String CATALOG_GENERAL_ERROR = "SIE05";
  public static final String SENTILO_UNKNOWN_ERROR = "SIE99";

  // Cross timestamp pattern
  public static final String TIMESTAMP_PATTERN = "dd/MM/yyyy'T'HH:mm:ss";

  private SentiloConstants() {
    // this prevents even the native class from calling this ctor as well :
    throw new AssertionError();
  }
}
