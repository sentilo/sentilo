/*
 * Sentilo
 *
 * Original version 1.4 Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de
 * Barcelona. Modified by Opentrends adding support for multitenant deployments and SaaS.
 * Modifications on version 1.5 Copyright (C) 2015 Opentrends Solucions i Sistemes, S.L.
 *
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
package org.sentilo.web.catalog.utils;

import java.util.HashMap;
import java.util.Map;

/**
 * This class provides dictionaries to translate domain business fields into api fields when these
 * fields are not equals.
 */
public abstract class ApiTranslator {

  /** Translates Sensor domain fields to CatalogSensor fields */
  public static final Map<String, String> SENSOR_DOMAIN_FIELDS = new HashMap<String, String>();
  /** Translates Sensor domain fields to CatalogComponent fields */
  public static final Map<String, String> COMPONENT_DOMAIN_FIELDS = new HashMap<String, String>();
  /** Translates Alert domain fields to CatalogAlert fields */
  public static final Map<String, String> ALERT_DOMAIN_FIELDS = new HashMap<String, String>();

  static {
    SENSOR_DOMAIN_FIELDS.put("sensorId", "sensor");
    SENSOR_DOMAIN_FIELDS.put("providerId", "provider");
    SENSOR_DOMAIN_FIELDS.put("componentId", "component");

    COMPONENT_DOMAIN_FIELDS.put("publicAccess", "componentPublicAccess");
    COMPONENT_DOMAIN_FIELDS.put("providerId", "provider");

    ALERT_DOMAIN_FIELDS.put("sensorId", "sensor");
    ALERT_DOMAIN_FIELDS.put("componentId", "component");
  }

  private ApiTranslator() {
    // this prevents even the native class from calling this ctor as well :
    throw new AssertionError();
  }
}
