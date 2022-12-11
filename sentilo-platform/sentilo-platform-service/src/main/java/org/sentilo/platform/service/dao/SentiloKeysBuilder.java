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
package org.sentilo.platform.service.dao;

import org.sentilo.common.utils.SentiloConstants;

public class SentiloKeysBuilder {

  private static final String PROVIDER = "provider";
  private static final String SENSOR = "sensor";
  private static final String ALERT = "alert";
  private static final String PID = "pid";
  private static final String SID = "sid";
  private static final String AID = "aid";
  private static final String SDID = "sdid";
  private static final String AMID = "amid";
  private static final String SOID = "soid";
  private static final String OBSERVATIONS = "observations";
  private static final String ALARMS = "alarms";
  private static final String ORDERS = "orders";
  private static final String SENSORS = "sensors";
  private static final String SUBS = "subs";
  private static final String DELIMITER = SentiloConstants.REDIS_KEY_TOKEN;

  public SentiloKeysBuilder() {
    super();
  }

  public String getSensorObservationsKey(final Long sid) {
    return getSensorObservationsKey(sid.toString());
  }

  public String getSensorObservationsKey(final String sid) {
    return SID + DELIMITER + sid + DELIMITER + OBSERVATIONS;
  }

  public String getObservationKey(final Long sdid) {
    return getObservationKey(sdid.toString());
  }

  public String getObservationKey(final String sdid) {
    return SDID + DELIMITER + sdid;
  }

  public String getAlertAlarmsKey(final Long aid) {
    return AID + DELIMITER + aid + DELIMITER + ALARMS;
  }

  public String getAlarmKey(final Long amid) {
    return AMID + DELIMITER + amid;
  }

  public String getSensorOrdersKey(final Long sid) {
    return SID + DELIMITER + sid + DELIMITER + ORDERS;
  }

  public String getOrderKey(final Long soid) {
    return SOID + DELIMITER + soid;
  }

  public String getProviderKey(final Long pid) {
    return PID + DELIMITER + pid;
  }

  public String getReverseProviderKey(final String providerId) {
    return PROVIDER + DELIMITER + providerId + DELIMITER + PID;
  }

  public String getSensorKey(final Long sid) {
    return SID + DELIMITER + sid;
  }

  public String getProviderSensorsKey(final Long pid) {
    return PID + DELIMITER + pid + DELIMITER + SENSORS;
  }

  public String getReverseSensorKey(final String providerId, final String sensorId) {
    return SENSOR + DELIMITER + providerId + DELIMITER + sensorId + DELIMITER + SID;
  }

  public String getAlertKey(final Long aid) {
    return AID + DELIMITER + aid;
  }

  public String getReverseAlertKey(final String alertId) {
    return ALERT + DELIMITER + alertId + DELIMITER + AID;
  }

  public String getSubscriptionKey(final String entityId) {
    return SUBS + DELIMITER + entityId;
  }

}
