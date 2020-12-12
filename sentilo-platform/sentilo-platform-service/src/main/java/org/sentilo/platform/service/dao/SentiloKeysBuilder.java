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

import org.sentilo.platform.service.utils.PubSubConstants;

public class SentiloKeysBuilder {

  public SentiloKeysBuilder() {
    super();
  }

  public String getSensorObservationsKey(final Long sid) {
    return getSensorObservationsKey(sid.toString());
  }

  public String getSensorObservationsKey(final String sid) {
    return "sid:" + sid + ":observations";
  }

  public String getObservationKey(final Long sdid) {
    return getObservationKey(sdid.toString());
  }

  public String getObservationKey(final String sdid) {
    return "sdid:" + sdid;
  }

  public String getAlertAlarmsKey(final Long aid) {
    return "aid:" + aid + ":alarms";
  }

  public String getAlarmKey(final Long amid) {
    return "amid:" + amid;
  }

  public String getSensorOrdersKey(final Long sid) {
    return "sid:" + sid + ":orders";
  }

  public String getOrderKey(final Long soid) {
    return "soid:" + soid;
  }

  public String getProviderKey(final Long pid) {
    return "pid:" + pid;
  }

  public String getReverseProviderKey(final String providerId) {
    return "provider:" + providerId + ":pid";
  }

  public String getSensorKey(final Long sid) {
    return "sid:" + sid;
  }

  public String getProviderSensorsKey(final Long pid) {
    return "pid:" + pid + ":sensors";
  }

  public String getReverseSensorKey(final String providerId, final String sensorId) {
    return "sensor:" + providerId + ":" + sensorId + ":sid";
  }

  public String getAlertKey(final Long aid) {
    return "aid:" + aid;
  }

  public String getReverseAlertKey(final String alertId) {
    return "alert:" + alertId + ":aid";
  }

  public String getSubscriptionKey(final String entityId) {
    return "subs" + PubSubConstants.REDIS_KEY_TOKEN + entityId;
  }
}
