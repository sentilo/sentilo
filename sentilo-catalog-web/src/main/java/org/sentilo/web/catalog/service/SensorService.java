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
package org.sentilo.web.catalog.service;

import java.util.Collection;
import java.util.List;

import org.sentilo.common.domain.OrderMessage;
import org.sentilo.common.domain.QueryFilterParams;
import org.sentilo.platform.client.core.domain.AlarmMessage;
import org.sentilo.platform.client.core.domain.Observation;
import org.sentilo.web.catalog.domain.Sensor;

public interface SensorService extends CrudService<Sensor> {

  List<Observation> getLastObservations(Sensor sensor);

  List<AlarmMessage> getLastAlarmsMessages(Sensor sensor);

  List<OrderMessage> getLastOrderMessages(Sensor sensor);

  Observation getLastObservation(Sensor sensor);

  Observation getLastObservation(Sensor sensor, QueryFilterParams params);

  void updateMulti(Collection<String> sensorIds, String param, Object value);

  void deleteSensors(String[] sensorsIds);

  void deleteSensorsFromComponents(List<String> componentsIds);

  Sensor findByName(String providerId, String sensorId);

  void changeAccessType(String[] sensorsIds, boolean isPublicAccess);
}
