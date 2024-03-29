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
package org.sentilo.platform.common.service;

import java.util.List;
import java.util.Optional;
import java.util.Set;

import org.sentilo.common.domain.CatalogAlert;
import org.sentilo.common.domain.CatalogSensor;
import org.sentilo.common.enums.EventType;
import org.sentilo.platform.common.domain.Alert;
import org.sentilo.platform.common.domain.Sensor;

public interface ResourceService {

  Long registerProviderIfNeedBe(String providerId);

  Long registerSensorIfNeedBe(final CatalogSensor sensor, boolean update);

  Set<String> getSensorsFromProvider(String providerId);

  Optional<Sensor> getSensor(final String providerId, final String sensorId, final boolean create);

  Optional<Sensor> getSensor(Long sid);

  void checkSensorState(final String providerId, final String sensorId);

  Set<String> getSensorsToInspect(String providerId, String sensorId);

  Long registerAlertIfNeedBe(final CatalogAlert alert, final boolean update);

  void checkAlertState(final String alertId);

  Optional<Alert> getAlert(final String alertId);

  void removeProvider(String providerId);

  void removeSensor(String sensorId, String providerId);

  void removeAlert(final CatalogAlert alert);

  void addOrphanEventsToRemove(final EventType type, final String zset, final List<String> eventsIds);
}
