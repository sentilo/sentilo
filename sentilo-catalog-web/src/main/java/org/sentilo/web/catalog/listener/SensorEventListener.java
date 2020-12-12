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
package org.sentilo.web.catalog.listener;

import org.sentilo.web.catalog.domain.Sensor;
import org.sentilo.web.catalog.service.PlatformService;
import org.sentilo.web.catalog.service.SensorService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.mongodb.core.mapping.event.AbstractMongoEventListener;
import org.springframework.data.mongodb.core.mapping.event.BeforeConvertEvent;

@org.springframework.stereotype.Component
public class SensorEventListener extends AbstractMongoEventListener<Sensor> {

  @Autowired
  private SensorService sensorService;

  @Autowired
  private PlatformService platformService;

  @Override
  public void onBeforeConvert(final BeforeConvertEvent<Sensor> event) {
    checkStateChange(event.getSource());
    checkTtl(event.getSource());
  }

  private void checkStateChange(final Sensor newSensor) {
    // If sensor state has change, this change should be notified.
    final Sensor currentSensor = sensorService.find(newSensor);
    if (currentSensor != null && !newSensor.getState().equals(currentSensor.getState())) {
      sensorService.notifyStateChange(newSensor);
    }
  }

  private void checkTtl(final Sensor sensor) {
    // If sensor TTL is null it means that its TTL equals default platform TTL
    if (sensor.getTtl() == null) {
      sensor.setTtl(platformService.getPlatformTtl());
    }
  }
}
