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

import org.sentilo.web.catalog.domain.Alert;
import org.sentilo.web.catalog.domain.Provider;
import org.sentilo.web.catalog.domain.Sensor;
import org.sentilo.web.catalog.domain.SyncResource;
import org.sentilo.web.catalog.utils.Constants;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.mongodb.core.MongoOperations;
import org.springframework.data.mongodb.core.mapping.event.AbstractMongoEventListener;
import org.springframework.stereotype.Component;

import com.mongodb.DBObject;

@Component
public class SyncResourceEventListener extends AbstractMongoEventListener<SyncResource> {

  @Autowired
  private MongoOperations mongoOps;

  @Override
  public void onBeforeSave(final SyncResource resource, final DBObject dbo) {
    // Mark a resource pending to sync if it has change its state
    if (!Provider.class.isAssignableFrom(resource.getClass()) && stateHasChange(resource)) {
      dbo.put(Constants.SYNC_FIELD, null);
    }
  }

  private boolean stateHasChange(final SyncResource resource) {
    boolean hasChange = true;
    // Get the current resource stored on MongoDB
    final Object currentResource = mongoOps.findById(resource.getId(), resource.getClass());
    if (currentResource != null && Sensor.class.isAssignableFrom(resource.getClass())) {
      hasChange = sensorStateHasChange((Sensor) resource, (Sensor) currentResource);
    } else if (currentResource != null && Alert.class.isAssignableFrom(resource.getClass())) {
      hasChange = alertStateHasChange((Alert) resource, (Alert) currentResource);
    }

    return hasChange;
  }

  private boolean sensorStateHasChange(final Sensor newSensor, final Sensor currentSensor) {
    return !newSensor.getState().equals(currentSensor.getState());
  }

  private boolean alertStateHasChange(final Alert newAlert, final Alert currentAlert) {
    return newAlert.isActive() != currentAlert.isActive();
  }
}
