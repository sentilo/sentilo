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
package org.sentilo.web.catalog.converter;

import java.util.ArrayList;
import java.util.Collection;

import org.sentilo.common.domain.CatalogAlert;
import org.sentilo.common.domain.CatalogEntity;
import org.sentilo.common.domain.CatalogSensor;
import org.sentilo.web.catalog.domain.Alert;
import org.sentilo.web.catalog.domain.CatalogDocument;
import org.sentilo.web.catalog.domain.Sensor;
import org.springframework.util.StringUtils;

public abstract class PlatformConverter {

  public static Collection<CatalogSensor> translateSensors(final Collection<? extends CatalogDocument> sensors) {
    final Collection<CatalogSensor> catalogSensors = new ArrayList<CatalogSensor>();
    for (final CatalogDocument document : sensors) {
      final CatalogSensor catalogSensor = new CatalogSensor();
      final Sensor sensor = (Sensor) document;
      catalogSensor.setSensor(sensor.getSensorId());
      catalogSensor.setProvider(sensor.getProviderId());
      // In the Catalog back-end, sensor TTL is stored in minutes but in PubSub back-end (Redis),
      // sensor TTL is expressed in seconds
      if (sensor.getTtl() != null) {
        catalogSensor.setTtl(sensor.getTtl() * 60);
      }

      if (sensor.getState() != null) {
        catalogSensor.setState(sensor.getState());
      }

      catalogSensors.add(catalogSensor);
    }

    return catalogSensors;
  }

  public static Collection<CatalogEntity> translateEntities(final Collection<? extends CatalogDocument> entities) {
    final Collection<CatalogEntity> catalogEntities = new ArrayList<CatalogEntity>();
    for (final CatalogDocument document : entities) {
      catalogEntities.add(new CatalogEntity(document.getId()));
    }

    return catalogEntities;
  }

  public static Collection<CatalogAlert> translateAlerts(final Collection<? extends CatalogDocument> alerts) {
    final Collection<CatalogAlert> catalogAlerts = new ArrayList<CatalogAlert>();
    for (final CatalogDocument document : alerts) {
      final Alert alert = (Alert) document;
      final CatalogAlert catalogAlert = new CatalogAlert();
      final String entityId = StringUtils.hasText(alert.getProviderId()) ? alert.getProviderId() : alert.getApplicationId();
      catalogAlert.setActive(Boolean.toString(alert.isActive()));
      catalogAlert.setEntity(entityId);
      catalogAlert.setId(alert.getId());

      catalogAlerts.add(catalogAlert);
    }

    return catalogAlerts;
  }

}
