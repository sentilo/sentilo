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
import org.sentilo.common.domain.CatalogProvider;
import org.sentilo.common.domain.CatalogSensor;
import org.sentilo.web.catalog.domain.Alert;
import org.sentilo.web.catalog.domain.CatalogDocument;
import org.sentilo.web.catalog.domain.Provider;
import org.sentilo.web.catalog.domain.Sensor;
import org.springframework.util.StringUtils;

public abstract class PlatformConverter {

  public static Collection<CatalogSensor> translateSensors(final Collection<? extends CatalogDocument> sensors) {
    final Collection<CatalogSensor> catalogSensors = new ArrayList<CatalogSensor>();
    for (final CatalogDocument document : sensors) {
      final CatalogSensor catalogSensor = new CatalogSensor();
      catalogSensor.setSensor(((Sensor) document).getSensorId());
      catalogSensor.setProvider(((Sensor) document).getProviderId());

      if (((Sensor) document).getState() != null) {
        catalogSensor.setState(((Sensor) document).getState());
      }

      catalogSensors.add(catalogSensor);
    }

    return catalogSensors;
  }

  public static Collection<CatalogProvider> translateProviders(final Collection<? extends CatalogDocument> providers) {
    final Collection<CatalogProvider> catalogProviders = new ArrayList<CatalogProvider>();
    for (final CatalogDocument document : providers) {
      final CatalogProvider catalogProvider = new CatalogProvider();
      catalogProvider.setProvider(((Provider) document).getId());

      catalogProviders.add(catalogProvider);
    }

    return catalogProviders;
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
