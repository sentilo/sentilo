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
package org.sentilo.web.catalog.service.impl;

import java.util.ArrayList;
import java.util.Collection;

import org.sentilo.common.domain.CatalogProvider;
import org.sentilo.common.domain.CatalogSensor;
import org.sentilo.common.rest.RESTClient;
import org.sentilo.web.catalog.domain.CatalogDocument;
import org.sentilo.web.catalog.domain.PlatformAdminInputMessage;
import org.sentilo.web.catalog.domain.PlatformStatsMessage;
import org.sentilo.web.catalog.domain.Provider;
import org.sentilo.web.catalog.domain.Sensor;
import org.sentilo.web.catalog.event.DeletePlatformResourcesEvent;
import org.sentilo.web.catalog.parser.PlatformMessageParser;
import org.sentilo.web.catalog.service.PlatformService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationListener;
import org.springframework.stereotype.Service;

@Service
public class PlatformServiceImpl implements PlatformService, ApplicationListener<DeletePlatformResourcesEvent<CatalogDocument>> {

  @Autowired
  private RESTClient restClient;

  private final PlatformMessageParser parser = new PlatformMessageParser();

  @Override
  public PlatformStatsMessage getCurrentPlatformStats() {
    final String response = restClient.get("admin/stats");
    return parser.unmarshallStatsMessage(response);
  }

  @Override
  public void onApplicationEvent(final DeletePlatformResourcesEvent<CatalogDocument> event) {
    final String path = "admin/delete";
    final PlatformAdminInputMessage message = new PlatformAdminInputMessage();
    if (event.resourcesAreSensors()) {
      message.setSensors(translateSensors(event.getResources()));
    } else {
      message.setProviders(translateProviders(event.getResources()));
    }

    restClient.put(path, parser.marshall(message));
  }

  private Collection<CatalogSensor> translateSensors(final Collection<CatalogDocument> sensors) {
    final Collection<CatalogSensor> catalogSensors = new ArrayList<CatalogSensor>();
    for (final CatalogDocument document : sensors) {
      final CatalogSensor catalogSensor = new CatalogSensor();
      catalogSensor.setSensor(((Sensor) document).getSensorId());
      catalogSensor.setProvider(((Sensor) document).getProviderId());

      catalogSensors.add(catalogSensor);
    }

    return catalogSensors;
  }

  private Collection<CatalogProvider> translateProviders(final Collection<CatalogDocument> providers) {
    final Collection<CatalogProvider> catalogProviders = new ArrayList<CatalogProvider>();
    for (final CatalogDocument document : providers) {
      final CatalogProvider catalogProvider = new CatalogProvider();
      catalogProvider.setProvider(((Provider) document).getId());

      catalogProviders.add(catalogProvider);
    }

    return catalogProviders;
  }

}
