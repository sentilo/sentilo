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
package org.sentilo.web.catalog.dto;

import java.util.ArrayList;
import java.util.List;

import org.sentilo.web.catalog.domain.Application;
import org.sentilo.web.catalog.domain.Provider;

public class EntitiesMetadataDTO {

  private List<EntityMetadataDTO> entitiesMetadata;

  public EntitiesMetadataDTO() {
    entitiesMetadata = new ArrayList<EntityMetadataDTO>();
  }

  public EntitiesMetadataDTO(final List<EntityMetadataDTO> entitiesMetadata) {
    this.entitiesMetadata = entitiesMetadata;
  }

  public List<EntityMetadataDTO> getEntitiesMetadata() {
    return entitiesMetadata;
  }

  public void setEntitiesMetadata(final List<EntityMetadataDTO> entitiesMetadata) {
    this.entitiesMetadata = entitiesMetadata;
  }

  public void addAll(final List<EntityMetadataDTO> entitiesMetadata) {
    this.entitiesMetadata.addAll(entitiesMetadata);
  }

  public void addAllApplications(final List<Application> applications) {
    for (final Application app : applications) {
      entitiesMetadata.add(
          new EntityMetadataDTO(app.getId(), app.getToken(), app.getTenantId(), app.isRestHttps(), app.getApiInputQuota(), app.getApiOutputQuota()));
    }
  }

  public void addAllProviders(final List<Provider> providers) {
    for (final Provider provider : providers) {
      entitiesMetadata.add(new EntityMetadataDTO(provider.getId(), provider.getToken(), provider.getTenantId(), provider.isRestHttps(),
          provider.getApiInputQuota(), provider.getApiOutputQuota()));
    }
  }

  public void add(final EntityMetadataDTO entityMetadata) {
    entitiesMetadata.add(entityMetadata);
  }

}
