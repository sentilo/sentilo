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
package org.sentilo.web.catalog.service.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import org.sentilo.web.catalog.domain.FederationConfig;
import org.sentilo.web.catalog.repository.FederationConfigRepository;
import org.sentilo.web.catalog.service.FederationConfigService;
import org.sentilo.web.catalog.service.ProviderService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.data.mongodb.core.query.Update;
import org.springframework.stereotype.Service;

@Service
public class FederationConfigServiceImpl extends AbstractBaseCrudServiceImpl<FederationConfig> implements FederationConfigService {

  @Autowired
  private FederationConfigRepository repository;

  @Autowired
  private ProviderService providerService;

  public FederationConfigServiceImpl() {
    super(FederationConfig.class);
  }

  public void setRepository(final FederationConfigRepository repository) {
    this.repository = repository;
  }

  @Override
  public FederationConfigRepository getRepository() {
    return repository;
  }

  @Override
  public String getEntityId(final FederationConfig entity) {
    return entity.getId();
  }

  @Override
  protected void doDelete(final Collection<FederationConfig> entities) {
    // When user select to delete a federationConfig resource from the view, internally resource is
    // marked as deleted (soft/logical delete)
    // but it isn't really removed from MongoDB.
    // The physical delete is handled by the federation agent module, which first remove remote
    // related
    // subscriptions and finally permanently delete the resource
    // from MongoDB
    final List<String> ids = new ArrayList<String>();
    for (final FederationConfig entity : entities) {
      ids.add(entity.getId());
    }

    final Query query = buildQueryForIdInCollection(ids);
    final Update update = Update.update("active", Boolean.FALSE);

    getMongoOps().updateMulti(query, update, FederationConfig.class);
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.web.catalog.service.impl.AbstractBaseCrudServiceImpl#doAfterDelete(org.sentilo.
   * web.catalog.domain.CatalogDocument)
   */
  @Override
  protected void doAfterDelete(final FederationConfig resource) {
    doAfterDelete(Arrays.asList(new FederationConfig[] {resource}));
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.web.catalog.service.impl.AbstractBaseCrudServiceImpl#delete(java.util.Collection)
   */
  @Override
  protected void doAfterDelete(final Collection<FederationConfig> resources) {
    for (final FederationConfig resource : resources) {
      deleteRelatedResources(resource);
    }
  }

  private void deleteRelatedResources(final FederationConfig resource) {
    // When a federationConfig resource is deleted also should be deleted all its related resources,
    // such as providers, components, sensors ....
    providerService.deleteFederatedResources(resource.getId());
  }
}
