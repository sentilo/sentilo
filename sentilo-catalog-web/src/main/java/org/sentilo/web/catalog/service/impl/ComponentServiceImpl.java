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

import org.sentilo.common.utils.SentiloUtils;
import org.sentilo.web.catalog.domain.Component;
import org.sentilo.web.catalog.repository.ComponentRepository;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.search.SearchFilterResult;
import org.sentilo.web.catalog.service.ComponentService;
import org.sentilo.web.catalog.service.SensorService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.geo.Box;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.stereotype.Service;

@Service
public class ComponentServiceImpl extends AbstractBaseCrudServiceImpl<Component> implements ComponentService {

  @Autowired
  private ComponentRepository repository;

  @Autowired
  private SensorService sensorService;

  public ComponentServiceImpl() {
    super(Component.class);
  }

  @Override
  public ComponentRepository getRepository() {
    return repository;
  }

  public void setRepository(final ComponentRepository repository) {
    this.repository = repository;
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.web.catalog.service.impl.AbstractBaseCrudServiceImpl#getEntityId(org.sentilo.web
   * .catalog .domain.CatalogDocument)
   */
  public String getEntityId(final Component entity) {
    return entity.getId();
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.service.ComponentService#findByName(java.lang.String,
   * java.lang.String)
   */
  public Component findByName(final String providerId, final String name) {
    final SearchFilter filter = new SearchFilter();
    filter.addAndParam("providerId", providerId);
    filter.addAndParam("name", name);

    return getMongoOps().findOne(buildQuery(filter), Component.class);
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.service.ComponentService#geoSpatialSearch(org.sentilo.web.catalog.
   * search .SearchFilter)
   */
  public SearchFilterResult<Component> geoSpatialSearch(final SearchFilter filter) {
    if (SentiloUtils.arrayIsEmpty(filter.getBounds())) {
      return super.search(filter);
    }

    // bounds = [lat_lo_left,lng_lo_left,lat_hi_west,lng_hi_west]
    final String[] mapBounds = filter.getBounds();
    final double[] lowerLeft = {Double.parseDouble(mapBounds[1]), Double.parseDouble(mapBounds[0])};
    final double[] upperRight = {Double.parseDouble(mapBounds[3]), Double.parseDouble(mapBounds[2])};
    final Box mapBox = new Box(lowerLeft, upperRight);
    final Criteria geoSpatialCriteria = Criteria.where("location.centroid").within(mapBox);

    final Query query = buildQuery(filter, false, geoSpatialCriteria);
    LOGGER.debug("GeoSpatial Search - query: {}", query);

    final List<Component> content = getMongoOps().find(query, Component.class);

    return new SearchFilterResult<Component>(content);

  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.service.ComponentService#changeAccessType(java.lang.String[],
   * boolean)
   */
  public void changeAccessType(final String[] componentsIds, final Boolean isPublicAccess) {
    updateMulti(Arrays.asList(componentsIds), "publicAccess", isPublicAccess);
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.service.ComponentService#findByProvider(java.lang.String)
   */
  @Override
  public List<Component> findByProvider(final String providerId) {
    final SearchFilter filter = new SearchFilter();
    filter.addAndParam("providerId", providerId);
    final Query providerFilter = buildQuery(filter);
    return getMongoOps().find(providerFilter, Component.class);
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.service.ComponentService#deleteComponents(java.lang.String,
   * java.lang.String[])
   */
  public void deleteComponents(final String providerId, final String[] componentsNames) {
    super.delete(getComponentsFromNames(providerId, componentsNames));
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.service.ComponentService#findByType(java.lang.String,
   * java.lang.String)
   */
  public List<Component> findByType(final String providerId, final String componentType) {
    final SearchFilter filter = new SearchFilter();
    filter.addAndParam("providerId", providerId);
    filter.addAndParam("componentType", componentType);
    return getMongoOps().find(buildQuery(filter), Component.class);
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.web.catalog.service.impl.AbstractBaseCrudServiceImpl#doAfterDelete(org.sentilo.web.
   * catalog.domain.CatalogDocument)
   */
  protected void doAfterDelete(final Component component) {
    doAfterDelete(Arrays.asList(new Component[] {component}));
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.service.impl.AbstractBaseCrudServiceImpl#doAfterDelete(java.util.
   * Collection)
   */
  protected void doAfterDelete(final Collection<Component> entities) {
    // After delete the components, a second step should be done: remove the sensors children and
    // the relations with other components
    final List<String> componentsIds = new ArrayList<String>();
    for (final Component component : entities) {
      componentsIds.add(component.getId());
    }
    sensorService.deleteSensorsFromComponents(componentsIds);
    disconnectChildrenComponents(componentsIds);
  }

  private List<Component> getComponentsFromNames(final String providerId, final String[] componentsNames) {
    final SearchFilter searchFilter = new SearchFilter();
    searchFilter.addAndParam("providerId", providerId);
    searchFilter.addAndParam("name", componentsNames);

    final Query nameFilter = buildQuery(searchFilter);
    return getMongoOps().find(nameFilter, Component.class);
  }

  private void disconnectChildrenComponents(final List<String> componentsIds) {
    updateMulti(componentsIds, "parentId", null);
  }

}
