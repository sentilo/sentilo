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
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.sentilo.web.catalog.domain.Alert;
import org.sentilo.web.catalog.repository.AlertRepository;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.service.AlertService;
import org.sentilo.web.catalog.service.ProviderService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

@Service
public class AlertServiceImpl extends AbstractBaseServiceImpl<Alert> implements AlertService {

  @Autowired
  private AlertRepository repository;

  @Autowired
  private ProviderService providerService;

  public AlertServiceImpl() {
    super(Alert.class);
  }

  @Override
  public AlertRepository getRepository() {
    return repository;
  }

  @Override
  public String getEntityId(final Alert entity) {
    return entity.getId();
  }

  /*
   * (non-Javadoc)
   * 
   * @see
   * org.sentilo.web.catalog.service.impl.AbstractBaseServiceImpl#create(org.sentilo.web.catalog
   * .domain.CatalogDocument)
   */
  public Alert create(final Alert alert) {
    // If name is null, we initialize it with the id value
    if (alert.getName() == null) {
      alert.setName(alert.getId());
    }

    return super.create(alert);
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.sentilo.web.catalog.service.AlertService#getAlertsByEntities(java.util.Collection,
   * java.util.Map)
   */
  public List<Alert> getAlertsByEntities(final Collection<String> entities, final Map<String, Object> filterParams) {
    final List<Alert> alerts = new ArrayList<Alert>();

    // Podemos filtrar la búsqueda de alertas por lo siguientes criterios:
    // 1. Tipo: INTERNAL o EXTERNAL
    // 2. Trigger: codificado según el valor de la enumeracion alarm.Type
    if (!CollectionUtils.isEmpty(entities)) {
      final Query queryForProviderAlerts = buildQuery("providerId", entities, filterParams);
      final Query queryForClientAppAlerts = buildQuery("applicationId", entities, filterParams);

      final List<Alert> providerAlerts = getMongoOps().find(queryForProviderAlerts, Alert.class);
      final List<Alert> clientAppAlerts = getMongoOps().find(queryForClientAppAlerts, Alert.class);

      if (!CollectionUtils.isEmpty(providerAlerts)) {
        alerts.addAll(providerAlerts);
      }

      if (CollectionUtils.isEmpty(clientAppAlerts)) {
        alerts.addAll(clientAppAlerts);
      }
    }
    return alerts;
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.sentilo.web.catalog.service.AlertService#deleteAllAlerts(java.lang.String)
   */
  public void deleteOwnAlerts(final String entityOwnerId) {
    // This method must remove all alerts with providerId or applicationId equals to entityOwnerId
    // and alert type equals to EXTERNAL
    final SearchFilter filter = new SearchFilter();
    filter.addParam("providerId", entityOwnerId);
    filter.addParam("applicationId", entityOwnerId);
    filter.addAndParam("type", Alert.Type.EXTERNAL.name());

    getMongoOps().remove(buildQuery(filter), Alert.class);
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.sentilo.web.catalog.service.AlertService#deleteOwnAlerts(java.util.Collection,
   * java.lang.String)
   */
  public void deleteOwnAlerts(final Collection<String> alertsIds, final String entityOwnerId) {
    // This method must remove all alerts with id into alertsIds collection,
    // providerId or applicationId equals to entityOwnerId
    // and alert type equals to EXTERNAL
    final boolean isEntityProvider = providerService.exist(entityOwnerId);
    final Map<String, Object> filterParams = new HashMap<String, Object>();
    filterParams.put("type", "EXTERNAL");
    if (isEntityProvider) {
      filterParams.put("providerId", entityOwnerId);
    } else {
      filterParams.put("applicationId", entityOwnerId);
    }

    getMongoOps().remove(buildQuery("id", alertsIds, filterParams), Alert.class);

  }

  private Query buildQuery(final String paramName, final Collection<String> values, final Map<String, Object> filterParams) {
    Criteria queryCriteria = Criteria.where(paramName).in(values);
    if (!CollectionUtils.isEmpty(filterParams)) {
      final Criteria[] aCriteria = buildAndParamsCriteria(filterParams);
      queryCriteria = queryCriteria.andOperator(aCriteria);
    }

    return new Query(queryCriteria);
  }

}
