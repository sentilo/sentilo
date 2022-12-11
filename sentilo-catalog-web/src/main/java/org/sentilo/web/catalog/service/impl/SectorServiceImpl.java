/*
 * Sentilo
 *  
 * Original version 1.4 Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de
 * Barcelona. Modified by Opentrends adding support for multitenant deployments and SaaS. 
 * Modifications on version 1.5 Copyright (C) 2015 Opentrends Solucions i Sistemes, S.L.
 * 
 *   
 * This program is licensed and may be used, modified and redistributed under the terms  of the 
 * European Public License (EUPL), either version 1.1 or (at your option) any later version as soon 
 * as they are approved by the European Commission.
 *   
 * Alternatively, you may redistribute and/or modify this program under the terms of the GNU Lesser 
 * General Public License as published by the Free Software Foundation; either  version 3 of the 
 * License, or (at your option) any later version. 
 *   
 * Unless required by applicable law or agreed to in writing, software distributed under the License 
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR  CONDITIONS OF ANY KIND, either express 
 * or implied. 
 *   
 * See the licenses for the specific language governing permissions, limitations and more details.
 *   
 * You should have received a copy of the EUPL1.1 and the LGPLv3 licenses along with this program; 
 * if not, you may find them at: 
 *   
 * https://joinup.ec.europa.eu/software/page/eupl/licence-eupl http://www.gnu.org/licenses/   and 
 * https://www.gnu.org/licenses/lgpl.txt
 */
package org.sentilo.web.catalog.service.impl;

import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

import org.sentilo.web.catalog.domain.ActiveSubscription;
import org.sentilo.web.catalog.domain.Alert;
import org.sentilo.web.catalog.domain.AlertRule;
import org.sentilo.web.catalog.domain.Application;
import org.sentilo.web.catalog.domain.Component;
import org.sentilo.web.catalog.domain.Provider;
import org.sentilo.web.catalog.domain.Sector;
import org.sentilo.web.catalog.domain.Sensor;
import org.sentilo.web.catalog.domain.User;
import org.sentilo.web.catalog.repository.SectorRepository;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.service.SectorService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.data.mongodb.core.query.Update;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.stereotype.Service;

@Service
public class SectorServiceImpl extends AbstractBaseCrudServiceImpl<Sector> implements SectorService {

  @Autowired
  private SectorRepository sectorRepository;

  public SectorServiceImpl() {
    super(Sector.class);
  }

  @Override
  public MongoRepository<Sector, String> getRepository() {
    return sectorRepository;
  }

  @Override
  public String getEntityId(final Sector entity) {
    return entity.getId();
  }

  @Override
  public void deleteFromTenant(final String tenantId) {
    final SearchFilter filter = new SearchFilter();
    filter.addAndParam("tenantId", tenantId);

    delete(filter);
  }

  @Override
  protected void doAfterDelete(final Collection<Sector> sectors) {
    final List<String> sectorsIds = sectors.stream().map(sector -> sector.getId()).collect(Collectors.toList());
    String[] aSectorsIds = new String[sectorsIds.size()];
    aSectorsIds = sectorsIds.toArray(aSectorsIds);
    final Query query = new Query();
    final Update update = new Update().pull("sectors", buildQueryForParamInCollection("sectorId", sectorsIds));
    getMongoOps().updateMulti(query, update, Application.class);
    getMongoOps().updateMulti(query, update, Provider.class);
    getMongoOps().updateMulti(query, update, Component.class);
    getMongoOps().updateMulti(query, update, Sensor.class);
    getMongoOps().updateMulti(query, update, Alert.class);
    getMongoOps().updateMulti(query, update, ActiveSubscription.class);
    getMongoOps().updateMulti(query, update, AlertRule.class);
    getMongoOps().updateMulti(query, new Update().pullAll("sectors", aSectorsIds), User.class);
  }

}
