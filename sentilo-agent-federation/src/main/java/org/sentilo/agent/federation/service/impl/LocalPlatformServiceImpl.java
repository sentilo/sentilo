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
package org.sentilo.agent.federation.service.impl;

import java.util.List;

import org.sentilo.agent.federation.domain.Application;
import org.sentilo.agent.federation.domain.FederationConfig;
import org.sentilo.agent.federation.service.LocalPlatformService;
import org.sentilo.common.cache.LRUCache;
import org.sentilo.common.cache.impl.LRUCacheImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.stereotype.Service;

@Service
public class LocalPlatformServiceImpl implements LocalPlatformService {

  final static int CACHE_SIZE = 100;
  final static int EXPIRE_MINUTES = 10;

  @Autowired
  private MongoTemplate mongoTemplate;

  @Value("${sentilo.master.application.id:sentilo-catalog}")
  private String sentiloMasterAppId;

  private String sentiloMasterAppToken;

  private LRUCache<String, FederationConfig> _cache = new LRUCacheImpl<String, FederationConfig>(CACHE_SIZE, EXPIRE_MINUTES);

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.agent.federation.service.LocalPlatformService#getFederatedConfig(java.lang.String)
   */
  public FederationConfig getFederatedConfig(final String federatedId) {
    FederationConfig fConfig = _cache.get(federatedId);

    if (fConfig == null) {
      fConfig = getFromRepository(federatedId);
    }

    return fConfig;
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.agent.federation.service.LocalPlatformService#getAll()
   */
  public List<FederationConfig> getFederatedConfigs() {
    return mongoTemplate.findAll(FederationConfig.class);
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.agent.federation.service.LocalPlatformService#getTokenMasterApp()
   */
  public String getTokenMasterApp() {
    if (sentiloMasterAppToken == null) {
      final Criteria criteria = Criteria.where("_id").is(sentiloMasterAppId);
      final Query query = new Query(criteria);
      final Application masterAppClient = mongoTemplate.findOne(query, Application.class);
      sentiloMasterAppToken = masterAppClient != null ? masterAppClient.getToken() : null;
    }

    return sentiloMasterAppToken;
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.agent.federation.service.LocalPlatformService#deleteFederatedConfig(org.sentilo.
   * agent .federation.domain.FederationConfig)
   */
  public void deleteFederatedConfig(final FederationConfig resource) {
    mongoTemplate.remove(resource);
    _cache.remove(resource.getId());
  }

  private FederationConfig getFromRepository(final String federatedId) {
    final Criteria criteria = Criteria.where("_id").is(federatedId);
    final Query query = new Query(criteria);
    final FederationConfig fConfig = mongoTemplate.findOne(query, FederationConfig.class);

    if (fConfig != null) {
      _cache.put(federatedId, fConfig);
    }

    return fConfig;
  }

}
