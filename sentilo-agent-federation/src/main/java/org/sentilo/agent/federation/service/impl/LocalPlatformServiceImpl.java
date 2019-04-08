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
