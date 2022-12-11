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
package org.sentilo.common.config.impl;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.TimeUnit;

import org.sentilo.common.config.SentiloModuleConfigRepository;
import org.sentilo.common.utils.SentiloConstants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Repository;
import org.springframework.util.CollectionUtils;

@Repository
public class SentiloModuleConfigRepositoryImpl implements SentiloModuleConfigRepository {

  protected static final Logger LOGGER = LoggerFactory.getLogger(SentiloModuleConfigRepositoryImpl.class);

  @Autowired(required = false)
  private RedisTemplate<String, String> redisTemplate;

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.common.config.SentiloModuleConfigRepository#saveArtifactConfig(java.lang.String,
   * java.util.Map)
   */
  @Override
  public void saveModuleConfig(final String moduleKey, final Map<String, Object> moduleConfig) {
    redisTemplate.opsForHash().putAll(moduleKey, moduleConfig);
    redisTemplate.opsForHash().put(moduleKey, "lastUpdate", Long.toString(System.currentTimeMillis()));

    // To evict orphaned keys into Redis if a component shutdown, a new TTL is fixed each time
    // component config is updated
    redisTemplate.expire(moduleKey, 1, TimeUnit.HOURS);

    redisTemplate.opsForSet().add(SentiloConstants.GLOBAL_CONFIG_LIST_KEY, moduleKey);

    LOGGER.debug("{} config successfully stored in Redis", moduleKey);
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.common.config.SentiloModuleConfigRepository#readArtifactConfig(java.lang.String)
   */
  @Override
  public Map<String, Object> readModuleConfig(final String moduleKey) {
    final Map<String, Object> params = new HashMap<String, Object>();
    // Each key that starts with (*) suffix contains sensitive data, so its value is
    // obfuscated before return response.
    redisTemplate.opsForHash().entries(moduleKey).forEach((k, v) ->
      {
        final String k1 = ((String) k).trim();
        final Object v1 = (k1.startsWith(SentiloConstants.CONFIG_SENSITIVE_KEY_PREFIX)) ? SentiloConstants.CONFIG_SENSITIVE_VALUE_MASK : v;
        params.put(k1.replace(SentiloConstants.CONFIG_SENSITIVE_KEY_PREFIX, ""), v1);
      });

    return params;
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.common.config.SentiloModuleConfigRepository#readGlobalConfig()
   */
  @Override
  public Map<String, Map<String, Object>> readGlobalConfig() {
    final Map<String, Map<String, Object>> globalConfig = new HashMap<String, Map<String, Object>>();

    final Set<String> modulesKeys = redisTemplate.opsForSet().members(SentiloConstants.GLOBAL_CONFIG_LIST_KEY);
    for (final String moduleKey : modulesKeys) {
      final Map<String, Object> params = readModuleConfig(moduleKey);

      if (!CollectionUtils.isEmpty(params)) {
        globalConfig.put(moduleKey, params);
      } else {
        redisTemplate.opsForSet().remove(SentiloConstants.GLOBAL_CONFIG_LIST_KEY, moduleKey);
      }
    }

    return globalConfig;
  }

}
