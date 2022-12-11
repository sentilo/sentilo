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
package org.sentilo.platform.service.monitor;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.sentilo.platform.service.impl.AbstractPlatformServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.util.CollectionUtils;

public abstract class AbstractMetricsServiceImpl extends AbstractPlatformServiceImpl {

  @Autowired
  protected StringRedisTemplate redisTemplate;

  protected String getHashValue(final Map<Object, Object> hash, final String fieldName, final String defaultValue) {
    return CollectionUtils.isEmpty(hash) || hash.get(fieldName) == null ? defaultValue : (String) hash.get(fieldName);
  }

  protected Long getHashValueAsLong(final Map<Object, Object> hash, final String fieldName) {
    return Long.valueOf(getHashValue(hash, fieldName, "0"));
  }

  protected String getFieldValue(final String hashKey, final String fieldKey) {
    return (String) redisTemplate.opsForHash().get(hashKey, fieldKey);
  }

  protected Map<Object, Object> getHashContent(final String hashKey) {
    return redisTemplate.opsForHash().entries(hashKey);
  }

  protected Set<String> getCountersKeys() {
    final Set<String> keys = new HashSet<String>();
    final Set<String> tenants = redisTemplate.opsForSet().members(MonitorConstants.TENANTS_KEY);
    for (final String tenant : tenants) {
      keys.add(MonitorConstants.TENANT_COUNTERS_KEY_PREFIX + tenant);
    }

    keys.add(MonitorConstants.MASTER_COUNTERS_KEY);

    return keys;
  }
}
