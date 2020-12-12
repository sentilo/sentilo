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
package org.sentilo.common.metrics.repository.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.concurrent.TimeUnit;

import org.sentilo.common.converter.DefaultStringMessageConverter;
import org.sentilo.common.converter.StringMessageConverter;
import org.sentilo.common.metrics.SentiloArtifactMetrics;
import org.sentilo.common.metrics.repository.SentiloArtifactMetricsRepository;
import org.sentilo.common.utils.SentiloConstants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.listener.ChannelTopic;
import org.springframework.data.redis.listener.Topic;
import org.springframework.stereotype.Repository;
import org.springframework.util.StringUtils;

@Repository
public class SentiloArtifactMetricsRepositoryImpl implements SentiloArtifactMetricsRepository {

  protected static final Logger LOGGER = LoggerFactory.getLogger(SentiloArtifactMetricsRepositoryImpl.class);

  private final StringMessageConverter converter = new DefaultStringMessageConverter();

  @Autowired(required = false)
  private RedisTemplate<String, String> redisTemplate;

  /*
   * (non-Javadoc)
   * 
   * @see
   * org.sentilo.common.metrics.repository.SentiloArtifactMetricsRepository#saveArtifactConfig(org.
   * sentilo.common.metrics.SentiloArtifactMetrics)
   */
  public void saveArtifactMetrics(final SentiloArtifactMetrics artifactMetrics) {
    // Artifact metrics are saved in Redis as a new <key,value> entry and are also published as an
    // event to be delivered to potentials consumers
    final String eventMessage = converter.marshal(artifactMetrics);
    redisTemplate.opsForValue().set(artifactMetrics.getName(), eventMessage);
    // To evict orphaned keys into Redis if an artifact shutdown, a new TTL is fixed each time
    // component config
    // is updated
    redisTemplate.expire(artifactMetrics.getName(), 1, TimeUnit.HOURS);

    // Add metrics key name to the global metrics list
    redisTemplate.opsForSet().add(SentiloConstants.GLOBAL_METRICS_LIST_KEY, artifactMetrics.getName());

    LOGGER.debug("{} metrics successfully stored in Redis", artifactMetrics.getName());

    // Finally, publish metrics
    publishMetrics(artifactMetrics);
  }

  /*
   * (non-Javadoc)
   * 
   * @see
   * org.sentilo.common.metrics.repository.SentiloArtifactMetricsRepository#getArtifactsMetrics()
   */
  public List<SentiloArtifactMetrics> getArtifactsMetrics() {
    final List<SentiloArtifactMetrics> artifactsMetrics = new ArrayList<SentiloArtifactMetrics>();
    final Set<String> artifactsMetricsKeys = redisTemplate.opsForSet().members(SentiloConstants.GLOBAL_METRICS_LIST_KEY);
    for (final String key : artifactsMetricsKeys) {
      final String sArtifactMetrics = redisTemplate.opsForValue().get(key);
      if (StringUtils.hasText(sArtifactMetrics)) {
        final SentiloArtifactMetrics artifactMetrics = (SentiloArtifactMetrics) converter.unmarshal(sArtifactMetrics, SentiloArtifactMetrics.class);
        artifactsMetrics.add(artifactMetrics);
      } else {
        redisTemplate.opsForSet().remove(SentiloConstants.GLOBAL_METRICS_LIST_KEY, key);
      }
    }

    return artifactsMetrics;
  }

  private void publishMetrics(final SentiloArtifactMetrics artifactMetrics) {
    final Topic topic = new ChannelTopic(SentiloConstants.METRICS_TOPIC + artifactMetrics.getName());
    final String eventMessage = converter.marshal(artifactMetrics);
    LOGGER.debug("Published artifact metrics message: {}", eventMessage);
    redisTemplate.convertAndSend(topic.getTopic(), eventMessage);
  }
}
