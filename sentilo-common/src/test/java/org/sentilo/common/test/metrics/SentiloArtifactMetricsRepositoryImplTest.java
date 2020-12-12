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
package org.sentilo.common.test.metrics;

import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.converter.DefaultStringMessageConverter;
import org.sentilo.common.metrics.SentiloArtifactMetrics;
import org.sentilo.common.metrics.repository.impl.SentiloArtifactMetricsRepositoryImpl;
import org.sentilo.common.metrics.service.impl.SentiloArtifactMetricsServiceImpl;
import org.sentilo.common.utils.SentiloConstants;
import org.springframework.data.redis.core.HashOperations;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.SetOperations;
import org.springframework.data.redis.core.ValueOperations;

public class SentiloArtifactMetricsRepositoryImplTest {

  @Mock
  private RedisTemplate<String, String> redisTemplate;

  @Mock
  private HashOperations<String, Object, Object> hashOps;

  @Mock
  private SetOperations<String, String> setOps;

  @Mock
  private ValueOperations<String, String> valOps;

  @InjectMocks
  private SentiloArtifactMetricsRepositoryImpl repository;

  private MockMetricsServiceImpl metricsService = new MockMetricsServiceImpl();

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void saveArtifactConfig() {
    final SentiloArtifactMetrics artifactMetrics = metricsService.collect();
    final String topicName = SentiloConstants.METRICS_TOPIC + artifactMetrics.getName();
    final String eventMessage = new DefaultStringMessageConverter().marshal(artifactMetrics);
    when(redisTemplate.opsForValue()).thenReturn(valOps);
    when(redisTemplate.opsForSet()).thenReturn(setOps);

    repository.saveArtifactMetrics(artifactMetrics);

    verify(valOps).set(eq(artifactMetrics.getName()), anyString());
    verify(setOps).add(SentiloConstants.GLOBAL_METRICS_LIST_KEY, artifactMetrics.getName());
    verify(redisTemplate).convertAndSend(eq(topicName), eq(eventMessage));

  }

  @Test
  public void getArtifactsMetrics() {
    final String artifact = "sentilo:abcd_1:relational:metrics";
    final String artifactMetrics =
        "{\"name\":\"sentilo:abcd_1:relational:metrics\",\"metrics\":{\"file_systems\":[{\"path\":\"/\",\"usable\":81479135232,\"total\":493717811200,\"free\":81479135232}],"
            + "\"system_memory\":{\"total\":17065136128,\"used\":13131243520,\"free\":3933892608},\"cpu\":{\"number\":8,\"system_load\":-1.0,\"process_load\":-1.0},\"threads\":{\"total\":15,\"started\":26,\"daemon\":8},"
            + "\"process_memory\":{\"non_heap\":{\"init\":2555904,\"committed\":37617664,\"max\":-1,\"used\":36758864},\"heap\":{\"init\":268435456,\"committed\":265814016,\"max\":3793747968,\"used\":39443688}},"
            + "\"events\":{\"output\":0,\"input\":4,\"remote_server_connection\":\"OK\",\"retry_queue_size\":0},\"data_sources\":[{\"size\":10,\"idle\":10,\"wait_count\":0,\"name\":\"psabDs\",\"active\":0,\"max_active\":10,"
            + "\"min_idle\":1},{\"size\":10,\"idle\":10,\"wait_count\":0,\"name\":\"regDs\",\"active\":0,\"max_active\":10,\"min_idle\":1}]},\"timestamp\":3597467428178}";
    final Set<String> members = new HashSet<String>(Arrays.asList(artifact));
    when(redisTemplate.opsForSet()).thenReturn(setOps);
    when(setOps.members(SentiloConstants.GLOBAL_METRICS_LIST_KEY)).thenReturn(members);
    when(redisTemplate.opsForValue()).thenReturn(valOps);
    when(valOps.get(artifact)).thenReturn(artifactMetrics);

    final List<SentiloArtifactMetrics> metrics = repository.getArtifactsMetrics();

    verify(valOps).get(artifact);
    verify(setOps).members(SentiloConstants.GLOBAL_METRICS_LIST_KEY);
    Assert.assertTrue(metrics.size() == 1);
    Assert.assertEquals(artifact, metrics.get(0).getName());

  }

  @Test
  public void getArtifactsMetrics_empty() {
    final String artifact = "sentilo:abcd_1:relational:metrics";
    final Set<String> members = new HashSet<String>(Arrays.asList(artifact));
    when(redisTemplate.opsForSet()).thenReturn(setOps);
    when(setOps.members(SentiloConstants.GLOBAL_METRICS_LIST_KEY)).thenReturn(members);
    when(redisTemplate.opsForValue()).thenReturn(valOps);
    when(valOps.get(artifact)).thenReturn(null);

    final List<SentiloArtifactMetrics> metrics = repository.getArtifactsMetrics();

    verify(valOps).get(artifact);
    verify(setOps).members(SentiloConstants.GLOBAL_METRICS_LIST_KEY);
    Assert.assertTrue(metrics.size() == 0);
  }

  class MockMetricsServiceImpl extends SentiloArtifactMetricsServiceImpl {

    @Override
    protected Map<String, Object> collectCustomMetrics() {
      final Map<String, Object> customMetrics = new HashMap<String, Object>();
      customMetrics.put("custom.metric.1", "value-1");
      customMetrics.put("custom.metric.2", "value-2");
      return customMetrics;
    }

    @Override
    public String getName() {
      return "mockArtifact";
    }

    @Override
    protected ArtifactState getStatus(final SentiloArtifactMetrics artifactMetrics) {
      return ArtifactState.GREEN;
    }
  }

}
