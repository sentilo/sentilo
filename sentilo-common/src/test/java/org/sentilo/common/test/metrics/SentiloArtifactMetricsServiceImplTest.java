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

import static org.mockito.Matchers.any;
import static org.mockito.Mockito.verify;

import java.util.HashMap;
import java.util.Map;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.metrics.SentiloArtifactMetrics;
import org.sentilo.common.metrics.repository.SentiloArtifactMetricsRepository;
import org.sentilo.common.metrics.service.impl.SentiloArtifactMetricsServiceImpl;

public class SentiloArtifactMetricsServiceImplTest {

  @Mock
  private SentiloArtifactMetricsRepository repository;

  @InjectMocks
  private SentiloArtifactMetricsServiceImpl metricsService = new MockMetricsServiceImpl();

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void collect() {
    final SentiloArtifactMetrics artifactMetrics = metricsService.collect();

    Assert.assertTrue(artifactMetrics.getName().endsWith("mockArtifact:metrics"));
    Assert.assertTrue(artifactMetrics.getMetrics().containsKey("custom.metric.1"));
    Assert.assertTrue(artifactMetrics.getMetrics().containsKey("custom.metric.2"));
  }

  @Test
  public void collectAndSave() {
    metricsService.collectAndSave();
    verify(repository).saveArtifactMetrics(any(SentiloArtifactMetrics.class));
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
