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
package org.sentilo.agent.location.test.batch;

import static org.mockito.Matchers.any;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.util.HashSet;
import java.util.Random;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.agent.common.metrics.AgentMetricsCounter;
import org.sentilo.agent.location.batch.ComponentLocationUpdater;
import org.sentilo.agent.location.parser.CatalogMessageConverter;
import org.sentilo.common.domain.CatalogInputMessage;
import org.sentilo.common.domain.SensorLocationElement;
import org.sentilo.common.exception.RESTClientException;
import org.sentilo.common.rest.RESTClient;
import org.sentilo.common.rest.RequestContext;
import org.springframework.test.util.ReflectionTestUtils;

public class ComponentLocationUpdaterTest {

  @Mock
  private RESTClient restClient;
  @Mock
  private CatalogMessageConverter parser;

  @InjectMocks
  private ComponentLocationUpdater componentLocationUpdater;

  @Mock
  private SensorLocationElement resource;

  @Mock
  private AgentMetricsCounter metricsCounters;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @SuppressWarnings({"unchecked"})
  @Test
  public void addResourceToUpdate() {
    componentLocationUpdater.addResourceToUpdate(resource);

    final HashSet<SensorLocationElement> currentUpdatesAwaiting = getSetField(componentLocationUpdater, "currentUpdatesAwaiting");
    Assert.assertTrue(currentUpdatesAwaiting.size() == 1);
  }

  @SuppressWarnings({"unchecked"})
  @Test
  public void addResourcesToUpdateWithSameTimestamp() {
    final HashSet<SensorLocationElement> resourcesToUpdate = getResourcesToUpdate(true);

    for (final SensorLocationElement resource : resourcesToUpdate) {
      componentLocationUpdater.addResourceToUpdate(resource);
    }

    final HashSet<SensorLocationElement> currentUpdatesAwaiting = getSetField(componentLocationUpdater, "currentUpdatesAwaiting");
    Assert.assertTrue(currentUpdatesAwaiting.size() <= 3);
  }

  @Test
  public void processEmptyList() {
    componentLocationUpdater.process();

    verify(parser, times(0)).marshall(any(CatalogInputMessage.class));
    verify(restClient, times(0)).put(any(RequestContext.class));
  }

  @SuppressWarnings({"unchecked"})
  @Test
  public void process() {
    ReflectionTestUtils.setField(componentLocationUpdater, "currentUpdatesAwaiting", getResourcesToUpdate(false));

    componentLocationUpdater.process();

    final HashSet<SensorLocationElement> currentUpdatesAwaiting = getSetField(componentLocationUpdater, "currentUpdatesAwaiting");
    final HashSet<SensorLocationElement> oldUpdatesAwaiting = getSetField(componentLocationUpdater, "oldUpdatesAwaiting");

    verify(parser).marshall(any(CatalogInputMessage.class));
    verify(restClient).put(any(RequestContext.class));
    Assert.assertTrue(currentUpdatesAwaiting.size() == 0);
    Assert.assertTrue(oldUpdatesAwaiting.size() == 0);
  }

  @SuppressWarnings({"unchecked"})
  @Test
  public void processWithErrors() {
    final HashSet<SensorLocationElement> resourcesToUpdate = getResourcesToUpdate(false);
    ReflectionTestUtils.setField(componentLocationUpdater, "currentUpdatesAwaiting", resourcesToUpdate);
    doThrow(RESTClientException.class).when(restClient).put(any(RequestContext.class));
    final int locationsToUpdate = resourcesToUpdate.size();

    componentLocationUpdater.process();

    final HashSet<SensorLocationElement> currentUpdatesAwaiting = getSetField(componentLocationUpdater, "currentUpdatesAwaiting");
    final HashSet<SensorLocationElement> oldUpdatesAwaiting = getSetField(componentLocationUpdater, "oldUpdatesAwaiting");

    verify(parser).marshall(any(CatalogInputMessage.class));
    verify(restClient).put(any(RequestContext.class));
    Assert.assertTrue(currentUpdatesAwaiting.size() == 0);
    Assert.assertTrue(oldUpdatesAwaiting.size() == locationsToUpdate);
  }

  private HashSet<SensorLocationElement> getResourcesToUpdate(final boolean sameTimestamp) {
    final HashSet<SensorLocationElement> resources = new HashSet<SensorLocationElement>();
    final Long[] timestamps = {new Long(1), new Long(2), new Long(3)};

    final Random randomGenerator = new Random();
    int size = 0;
    do {
      size = randomGenerator.nextInt(30);
    } while (size == 0);

    for (int i = 0; i < size; i++) {
      final SensorLocationElement resource = new SensorLocationElement();
      resource.setProvider("Provider");
      resource.setSensor("Sensor");
      resource.setLocation("32.111 43.1234");
      resource.setFromTsTime(sameTimestamp ? timestamps[i % 3] : System.currentTimeMillis() + size % (i + 1));

      resources.add(resource);
    }

    return resources;
  }

  @SuppressWarnings({"rawtypes"})
  private HashSet getSetField(final Object obj, final String fieldName) {
    return (HashSet) ReflectionTestUtils.getField(obj, fieldName);
  }
}
