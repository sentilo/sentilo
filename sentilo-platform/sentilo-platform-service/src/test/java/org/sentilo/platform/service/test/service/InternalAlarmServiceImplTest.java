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
package org.sentilo.platform.service.test.service;

import static org.mockito.Matchers.argThat;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentMatcher;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.platform.common.domain.EntityMetadataMessage;
import org.sentilo.platform.common.domain.Observation;
import org.sentilo.platform.common.ratelimiter.QuotaContext;
import org.sentilo.platform.common.security.RequesterContext;
import org.sentilo.platform.common.security.RequesterContextHolder;
import org.sentilo.platform.common.security.repository.EntityMetadataRepository;
import org.sentilo.platform.service.impl.InternalAlarmServiceImpl;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.util.StringUtils;

public class InternalAlarmServiceImplTest {

  @Mock
  private StringRedisTemplate redisTemplate;

  @Mock
  private RequesterContext requesterContext;

  @Mock
  private EntityMetadataRepository entityMetadataRepository;

  @InjectMocks
  private InternalAlarmServiceImpl service;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    RequesterContextHolder.setContext(requesterContext);
  }

  @After
  public void tearDown() {
    RequesterContextHolder.clearContext();
  }

  @Test
  public void publishGhostSensorAlarm() {
    final String provider = "mockProvider";
    final String sensor = "mockSensor";
    final Observation data = Mockito.mock(Observation.class);

    when(data.getProvider()).thenReturn(provider);
    when(data.getSensor()).thenReturn(sensor);

    // Invoke two times publishGhostSensorAlarm method and check that only one alarm is published
    service.publishGhostSensorAlarm(data);
    service.publishGhostSensorAlarm(data);

    verify(redisTemplate).convertAndSend(eq("/alarm/mockProvider/mockSensor"), argThat(new AlertIdMatcher(SentiloConstants.GHOST_SENSOR_ALERT)));
  }

  @Test
  public void publishInboundRateLimitingAlarms() {
    final String provider = "mockProvider";
    final QuotaContext quotaContext = Mockito.mock(QuotaContext.class);
    when(quotaContext.getType()).thenReturn(QuotaContext.Type.ENTITY);
    when(quotaContext.getLimit()).thenReturn(10l);

    // Invoke two times publishInboundRateLimitingAlarm method and check that only one alarm is
    // published
    service.publishInboundRateLimiterAlarm(provider, quotaContext);
    service.publishInboundRateLimiterAlarm(provider, quotaContext);

    verify(redisTemplate).convertAndSend(eq("/alarm/mockProvider"), argThat(new AlertIdMatcher(SentiloConstants.OVER_QUOTA_INBOUND_ALERT)));
  }

  @Test
  public void publishOutboundRateLimitingAlarm() {
    final String provider = "mockProvider";
    final QuotaContext quotaContext = Mockito.mock(QuotaContext.class);
    final EntityMetadataMessage emm = Mockito.mock(EntityMetadataMessage.class);
    when(entityMetadataRepository.getEntityMetadataFromId(provider)).thenReturn(emm);
    when(quotaContext.getType()).thenReturn(QuotaContext.Type.ENTITY);
    when(quotaContext.getLimit()).thenReturn(10l);

    // Invoke two times publishOutboundRateLimitingAlarm method and check that only one alarm is
    // published
    service.publishOutboundRateLimiterAlarm(provider, quotaContext);
    service.publishOutboundRateLimiterAlarm(provider, quotaContext);

    verify(redisTemplate).convertAndSend(eq("/alarm/mockProvider"), argThat(new AlertIdMatcher(SentiloConstants.OVER_QUOTA_OUTBOUND_ALERT)));
  }

  class AlertIdMatcher extends ArgumentMatcher<String> {

    private final String alertId;

    public AlertIdMatcher(final String alertId) {
      this.alertId = alertId;
    }

    @Override
    public boolean matches(final Object argument) {
      final String msg = (String) argument;
      return StringUtils.hasText(msg) && msg.contains(alertId);
    }

  }
}
