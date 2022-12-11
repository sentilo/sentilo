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
package org.sentilo.common.test.signal;

import static org.mockito.Matchers.argThat;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.verify;

import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentMatcher;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.enums.SignalType;
import org.sentilo.common.signal.DefaultPublishSignalServiceImpl;
import org.sentilo.common.utils.ModuleUtils;
import org.sentilo.common.utils.SentiloConstants;
import org.springframework.data.redis.core.StringRedisTemplate;



public class DefaultPublishSignalServiceImplTest {

  private static final String AGENT_NAME = "mockAgent";

  @InjectMocks
  private DefaultPublishSignalServiceImpl service;

  @Mock
  private StringRedisTemplate redisTemplate;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    System.setProperty(SentiloConstants.SENTILO_AGENT_NAME_ENV, AGENT_NAME);
  }

  @Test
  public void publishInternalSignal() {
    final String[] valuesToMatch = {SignalType.RELOAD_ALERTS.name(), ModuleUtils.getModuleName()};
    
    service.publishInternalSignal(SignalType.RELOAD_ALERTS);
    
    verify(redisTemplate).convertAndSend(eq(SentiloConstants.INTERNAL_SIGNALS_TOPIC), argThat(new StringContainsMatcher(valuesToMatch)));
  }

  @Test
  public void publishInternalSignal_with_sender() {
    final String sender = "externalMockAgent";
    final String[] valuesToMatch = {SignalType.RELOAD_ALERTS.name(), sender};

    service.publishInternalSignal(SignalType.RELOAD_ALERTS, sender);

    verify(redisTemplate).convertAndSend(eq(SentiloConstants.INTERNAL_SIGNALS_TOPIC), argThat(new StringContainsMatcher(valuesToMatch)));
  }

  class StringContainsMatcher extends ArgumentMatcher<String> {

    private final String[] values;

    public StringContainsMatcher(final String[] values) {
      this.values = values;
    }

    @Override
    public boolean matches(final Object obj) {
      final String message = (String) obj;
      boolean matches = true;
      for (final String value : values) {
        matches &= message.contains(value);
      }
      return matches;
    }
  }
}
