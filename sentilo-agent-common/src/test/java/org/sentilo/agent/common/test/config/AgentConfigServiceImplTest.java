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
package org.sentilo.agent.common.test.config;

import static org.mockito.Matchers.anyMapOf;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.verify;

import java.util.Map;
import java.util.Properties;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.agent.common.config.AgentConfigServiceImpl;
import org.sentilo.common.config.SentiloModuleConfigRepository;
import org.sentilo.common.utils.SentiloConstants;

public class AgentConfigServiceImplTest {

  private static final String USER_TIMEZONE_PARAM = "user.timezone";
  private static final String FILE_ENCODING_PARAM = "file.encoding";
  private static final String SPRING_PROFILES_ACTIVE_PARAM = "spring.profiles.active";
  private final String agentName = "MOCK-AGENT";

  @Mock
  private Properties configProperties;

  @Mock
  private SentiloModuleConfigRepository repository;

  @InjectMocks
  private AgentConfigServiceImpl service;

  @Before
  public void setUp() throws Exception {
    System.setProperty(SentiloConstants.SENTILO_AGENT_NAME_ENV, agentName);
    System.setProperty(SPRING_PROFILES_ACTIVE_PARAM, "test");
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void getName() {
    final String actualAgentName = service.getName();
    Assert.assertEquals(agentName.toLowerCase(), actualAgentName);
  }

  @Test
  public void getArtifactConfig() {
    final Map<String, Object> config = service.getConfig();
    Assert.assertEquals(agentName, config.get(SentiloConstants.SENTILO_AGENT_NAME_ENV));
    Assert.assertEquals(System.getProperty(USER_TIMEZONE_PARAM, "-"), config.get(USER_TIMEZONE_PARAM));
    Assert.assertEquals(System.getProperty(FILE_ENCODING_PARAM, "-"), config.get(FILE_ENCODING_PARAM));
    Assert.assertEquals("test", config.get(SPRING_PROFILES_ACTIVE_PARAM));
  }

  @Test
  public void save() {
    service.save();
    verify(repository).saveModuleConfig(anyString(), anyMapOf(String.class, Object.class));
  }

}
