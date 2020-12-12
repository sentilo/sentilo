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
package org.sentilo.common.test.config;

import static org.mockito.Matchers.anyMapOf;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.verify;

import java.util.Map;
import java.util.Properties;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.config.SentiloArtifactConfigRepository;
import org.sentilo.common.config.impl.SentiloArtifactConfigServiceImpl;

public class SentiloArtifactConfigServiceTest {

  @Mock
  private SentiloArtifactConfigRepository repository;

  @InjectMocks
  private MockConfigServiceImpl configService = new MockConfigServiceImpl();

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void buildHashKey() {
    final String hashKey = configService.buildUniqueModuleKey();
    Assert.assertTrue(hashKey.startsWith("sentilo:"));
    Assert.assertTrue(hashKey.endsWith(configService.getName() + ":config"));
  }

  @Test
  public void save() {
    final String hashKey = configService.buildUniqueModuleKey();
    configService.save();

    verify(repository).saveArtifactConfig(eq(hashKey), anyMapOf(String.class, Object.class));
  }

  @Test
  public void getConfigValue() {
    Assert.assertEquals("value1", configService.getConfigValue("config.param.1"));
    Assert.assertEquals("defaultValue", configService.getConfigValue("config.param.unkown", "defaultValue"));
    Assert.assertEquals(Boolean.TRUE, configService.getConfigValue("config.param.unkown_boolean", Boolean.class, Boolean.TRUE));
    Assert.assertEquals(Integer.valueOf(25), configService.getConfigValue("config.param.3.points", Integer.class));
  }

  class MockConfigServiceImpl extends SentiloArtifactConfigServiceImpl {

    public void save() {
      doSave();
    }

    @Override
    public String getName() {
      return "mockArtifact";
    }

    @Override
    public Map<String, Object> getArtifactConfig() {
      final Properties mockProps = new Properties();
      mockProps.put("config.param.1", "value1");
      mockProps.put("config.param.2.name", "mockName");
      mockProps.put("config.param.2.password", "mockPassword");
      mockProps.put("config.param.3.points", 25);
      return toMap(mockProps);
    }

  }

}
