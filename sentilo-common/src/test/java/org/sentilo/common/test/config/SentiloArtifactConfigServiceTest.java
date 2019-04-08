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
