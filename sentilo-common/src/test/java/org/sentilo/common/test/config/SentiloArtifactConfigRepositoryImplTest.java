package org.sentilo.common.test.config;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.config.impl.SentiloArtifactConfigRepositoryImpl;
import org.sentilo.common.config.impl.SentiloArtifactConfigServiceImpl;
import org.sentilo.common.utils.SentiloConstants;
import org.springframework.data.redis.core.HashOperations;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.SetOperations;

public class SentiloArtifactConfigRepositoryImplTest {

  @Mock
  private RedisTemplate<String, String> redisTemplate;

  @Mock
  private HashOperations<String, Object, Object> hashOps;

  @Mock
  private SetOperations<String, String> setOps;

  @InjectMocks
  private SentiloArtifactConfigRepositoryImpl repository;

  private MockConfigServiceImpl configService = new MockConfigServiceImpl();

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void saveArtifactConfig() {
    final String artifactKey = "mockArtifactKey";
    final String sensitiveParamKey = SentiloConstants.CONFIG_SENSITIVE_KEY_PREFIX + "config.param.2.password";
    final String sensitiveParamValue = "mockPassword";
    final Map<String, Object> artifactConfig = configService.getArtifactConfig();
    when(redisTemplate.opsForHash()).thenReturn(hashOps);
    when(redisTemplate.opsForSet()).thenReturn(setOps);

    repository.saveArtifactConfig(artifactKey, artifactConfig);

    Assert.assertTrue(artifactConfig.containsKey(sensitiveParamKey));
    Assert.assertEquals(sensitiveParamValue, artifactConfig.get(sensitiveParamKey));
    verify(hashOps).putAll(artifactKey, artifactConfig);
    verify(setOps).add(SentiloConstants.GLOBAL_CONFIG_LIST_KEY, artifactKey);
  }

  @Test
  public void readGlobalConfig() {
    final String[] configArtifactKeys = {configService.buildUniqueModuleKey()};
    final Set<String> componentsConfigRegistry = new HashSet<String>(Arrays.asList(configArtifactKeys));
    final String sensitiveParamKey = "config.param.2.password";
    final String noSensitiveParamKey = "config.param.2.name";
    when(redisTemplate.opsForHash()).thenReturn(hashOps);
    when(redisTemplate.opsForSet()).thenReturn(setOps);
    when(setOps.members(SentiloConstants.GLOBAL_CONFIG_LIST_KEY)).thenReturn(componentsConfigRegistry);
    when(hashOps.entries(configService.buildUniqueModuleKey())).thenReturn(new HashMap<Object, Object>(configService.getArtifactConfig()));

    final Map<String, Map<String, Object>> globalConfig = repository.readGlobalConfig();

    Assert.assertTrue(globalConfig.size() == 1);
    Assert.assertTrue(globalConfig.containsKey(configService.buildUniqueModuleKey()));
    Assert.assertTrue(globalConfig.get(configService.buildUniqueModuleKey()).size() == configService.getArtifactConfig().size());
    Assert.assertTrue(globalConfig.get(configService.buildUniqueModuleKey()).containsKey(sensitiveParamKey));
    Assert.assertTrue(globalConfig.get(configService.buildUniqueModuleKey()).containsKey(noSensitiveParamKey));
    Assert.assertEquals(SentiloConstants.CONFIG_SENSITIVE_VALUE_MASK, globalConfig.get(configService.buildUniqueModuleKey()).get(sensitiveParamKey));
    Assert.assertEquals("mockName", globalConfig.get(configService.buildUniqueModuleKey()).get(noSensitiveParamKey));
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
      return toMap(mockProps);
    }

  }

}
