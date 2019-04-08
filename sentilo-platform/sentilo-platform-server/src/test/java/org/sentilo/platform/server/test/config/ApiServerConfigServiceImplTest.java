package org.sentilo.platform.server.test.config;

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
import org.sentilo.common.config.SentiloArtifactConfigRepository;
import org.sentilo.platform.server.config.ApiServerConfigServiceImpl;

public class ApiServerConfigServiceImplTest {

  private static final String USER_TIMEZONE_PARAM = "user.timezone";
  private static final String FILE_ENCODING_PARAM = "file.encoding";
  private static final String SPRING_PROFILES_ACTIVE_PARAM = "spring.profiles.active";
  private final String processName = "api-server";

  @Mock
  private Properties platformServiceProperties;

  @Mock
  private Properties platformServerProperties;

  @Mock
  private SentiloArtifactConfigRepository repository;

  @InjectMocks
  private ApiServerConfigServiceImpl service;

  @Before
  public void setUp() throws Exception {
    System.setProperty(SPRING_PROFILES_ACTIVE_PARAM, "test");
    MockitoAnnotations.initMocks(this);

  }

  @Test
  public void getName() {
    final String actualAgentName = service.getName();
    Assert.assertEquals(processName.toLowerCase(), actualAgentName);
  }

  @Test
  public void getArtifactConfig() {
    final Map<String, Object> config = service.getArtifactConfig();
    Assert.assertEquals(System.getProperty(USER_TIMEZONE_PARAM, "-"), config.get(USER_TIMEZONE_PARAM));
    Assert.assertEquals(System.getProperty(FILE_ENCODING_PARAM, "-"), config.get(FILE_ENCODING_PARAM));
    Assert.assertEquals("test", config.get(SPRING_PROFILES_ACTIVE_PARAM));
  }

  @Test
  public void save() {
    service.save();
    verify(repository).saveArtifactConfig(anyString(), anyMapOf(String.class, Object.class));
  }

}
