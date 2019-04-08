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
import org.sentilo.agent.common.utils.Constants;
import org.sentilo.common.config.SentiloArtifactConfigRepository;

public class AgentConfigServiceImplTest {

  private static final String USER_TIMEZONE_PARAM = "user.timezone";
  private static final String FILE_ENCODING_PARAM = "file.encoding";
  private static final String SPRING_PROFILES_ACTIVE_PARAM = "spring.profiles.active";
  private final String agentName = "MOCK-AGENT";

  @Mock
  private Properties configProperties;

  @Mock
  private SentiloArtifactConfigRepository repository;

  @InjectMocks
  private AgentConfigServiceImpl service;

  @Before
  public void setUp() throws Exception {
    System.setProperty(Constants.SENTILO_AGENT_NAME_ENV, agentName);
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
    final Map<String, Object> config = service.getArtifactConfig();
    Assert.assertEquals(agentName, config.get(Constants.SENTILO_AGENT_NAME_ENV));
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
