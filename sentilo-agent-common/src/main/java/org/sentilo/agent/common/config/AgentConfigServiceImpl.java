package org.sentilo.agent.common.config;

import java.util.Map;
import java.util.Properties;

import org.sentilo.agent.common.utils.Constants;
import org.sentilo.common.config.impl.SentiloArtifactConfigServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

@Component
public class AgentConfigServiceImpl extends SentiloArtifactConfigServiceImpl {

  @Autowired
  @Qualifier("agentConfigProperties")
  private Properties configProperties;

  @Scheduled(initialDelay = INITIAL_DELAY, fixedDelay = FIXED_DELAY)
  public void save() {
    doSave();
  }

  @Override
  public String getName() {
    final String agentName = System.getProperty(Constants.SENTILO_AGENT_NAME_ENV);
    return agentName.toLowerCase();
  }

  @Override
  public Map<String, Object> getArtifactConfig() {
    final Properties artifactProperties = new Properties();
    artifactProperties.putAll(configProperties);
    artifactProperties.put(USER_TIMEZONE_PARAM, System.getProperty(USER_TIMEZONE_PARAM, "-"));
    artifactProperties.put(FILE_ENCODING_PARAM, System.getProperty(FILE_ENCODING_PARAM, "-"));
    artifactProperties.put(Constants.SENTILO_AGENT_NAME_ENV, System.getProperty(Constants.SENTILO_AGENT_NAME_ENV));
    artifactProperties.put(SPRING_PROFILES_ACTIVE_PARAM, System.getProperty(SPRING_PROFILES_ACTIVE_PARAM));

    return toMap(artifactProperties);
  }

}
