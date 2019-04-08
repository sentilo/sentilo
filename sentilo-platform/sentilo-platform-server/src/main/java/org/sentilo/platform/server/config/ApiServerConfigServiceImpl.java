package org.sentilo.platform.server.config;

import java.util.Map;
import java.util.Properties;

import org.sentilo.common.config.impl.SentiloArtifactConfigServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

@Component
public class ApiServerConfigServiceImpl extends SentiloArtifactConfigServiceImpl {

  @Autowired
  @Qualifier("platformServiceProperties")
  private Properties platformServiceProperties;

  @Autowired
  @Qualifier("platformServerProperties")
  private Properties platformServerProperties;

  @Scheduled(initialDelay = INITIAL_DELAY, fixedDelay = FIXED_DELAY)
  public void save() {
    doSave();
  }

  @Override
  public String getName() {
    return "api-server";
  }

  @Override
  public Map<String, Object> getArtifactConfig() {
    final Properties artifactProperties = new Properties();
    artifactProperties.putAll(platformServerProperties);
    artifactProperties.putAll(platformServiceProperties);
    artifactProperties.put(USER_TIMEZONE_PARAM, System.getProperty(USER_TIMEZONE_PARAM, "-"));
    artifactProperties.put(FILE_ENCODING_PARAM, System.getProperty(FILE_ENCODING_PARAM, "-"));
    artifactProperties.put(SPRING_PROFILES_ACTIVE_PARAM, System.getProperty(SPRING_PROFILES_ACTIVE_PARAM));

    return toMap(artifactProperties);
  }

}
