package org.sentilo.common.config;

import java.util.Map;

public interface SentiloArtifactConfigService {

  void save();

  String getName();

  Map<String, Object> getArtifactConfig();

  String getConfigValue(String configKey);

  String getConfigValue(String configKey, String defaultValue);

  <T> T getConfigValue(String configKey, final Class<T> expectedType);

  <T> T getConfigValue(String configKey, final Class<T> expectedType, final T defaultValue);
}
