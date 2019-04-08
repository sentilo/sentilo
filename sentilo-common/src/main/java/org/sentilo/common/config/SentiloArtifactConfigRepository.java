package org.sentilo.common.config;

import java.util.Map;

public interface SentiloArtifactConfigRepository {

  void saveArtifactConfig(final String artifactKey, final Map<String, Object> artifactConfig);

  Map<String, Object> readArtifactConfig(final String artifactKey);

  Map<String, Map<String, Object>> readGlobalConfig();

}
