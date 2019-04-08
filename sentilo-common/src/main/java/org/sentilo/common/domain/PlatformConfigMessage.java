package org.sentilo.common.domain;

import java.util.HashMap;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

public class PlatformConfigMessage {

  @JsonInclude(value = Include.NON_EMPTY)
  private Map<String, Map<String, Object>> artifactsConfig;

  public void addArtifactConfig(final String artifactKey, final Map<String, Object> artifactConfig) {
    if (artifactsConfig == null) {
      artifactsConfig = new HashMap<String, Map<String, Object>>();
    }

    artifactsConfig.put(artifactKey, artifactConfig);
  }

  public Map<String, Map<String, Object>> getArtifactsConfig() {
    return artifactsConfig == null ? new HashMap<String, Map<String, Object>>() : artifactsConfig;
  }

  public void setGlobalConfig(final Map<String, Map<String, Object>> artifactsConfig) {
    this.artifactsConfig = artifactsConfig;
  }

}
