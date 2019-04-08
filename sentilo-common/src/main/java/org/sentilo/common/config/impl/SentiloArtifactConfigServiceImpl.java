package org.sentilo.common.config.impl;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import org.sentilo.common.config.SentiloArtifactConfigRepository;
import org.sentilo.common.config.SentiloArtifactConfigService;
import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.common.utils.SentiloUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;

public abstract class SentiloArtifactConfigServiceImpl implements SentiloArtifactConfigService {

  protected static final Logger LOGGER = LoggerFactory.getLogger(SentiloArtifactConfigServiceImpl.class);

  protected static final int INITIAL_DELAY = 5 * 1000; // 5 seconds
  protected static final int FIXED_DELAY = 30 * 60 * 1000; // 30 minutes

  protected static final String USER_TIMEZONE_PARAM = "user.timezone";
  protected static final String FILE_ENCODING_PARAM = "file.encoding";
  protected static final String SPRING_PROFILES_ACTIVE_PARAM = "spring.profiles.active";
  /** password,credentials,identity.key,host,port are default sensitive words */
  protected static final String[] DEFAULT_KEY_SENTITIVE_WORDS = {"password", "credentials", "identity.key", "host", "port"};

  @Value("${api.config.key.sensitive.words:#{null}}")
  private String[] keySensitiveWords;

  @Autowired
  private SentiloArtifactConfigRepository repository;

  /**
   * Return an unique hash key which identifies this component. For example,
   * <code>sentilo.AWS-1234.agent.relational</code>
   *
   * @return component config hash key
   */
  public String buildUniqueModuleKey() {
    // To differentiate instances of the same artifact, each hash key will be compounded by the
    // artifact name plus hostname
    String hostname = "UNKNOWN";
    try {
      hostname = InetAddress.getLocalHost().getHostName();
    } catch (final UnknownHostException uhe) {
      LOGGER.warn("Local hostname could not be resolved. UNKNOWN word will be used");
    }

    return String.format("sentilo:%s:%s:config", hostname, getName());
  }

  public String getConfigValue(final String configKey) {
    // Default config parameters values's type is equal to String
    return getConfigValue(configKey, String.class);
  }

  public String getConfigValue(final String configKey, final String defaultValue) {
    // Default config parameters values's type is equal to String
    return getConfigValue(configKey, String.class, defaultValue);
  }

  public <T> T getConfigValue(final String configKey, final Class<T> expectedType) {
    return getConfigValue(configKey, expectedType, null);
  }

  public <T> T getConfigValue(final String configKey, final Class<T> expectedType, final T defaultValue) {
    Object value = getArtifactConfig().get(configKey);
    if (value == null) {
      final String sensitiveKey = SentiloConstants.CONFIG_SENSITIVE_KEY_PREFIX + configKey;
      value = getArtifactConfig().get(sensitiveKey);
    }

    return castValue(value, expectedType, defaultValue);
  }

  private <T> T castValue(final Object value, final Class<T> expectedType, final T defaultValue) {
    T returnValue;
    try {
      returnValue = value != null ? expectedType.cast(value) : defaultValue;
    } catch (final ClassCastException cce) {
      returnValue = defaultValue;
    }

    return returnValue;
  }

  protected void doSave() {
    if (!getArtifactConfig().isEmpty()) {
      final String moduleKey = buildUniqueModuleKey();
      saveArtifactConfig(moduleKey);
    }
  }

  protected List<String> getSensitiveParamKeys() {
    return new ArrayList<>();
  }

  protected void saveArtifactConfig(final String moduleKey) {
    repository.saveArtifactConfig(moduleKey, getArtifactConfig());
  }

  protected Map<String, Object> toMap(final Properties propsConfig) {
    final Map<String, Object> mapConfig = new HashMap<String, Object>();
    final String[] wordsToFilter = SentiloUtils.arrayIsEmpty(keySensitiveWords) ? DEFAULT_KEY_SENTITIVE_WORDS : keySensitiveWords;

    propsConfig.forEach((k, v) -> {
      final String k1 = (String) k;
      boolean sensitiveDataFound = getSensitiveParamKeys().contains(k1);

      for (int i = 0; i < wordsToFilter.length && !sensitiveDataFound; i++) {
        sensitiveDataFound = k1.contains(wordsToFilter[i].trim());
      }

      if (sensitiveDataFound) {
        mapConfig.put(SentiloConstants.CONFIG_SENSITIVE_KEY_PREFIX + k1, v);
      } else {
        mapConfig.put(k1, v);
      }
    });

    return mapConfig;
  }

}
