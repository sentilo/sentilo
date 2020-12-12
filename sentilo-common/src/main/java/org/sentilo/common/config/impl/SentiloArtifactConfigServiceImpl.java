/*
 * Sentilo
 * 
 * Original version 1.4 Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de
 * Barcelona. Modified by Opentrends adding support for multitenant deployments and SaaS.
 * Modifications on version 1.5 Copyright (C) 2015 Opentrends Solucions i Sistemes, S.L.
 *
 * 
 * This program is licensed and may be used, modified and redistributed under the terms of the
 * European Public License (EUPL), either version 1.1 or (at your option) any later version as soon
 * as they are approved by the European Commission.
 * 
 * Alternatively, you may redistribute and/or modify this program under the terms of the GNU Lesser
 * General Public License as published by the Free Software Foundation; either version 3 of the
 * License, or (at your option) any later version.
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied.
 * 
 * See the licenses for the specific language governing permissions, limitations and more details.
 * 
 * You should have received a copy of the EUPL1.1 and the LGPLv3 licenses along with this program;
 * if not, you may find them at:
 * 
 * https://joinup.ec.europa.eu/software/page/eupl/licence-eupl http://www.gnu.org/licenses/ and
 * https://www.gnu.org/licenses/lgpl.txt
 */
package org.sentilo.common.config.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import org.sentilo.common.config.SentiloArtifactConfigRepository;
import org.sentilo.common.config.SentiloArtifactConfigService;
import org.sentilo.common.utils.ArtifactUtils;
import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.common.utils.SentiloUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;

public abstract class SentiloArtifactConfigServiceImpl implements SentiloArtifactConfigService {

  protected static final Logger LOGGER = LoggerFactory.getLogger(SentiloArtifactConfigServiceImpl.class);

  protected static final String SUFFIX_KEY = "config";
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

  public String buildUniqueModuleKey() {
    return ArtifactUtils.buildUniqueArtifactKey(getName(), SUFFIX_KEY);
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
