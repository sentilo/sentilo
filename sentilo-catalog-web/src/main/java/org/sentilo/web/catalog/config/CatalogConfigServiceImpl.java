package org.sentilo.web.catalog.config;

import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import org.sentilo.common.config.impl.SentiloArtifactConfigServiceImpl;
import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.web.catalog.service.PlatformService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

@Component
public class CatalogConfigServiceImpl extends SentiloArtifactConfigServiceImpl {

  protected static final int CATALOG_INITIAL_DELAY = 60 * 1000; // 60 seconds.

  @Autowired
  @Qualifier("catalogConfigProperties")
  private Properties configProperties;

  @Autowired
  private PlatformService platformService;

  @Scheduled(initialDelay = CATALOG_INITIAL_DELAY, fixedDelay = FIXED_DELAY)
  public void save() {
    doSave();
  }

  @Override
  public String getName() {
    return "catalog-web";
  }

  @Override
  public Map<String, Object> getArtifactConfig() {
    final Properties artifactProperties = new Properties();
    artifactProperties.putAll(configProperties);
    artifactProperties.put(USER_TIMEZONE_PARAM, System.getProperty(USER_TIMEZONE_PARAM, "-"));
    artifactProperties.put(FILE_ENCODING_PARAM, System.getProperty(FILE_ENCODING_PARAM, "-"));
    artifactProperties.put(SPRING_PROFILES_ACTIVE_PARAM, System.getProperty(SPRING_PROFILES_ACTIVE_PARAM));
    artifactProperties.put(SentiloConstants.SENTILO_STATE_PAGE_ENABLED_PROP_KEY,
        System.getProperty(SentiloConstants.SENTILO_STATE_PAGE_ENABLED_PROP_KEY, "false"));
    artifactProperties.put(SentiloConstants.SENTILO_MULTITENANT_PROP_KEY, System.getProperty(SentiloConstants.SENTILO_MULTITENANT_PROP_KEY, "false"));
    artifactProperties.put(SentiloConstants.SENTILO_MULTITENANT_INFER_PROP_KEY,
        System.getProperty(SentiloConstants.SENTILO_MULTITENANT_INFER_PROP_KEY, "false"));
    artifactProperties.put(SentiloConstants.SENTILO_FEDERATION_ENABLED_PROP_KEY,
        System.getProperty(SentiloConstants.SENTILO_FEDERATION_ENABLED_PROP_KEY, "false"));

    return toMap(artifactProperties);
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.common.config.SentiloArtifactConfigContext#saveModuleConfig(java.lang.String)
   */
  protected void saveArtifactConfig(final String moduleKey) {
    final Map<String, Map<String, Object>> catalogConfig = new HashMap<String, Map<String, Object>>();
    catalogConfig.put(moduleKey, getArtifactConfig());

    platformService.saveCatalogConfig(catalogConfig);
  }

}
