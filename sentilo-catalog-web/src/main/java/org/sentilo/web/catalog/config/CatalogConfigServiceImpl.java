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
package org.sentilo.web.catalog.config;

import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import org.sentilo.common.config.impl.SentiloArtifactConfigServiceImpl;
import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.common.utils.SentiloUtils;
import org.sentilo.web.catalog.service.PlatformService;
import org.sentilo.web.catalog.utils.CatalogUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.core.env.Environment;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

@Component
public class CatalogConfigServiceImpl extends SentiloArtifactConfigServiceImpl {

  protected static final int CATALOG_INITIAL_DELAY = 60 * 1000; // 60 seconds.

  @Autowired
  @Qualifier("catalogConfigProperties")
  private Properties configProperties;

  @Autowired
  private Environment environment;

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
    artifactProperties.put(SPRING_PROFILES_ACTIVE_PARAM, CatalogUtils.arrayToString(getActiveProfiles()));
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

  private String[] getActiveProfiles() {
    return SentiloUtils.arrayIsEmpty(environment.getActiveProfiles()) ? environment.getDefaultProfiles() : environment.getActiveProfiles();
  }
}
