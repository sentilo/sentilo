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
package org.sentilo.web.catalog.domain;

import java.util.Collection;
import java.util.Map;

import org.sentilo.common.domain.CatalogAlert;
import org.sentilo.common.domain.CatalogEntity;
import org.sentilo.common.domain.CatalogSensor;
import org.sentilo.common.domain.PlatformInputMessage;
import org.sentilo.common.metrics.SentiloArtifactMetrics;
import org.sentilo.web.catalog.converter.PlatformConverter;
import org.sentilo.web.catalog.utils.CatalogUtils;
import org.springframework.util.CollectionUtils;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

public class PlatformAdminInputMessage implements PlatformInputMessage {

  @JsonInclude(value = Include.NON_EMPTY)
  private Collection<CatalogSensor> sensors;

  @JsonInclude(value = Include.NON_EMPTY)
  private Collection<CatalogEntity> providers;

  @JsonInclude(value = Include.NON_EMPTY)
  private Collection<CatalogEntity> applications;

  @JsonInclude(value = Include.NON_EMPTY)
  private Collection<CatalogAlert> alerts;

  @JsonInclude(value = Include.NON_EMPTY)
  private Map<String, Map<String, Object>> artifactsConfig;

  @JsonInclude(value = Include.NON_EMPTY)
  private Collection<SentiloArtifactMetrics> artifactsMetrics;

  public PlatformAdminInputMessage() {
    super();
  }

  public PlatformAdminInputMessage(final Collection<? extends CatalogDocument> resources) {
    this();
    if (!CollectionUtils.isEmpty(resources)) {
      final Class<? extends CatalogDocument> resourceType = CatalogUtils.getCollectionType(resources);
      if (Sensor.class.isAssignableFrom(resourceType)) {
        setSensors(PlatformConverter.translateSensors(resources));
      } else if (Alert.class.isAssignableFrom(resourceType)) {
        setAlerts(PlatformConverter.translateAlerts(resources));
      } else if (Application.class.isAssignableFrom(resourceType)) {
        setApplications(PlatformConverter.translateEntities(resources));
      } else {
        setProviders(PlatformConverter.translateEntities(resources));
      }
    }
  }

  public Collection<CatalogSensor> getSensors() {
    return sensors;
  }

  public void setSensors(final Collection<CatalogSensor> sensors) {
    this.sensors = sensors;
  }

  public Collection<CatalogEntity> getProviders() {
    return providers;
  }

  public void setProviders(final Collection<CatalogEntity> providers) {
    this.providers = providers;
  }

  public Collection<CatalogEntity> getApplications() {
    return applications;
  }

  public void setApplications(final Collection<CatalogEntity> applications) {
    this.applications = applications;
  }

  public Collection<CatalogAlert> getAlerts() {
    return alerts;
  }

  public void setAlerts(final Collection<CatalogAlert> alerts) {
    this.alerts = alerts;
  }

  public Map<String, Map<String, Object>> getArtifactsConfig() {
    return artifactsConfig;
  }

  public void setArtifactsConfig(final Map<String, Map<String, Object>> artifactsConfig) {
    this.artifactsConfig = artifactsConfig;
  }

  public Collection<SentiloArtifactMetrics> getArtifactsMetrics() {
    return artifactsMetrics;
  }

  public void setArtifactsMetrics(final Collection<SentiloArtifactMetrics> artifactsMetrics) {
    this.artifactsMetrics = artifactsMetrics;
  }
}
