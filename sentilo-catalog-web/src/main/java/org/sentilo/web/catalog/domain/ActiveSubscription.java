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

import java.util.Arrays;
import java.util.Date;
import java.util.HashSet;
import java.util.Set;

import org.sentilo.platform.client.core.domain.Subscription;
import org.sentilo.web.catalog.utils.Constants;
import org.sentilo.web.catalog.utils.enums.EntityType;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.format.annotation.DateTimeFormat;

import com.fasterxml.jackson.annotation.JsonIgnore;

@Document
public class ActiveSubscription implements TenantResource, EntityResource, CatalogDocument {

  private static final long serialVersionUID = 1L;

  @Id
  private String id;

  private String entityId;
  private EntityType entityType;
  private String subscriptionType;
  private String provider;
  private String sensor;
  private String alert;
  private String endpoint;
  private long maxRetries;
  private long retryDelay;

  @DateTimeFormat(pattern = Constants.DATETIME_FORMAT)
  private Date createdAt;

  private String createdBy;

  @DateTimeFormat(pattern = Constants.DATETIME_FORMAT)
  private Date updatedAt;

  private String updatedBy;

  private String tenantId;

  private Set<String> tenantsAuth;

  private Set<String> tenantsListVisible;

  public ActiveSubscription() {
    super();
  }

  public ActiveSubscription(final String id) {
    this();
    this.id = id;
  }

  public ActiveSubscription(final TenantResource tenantResource, final EntityType entityType, final Subscription subscription) {
    this();
    entityId = tenantResource.getId();
    tenantId = tenantResource.getTenantId();
    this.entityType = entityType;
    subscriptionType = subscription.getType();
    provider = subscription.getProvider();
    sensor = subscription.getSensor();
    alert = subscription.getAlert();
    endpoint = subscription.getEndpoint();
    maxRetries = subscription.getMaxRetries();
    retryDelay = subscription.getRetryDelay();

    // Set all auth fields as the own resource tenantId, because it only refers to
    // its own tenant and not from others
    setTenantsAuth(new HashSet<String>(Arrays.asList(tenantResource.getTenantId())));
    setTenantsListVisible(new HashSet<String>(Arrays.asList(tenantResource.getTenantId())));
  }

  @Override
  public int hashCode() {
    // Hashcode return must be consistent with the equals method
    final int prime = 43;
    int result = 1;
    result = prime * result + (id == null ? 0 : id.hashCode());
    return result;
  }

  @Override
  public boolean equals(final Object obj) {
    if (this == obj) {
      return true;
    }
    if (obj == null) {
      return false;
    }
    if (getClass() != obj.getClass()) {
      return false;
    }
    final ActiveSubscription other = (ActiveSubscription) obj;
    if (id == null) {
      if (other.id != null) {
        return false;
      }
    } else if (!id.equals(other.id)) {
      return false;
    }
    return true;
  }

  @Override
  public String getId() {
    return id;
  }

  public String getEntityId() {
    return entityId;
  }

  public void setEntityId(final String entityId) {
    this.entityId = entityId;
  }

  public EntityType getEntityType() {
    return entityType;
  }

  public void setEntityType(final EntityType entityType) {
    this.entityType = entityType;
  }

  public String getSubscriptionType() {
    return subscriptionType;
  }

  public void setSubscriptionType(final String subscriptionType) {
    this.subscriptionType = subscriptionType;
  }

  public String getProvider() {
    return provider;
  }

  public void setProvider(final String provider) {
    this.provider = provider;
  }

  public String getSensor() {
    return sensor;
  }

  public void setSensor(final String sensor) {
    this.sensor = sensor;
  }

  public String getAlert() {
    return alert;
  }

  public void setAlert(final String alert) {
    this.alert = alert;
  }

  public String getEndpoint() {
    return endpoint;
  }

  public void setEndpoint(final String endpoint) {
    this.endpoint = endpoint;
  }

  public long getMaxRetries() {
    return maxRetries;
  }

  public void setMaxRetries(final long maxRetries) {
    this.maxRetries = maxRetries;
  }

  public long getRetryDelay() {
    return retryDelay;
  }

  public void setRetryDelay(final long retryDelay) {
    this.retryDelay = retryDelay;
  }

  @Override
  @JsonIgnore
  public String getCreatedBy() {
    return createdBy;
  }

  @Override
  public void setCreatedBy(final String createdBy) {
    this.createdBy = createdBy;
  }

  @Override
  @JsonIgnore
  public Date getUpdatedAt() {
    return updatedAt;
  }

  @Override
  public void setUpdatedAt(final Date updatedAt) {
    this.updatedAt = updatedAt;
  }

  @Override
  @JsonIgnore
  public String getUpdatedBy() {
    return updatedBy;
  }

  @Override
  public void setUpdatedBy(final String updatedBy) {
    this.updatedBy = updatedBy;
  }

  @Override
  public void setCreatedAt(final Date createdAt) {
    this.createdAt = createdAt;
  }

  @Override
  @JsonIgnore
  public Date getCreatedAt() {
    return createdAt;
  }

  @Override
  public String getTenantId() {
    return tenantId;
  }

  @Override
  public void setTenantId(final String tenantId) {
    this.tenantId = tenantId;
  }

  @Override
  public Set<String> getTenantsAuth() {
    return tenantsAuth;
  }

  @Override
  public void setTenantsAuth(final Set<String> tenantsAuth) {
    this.tenantsAuth = tenantsAuth;
  }

  @Override
  public Set<String> getTenantsListVisible() {
    return tenantsListVisible;
  }

  @Override
  public void setTenantsListVisible(final Set<String> tenantsListVisible) {
    this.tenantsListVisible = tenantsListVisible;
  }

  @Override
  public String getEntityOwner() {
    return entityId;
  }

  @Override
  public String toString() {
    return "\n--- ActiveSubscription ---" + "\n\tentityId: " + entityId + "\n\ttenantId: " + tenantId + "\n\tentityType: " + entityType
        + "\n\tsubscriptionType: " + getSubscriptionType() + "\n\tprovider: " + provider + "\n\tsensor: " + sensor + "\n\talert: " + alert
        + "\n\tendpoint: " + endpoint + "\n\tmaxRetries: " + maxRetries + "\n\tretryDelay: " + retryDelay + "\n";
  }

}
