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

import java.util.Date;

import org.sentilo.common.domain.PlatformPerformance;
import org.sentilo.web.catalog.utils.Constants;
import org.springframework.data.annotation.Id;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.util.StringUtils;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

public class Performance implements CatalogDocument, Comparable<Performance> {

  private static final long serialVersionUID = 1L;

  @JsonIgnore
  @Id
  private String id;

  @JsonInclude(value = Include.NON_EMPTY)
  private String tenant;

  private float instantAvg;
  private float maxDailyAvg;
  private float maxAvg;
  private long timestamp;
  private long totalRequests;
  private long totalObs;
  private long totalOrders;
  private long totalAlarms;

  @DateTimeFormat(pattern = Constants.DATETIME_FORMAT)
  private Date createdAt;

  private String createdBy;

  @DateTimeFormat(pattern = Constants.DATETIME_FORMAT)
  private Date updatedAt;

  private String updatedBy;

  public Performance() {
    super();
  }

  public Performance(final PlatformPerformance platformPerformance) {
    this();
    tenant = platformPerformance.getTenant();
    instantAvg = platformPerformance.getInstantAvg();
    maxDailyAvg = platformPerformance.getMaxDailyAvg();
    maxAvg = platformPerformance.getMaxAvg();
    timestamp = platformPerformance.getTimestamp();

    totalRequests = platformPerformance.getTotalRequests();
    totalObs = platformPerformance.getTotalObs();
    totalOrders = platformPerformance.getTotalOrders();
    totalAlarms = platformPerformance.getTotalAlarms();

    buildId();
  }

  @Override
  public String getId() {
    return id;
  }

  public void setId(final String id) {
    this.id = id;
  }

  public void buildId() {
    String aux = Long.toString(timestamp);
    if (StringUtils.hasText(tenant)) {
      aux += tenant;
    }

    id = aux;
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
    final Performance other = (Performance) obj;
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
  public int hashCode() {
    // Hashcode return must be consistent with the equals method
    final int prime = 73;
    int result = 1;
    result = prime * result + (id == null ? 0 : id.hashCode());
    return result;
  }

  @Override
  public int compareTo(final Performance o) {
    return id.compareTo(o.getId());
  }

  public String getTenant() {
    return tenant;
  }

  public void setTenant(final String tenant) {
    this.tenant = tenant;
  }

  public float getInstantAvg() {
    return instantAvg;
  }

  public void setInstantAvg(final float instantAvg) {
    this.instantAvg = instantAvg;
  }

  public float getMaxDailyAvg() {
    return maxDailyAvg;
  }

  public void setMaxDailyAvg(final float maxDailyAvg) {
    this.maxDailyAvg = maxDailyAvg;
  }

  public float getMaxAvg() {
    return maxAvg;
  }

  public void setMaxAvg(final float maxAvg) {
    this.maxAvg = maxAvg;
  }

  public long getTimestamp() {
    return timestamp;
  }

  public void setTimestamp(final long timestamp) {
    this.timestamp = timestamp;
  }

  public long getTotalRequests() {
    return totalRequests;
  }

  public void setTotalRequests(final long totalRequests) {
    this.totalRequests = totalRequests;
  }

  public long getTotalObs() {
    return totalObs;
  }

  public void setTotalObs(final long totalObs) {
    this.totalObs = totalObs;
  }

  public long getTotalOrders() {
    return totalOrders;
  }

  public void setTotalOrders(final long totalOrders) {
    this.totalOrders = totalOrders;
  }

  public long getTotalAlarms() {
    return totalAlarms;
  }

  public void setTotalAlarms(final long totalAlarms) {
    this.totalAlarms = totalAlarms;
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

  @JsonIgnore
  @Override
  public Date getCreatedAt() {
    return createdAt;
  }
}
