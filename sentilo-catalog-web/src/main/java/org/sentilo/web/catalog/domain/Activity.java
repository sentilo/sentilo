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

import org.sentilo.common.domain.PlatformActivity;
import org.sentilo.web.catalog.utils.Constants;
import org.springframework.data.annotation.Id;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.util.StringUtils;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

public class Activity implements CatalogDocument, Comparable<Activity> {

  private static final long serialVersionUID = 1L;

  @JsonIgnore
  @Id
  private String id;

  @JsonInclude(value = Include.NON_EMPTY)
  private String tenant;

  private long requests;
  private long getRequests;
  private long putRequests;
  private long observations;
  private long getObservations;
  private long putObservations;
  private long alarms;
  private long getAlarms;
  private long putAlarms;
  private long orders;
  private long getOrders;
  private long putOrders;
  private long timestamp;

  @JsonIgnore
  private long totalRequests;
  @JsonIgnore
  private long totalGetRequests;
  @JsonIgnore
  private long totalPutRequests;
  @JsonIgnore
  private long totalObservations;
  @JsonIgnore
  private long totalGetObservations;
  @JsonIgnore
  private long totalPutObservations;
  @JsonIgnore
  private long totalAlarms;
  @JsonIgnore
  private long totalGetAlarms;
  @JsonIgnore
  private long totalPutAlarms;
  @JsonIgnore
  private long totalOrders;
  @JsonIgnore
  private long totalGetOrders;
  @JsonIgnore
  private long totalPutOrders;

  @JsonInclude(value = Include.NON_EMPTY)
  private String timestampToString;

  @DateTimeFormat(pattern = Constants.DATETIME_FORMAT)
  private Date createdAt;

  private String createdBy;

  @DateTimeFormat(pattern = Constants.DATETIME_FORMAT)
  private Date updatedAt;

  private String updatedBy;

  public Activity() {
    super();
  }

  public Activity(final PlatformActivity platformActivity, final Activity previous) {
    this();
    tenant = platformActivity.getTenant();

    totalAlarms = platformActivity.getTotalAlarms();
    totalGetAlarms = platformActivity.getTotalGetAlarms();
    totalPutAlarms = platformActivity.getTotalPutAlarms();

    totalObservations = platformActivity.getTotalObs();
    totalGetObservations = platformActivity.getTotalGetObs();
    totalPutObservations = platformActivity.getTotalPutObs();

    totalOrders = platformActivity.getTotalOrders();
    totalGetOrders = platformActivity.getTotalGetOrders();
    totalPutOrders = platformActivity.getTotalPutOrders();

    totalRequests = platformActivity.getTotalRequests();
    totalGetRequests = platformActivity.getTotalGetRequests();
    totalPutRequests = platformActivity.getTotalPutRequests();

    timestamp = platformActivity.getTimestamp();

    requests = totalRequests - previous.getTotalRequests();
    getRequests = totalGetRequests - previous.getTotalGetRequests();
    putRequests = totalPutRequests - previous.getTotalPutRequests();
    observations = totalObservations - previous.getTotalObservations();
    getObservations = totalGetObservations - previous.getTotalGetObservations();
    putObservations = totalPutObservations - previous.getTotalPutObservations();
    orders = totalOrders - previous.getTotalOrders();
    getOrders = totalGetOrders - previous.getTotalGetOrders();
    putOrders = totalPutOrders - previous.getTotalPutOrders();
    alarms = totalAlarms - previous.getTotalAlarms();
    getAlarms = totalGetAlarms - previous.getTotalGetAlarms();
    putAlarms = totalPutAlarms - previous.getTotalPutAlarms();

    buildId();
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
    final Activity other = (Activity) obj;
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
    final int prime = 79;
    int result = 1;
    result = prime * result + (id == null ? 0 : id.hashCode());
    return result;
  }

  public String getTimestampToString() {
    return timestampToString;
  }

  public void setTimestampToString(final String timestampToString) {
    this.timestampToString = timestampToString;
  }

  @Override
  public String getId() {
    return id;
  }

  public long getObservations() {
    return observations;
  }

  public void setObservations(final long observations) {
    this.observations = observations;
  }

  public long getTotalObservations() {
    return totalObservations;
  }

  public void setTotalObservations(final long totalObservations) {
    this.totalObservations = totalObservations;
  }

  public long getAlarms() {
    return alarms;
  }

  public void setAlarms(final long alarms) {
    this.alarms = alarms;
  }

  public long getTotalAlarms() {
    return totalAlarms;
  }

  public void setTotalAlarms(final long totalAlarms) {
    this.totalAlarms = totalAlarms;
  }

  public long getOrders() {
    return orders;
  }

  public void setOrders(final long orders) {
    this.orders = orders;
  }

  public long getTotalOrders() {
    return totalOrders;
  }

  public void setTotalOrders(final long totalOrders) {
    this.totalOrders = totalOrders;
  }

  public long getTimestamp() {
    return timestamp;
  }

  public void setTimestamp(final long timestamp) {
    this.timestamp = timestamp;
  }

  public void setId(final String id) {
    this.id = id;
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

  public String getTenant() {
    return tenant;
  }

  public void setTenant(final String tenant) {
    this.tenant = tenant;
  }

  @Override
  public int compareTo(final Activity o1) {
    return new Long(timestamp).compareTo(new Long(o1.getTimestamp()));
  }

  public long getRequests() {
    return requests;
  }

  public void setRequests(final long requests) {
    this.requests = requests;
  }

  public long getTotalRequests() {
    return totalRequests;
  }

  public void setTotalRequests(final long totalRequests) {
    this.totalRequests = totalRequests;
  }

  public long getGetRequests() {
    return getRequests;
  }

  public void setGetRequests(final long getRequests) {
    this.getRequests = getRequests;
  }

  public long getPutRequests() {
    return putRequests;
  }

  public void setPutRequests(final long putRequests) {
    this.putRequests = putRequests;
  }

  public long getGetObservations() {
    return getObservations;
  }

  public void setGetObservations(final long getObservations) {
    this.getObservations = getObservations;
  }

  public long getPutObservations() {
    return putObservations;
  }

  public void setPutObservations(final long putObservations) {
    this.putObservations = putObservations;
  }

  public long getGetAlarms() {
    return getAlarms;
  }

  public void setGetAlarms(final long getAlarms) {
    this.getAlarms = getAlarms;
  }

  public long getPutAlarms() {
    return putAlarms;
  }

  public void setPutAlarms(final long putAlarms) {
    this.putAlarms = putAlarms;
  }

  public long getGetOrders() {
    return getOrders;
  }

  public void setGetOrders(final long getOrders) {
    this.getOrders = getOrders;
  }

  public long getPutOrders() {
    return putOrders;
  }

  public void setPutOrders(final long putOrders) {
    this.putOrders = putOrders;
  }

  public long getTotalGetRequests() {
    return totalGetRequests;
  }

  public void setTotalGetRequests(final long totalGetRequests) {
    this.totalGetRequests = totalGetRequests;
  }

  public long getTotalPutRequests() {
    return totalPutRequests;
  }

  public void setTotalPutRequests(final long totalPutRequests) {
    this.totalPutRequests = totalPutRequests;
  }

  public long getTotalGetObservations() {
    return totalGetObservations;
  }

  public void setTotalGetObservations(final long totalGetObservations) {
    this.totalGetObservations = totalGetObservations;
  }

  public long getTotalPutObservations() {
    return totalPutObservations;
  }

  public void setTotalPutObservations(final long totalPutObservations) {
    this.totalPutObservations = totalPutObservations;
  }

  public long getTotalGetAlarms() {
    return totalGetAlarms;
  }

  public void setTotalGetAlarms(final long totalGetAlarms) {
    this.totalGetAlarms = totalGetAlarms;
  }

  public long getTotalPutAlarms() {
    return totalPutAlarms;
  }

  public void setTotalPutAlarms(final long totalPutAlarms) {
    this.totalPutAlarms = totalPutAlarms;
  }

  public long getTotalGetOrders() {
    return totalGetOrders;
  }

  public void setTotalGetOrders(final long totalGetOrders) {
    this.totalGetOrders = totalGetOrders;
  }

  public long getTotalPutOrders() {
    return totalPutOrders;
  }

  public void setTotalPutOrders(final long totalPutOrders) {
    this.totalPutOrders = totalPutOrders;
  }
}
