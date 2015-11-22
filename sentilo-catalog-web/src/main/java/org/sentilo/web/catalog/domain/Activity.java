/*
 * Sentilo
 *  
 * Original version 1.4 Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de Barcelona.
 * Modified by Opentrends adding support for multitenant deployments and SaaS. Modifications on version 1.5 Copyright (C) 2015 Opentrends Solucions i Sistemes, S.L.
 * 
 *   
 * This program is licensed and may be used, modified and redistributed under the
 * terms  of the European Public License (EUPL), either version 1.1 or (at your 
 * option) any later version as soon as they are approved by the European 
 * Commission.
 *   
 * Alternatively, you may redistribute and/or modify this program under the terms
 * of the GNU Lesser General Public License as published by the Free Software 
 * Foundation; either  version 3 of the License, or (at your option) any later 
 * version. 
 *   
 * Unless required by applicable law or agreed to in writing, software distributed
 * under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR 
 * CONDITIONS OF ANY KIND, either express or implied. 
 *   
 * See the licenses for the specific language governing permissions, limitations 
 * and more details.
 *   
 * You should have received a copy of the EUPL1.1 and the LGPLv3 licenses along 
 * with this program; if not, you may find them at: 
 *   
 *   https://joinup.ec.europa.eu/software/page/eupl/licence-eupl
 *   http://www.gnu.org/licenses/ 
 *   and 
 *   https://www.gnu.org/licenses/lgpl.txt
 */
package org.sentilo.web.catalog.domain;

import java.util.Date;

import org.codehaus.jackson.annotate.JsonIgnore;
import org.codehaus.jackson.map.annotate.JsonSerialize;
import org.codehaus.jackson.map.annotate.JsonSerialize.Inclusion;
import org.sentilo.common.domain.PlatformActivity;
import org.sentilo.web.catalog.utils.Constants;
import org.springframework.data.annotation.Id;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.util.StringUtils;

public class Activity implements CatalogDocument, Comparable<Activity> {

  private static final long serialVersionUID = 1L;

  @JsonIgnore
  @Id
  private String id;

  @JsonSerialize(include = Inclusion.NON_EMPTY)
  private String tenant;

  private long requests;
  private long observations;
  private long alarms;
  private long orders;
  private long timestamp;

  @JsonIgnore
  private long totalRequests;
  @JsonIgnore
  private long totalObservations;
  @JsonIgnore
  private long totalAlarms;
  @JsonIgnore
  private long totalOrders;

  @JsonSerialize(include = Inclusion.NON_EMPTY)
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
    totalObservations = platformActivity.getTotalObs();
    totalOrders = platformActivity.getTotalOrders();
    totalRequests = platformActivity.getTotalRequests();
    timestamp = platformActivity.getTimestamp();

    requests = totalRequests - previous.getTotalRequests();
    observations = totalObservations - previous.getTotalObservations();
    orders = totalOrders - previous.getTotalOrders();
    alarms = totalAlarms - previous.getTotalAlarms();

    buildId();
  }

  public void buildId() {
    String aux = Long.toString(timestamp);
    if (StringUtils.hasText(tenant)) {
      aux += tenant;
    }

    id = aux;
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

  @JsonIgnore
  public String getCreatedBy() {
    return createdBy;
  }

  public void setCreatedBy(final String createdBy) {
    this.createdBy = createdBy;
  }

  @JsonIgnore
  public Date getUpdatedAt() {
    return updatedAt;
  }

  public void setUpdatedAt(final Date updatedAt) {
    this.updatedAt = updatedAt;
  }

  @JsonIgnore
  public String getUpdatedBy() {
    return updatedBy;
  }

  public void setUpdatedBy(final String updatedBy) {
    this.updatedBy = updatedBy;
  }

  public void setCreatedAt(final Date createdAt) {
    this.createdAt = createdAt;
  }

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
    return (new Long(timestamp)).compareTo(new Long(o1.getTimestamp()));
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

}
