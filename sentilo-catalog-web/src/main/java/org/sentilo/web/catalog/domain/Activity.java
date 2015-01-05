/*
 * Sentilo
 * 
 * Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de Barcelona.
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

import org.codehaus.jackson.annotate.JsonIgnore;
import org.codehaus.jackson.map.annotate.JsonSerialize;
import org.codehaus.jackson.map.annotate.JsonSerialize.Inclusion;
import org.sentilo.web.catalog.domain.Statistics.Events;
import org.springframework.data.annotation.Id;

public class Activity implements CatalogDocument, Comparable<Activity> {

  private static final long serialVersionUID = 1L;

  @JsonIgnore
  @Id
  private int id;
  private long observations;
  @JsonIgnore
  private long totalObservations;
  private long alarms;
  @JsonIgnore
  private long totalAlarms;
  private long orders;
  @JsonIgnore
  private long totalOrders;
  private long timestamp;

  @JsonSerialize(include = Inclusion.NON_EMPTY)
  private String timestampToString;

  public Activity() {
    super();

    timestamp = System.currentTimeMillis();
  }

  public Activity(final Events events, final Activity lastActivity) {
    this();
    totalObservations = events.getObservations();
    totalOrders = events.getOrders();
    totalAlarms = events.getAlarms();

    observations = totalObservations - lastActivity.getTotalObservations();
    alarms = totalAlarms - lastActivity.getTotalAlarms();
    orders = totalOrders - lastActivity.getTotalOrders();

    id = getNextId(lastActivity.id);
  }

  public Activity(final Events events) {
    this(events, new Activity());
  }

  private int getNextId(final int lastId) {
    return lastId + 1;
  }

  public String getTimestampToString() {
    return timestampToString;
  }

  public void setTimestampToString(final String timestampToString) {
    this.timestampToString = timestampToString;
  }

  @Override
  public String getId() {
    return Integer.toString(id);
  }

  @Override
  public void setUpdateAt(final Date date) {
  }

  @Override
  public void setCreatedAt(final Date date) {
  }

  @JsonIgnore
  @Override
  public Date getCreatedAt() {
    return null;
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

  public void setId(final int id) {
    this.id = id;
  }

  @Override
  public int compareTo(final Activity o1) {
    return (new Long(timestamp)).compareTo(new Long(o1.getTimestamp()));
  }

}
