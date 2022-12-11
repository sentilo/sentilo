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
package org.sentilo.common.domain;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.annotation.JsonProperty;

public class PlatformActivity {

  @JsonInclude(value = Include.NON_NULL)
  private String tenant;
  private long totalRequests;
  private long totalPutRequests;
  private long totalGetRequests;
  private long totalPushRequests;
  private long totalObs;
  private long totalPushObs;
  private long totalPutObs;
  private long totalGetObs;
  private long totalOrders;
  private long totalPushOrders;
  private long totalPutOrders;
  private long totalGetOrders;
  private long totalAlarms;
  private long totalPushAlarms;
  private long totalPutAlarms;
  private long totalGetAlarms;
  private long timestamp;
  private boolean isMaster;

  public PlatformActivity() {
    super();
  }

  public PlatformActivity(final String tenant, final long timestamp, final boolean isMaster) {
    this();
    this.tenant = tenant;
    this.timestamp = timestamp;
    this.isMaster = isMaster;
  }

  @JsonIgnore
  public String getName() {
    return isMaster ? "sentilo" : tenant;
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
    final PlatformActivity other = (PlatformActivity) obj;

    return getName().equals(other.getName()) && getTimestamp() == other.getTimestamp();
  }

  public String getTenant() {
    return tenant;
  }

  public void setTenant(final String tenant) {
    this.tenant = tenant;
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

  public long getTimestamp() {
    return timestamp;
  }

  public void setTimestamp(final long timestamp) {
    this.timestamp = timestamp;
  }

  @JsonProperty("isMaster")
  public boolean isMaster() {
    return isMaster;
  }

  @JsonProperty("isMaster")
  public void setMaster(final boolean isMaster) {
    this.isMaster = isMaster;
  }

  public long getTotalPutRequests() {
    return totalPutRequests;
  }

  public void setTotalPutRequests(final long totalPutRequests) {
    this.totalPutRequests = totalPutRequests;
  }

  public long getTotalGetRequests() {
    return totalGetRequests;
  }

  public void setTotalGetRequests(final long totalGetRequests) {
    this.totalGetRequests = totalGetRequests;
  }

  public long getTotalPushRequests() {
    return totalPushRequests;
  }

  public void setTotalPushRequests(final long totalPushRequests) {
    this.totalPushRequests = totalPushRequests;
  }

  public long getTotalPutObs() {
    return totalPutObs;
  }

  public void setTotalPutObs(final long totalPutObs) {
    this.totalPutObs = totalPutObs;
  }

  public long getTotalGetObs() {
    return totalGetObs;
  }

  public void setTotalGetObs(final long totalGetObs) {
    this.totalGetObs = totalGetObs;
  }

  public long getTotalPutOrders() {
    return totalPutOrders;
  }

  public void setTotalPutOrders(final long totalPutOrders) {
    this.totalPutOrders = totalPutOrders;
  }

  public long getTotalGetOrders() {
    return totalGetOrders;
  }

  public void setTotalGetOrders(final long totalGetOrders) {
    this.totalGetOrders = totalGetOrders;
  }

  public long getTotalPutAlarms() {
    return totalPutAlarms;
  }

  public void setTotalPutAlarms(final long totalPutAlarms) {
    this.totalPutAlarms = totalPutAlarms;
  }

  public long getTotalGetAlarms() {
    return totalGetAlarms;
  }

  public void setTotalGetAlarms(final long totalGetAlarms) {
    this.totalGetAlarms = totalGetAlarms;
  }

  public long getTotalPushObs() {
    return totalPushObs;
  }

  public void setTotalPushObs(final long totalPushObs) {
    this.totalPushObs = totalPushObs;
  }

  public long getTotalPushOrders() {
    return totalPushOrders;
  }

  public void setTotalPushOrders(final long totalPushOrders) {
    this.totalPushOrders = totalPushOrders;
  }

  public long getTotalPushAlarms() {
    return totalPushAlarms;
  }

  public void setTotalPushAlarms(final long totalPushAlarms) {
    this.totalPushAlarms = totalPushAlarms;
  }

}
