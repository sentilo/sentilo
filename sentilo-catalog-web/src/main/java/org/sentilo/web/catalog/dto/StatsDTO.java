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
package org.sentilo.web.catalog.dto;

public class StatsDTO {

  private String totalDevices;
  private String totalRouterDevices;
  private String totalOtherDevices;

  private String totalEvents;
  private String totalObsEvents;
  private String totalOrderEvents;
  private String totalAlarmEvents;

  private String eventsPerSecond;
  private String dailyAverageRate;
  private String maxRate;

  private String totalActiveAccounts;
  private String totalProviderAccounts;
  private String totalApplicationAccounts;

  public String getTotalDevices() {
    return totalDevices;
  }

  public void setTotalDevices(final String totalDevices) {
    this.totalDevices = totalDevices;
  }

  public String getTotalEvents() {
    return totalEvents;
  }

  public void setTotalEvents(final String totalEvents) {
    this.totalEvents = totalEvents;
  }

  public String getTotalActiveAccounts() {
    return totalActiveAccounts;
  }

  public void setTotalActiveAccounts(final String totalActiveAccounts) {
    this.totalActiveAccounts = totalActiveAccounts;
  }

  public String getEventsPerSecond() {
    return eventsPerSecond;
  }

  public void setEventsPerSecond(final String eventsPerSecond) {
    this.eventsPerSecond = eventsPerSecond;
  }

  public String getTotalRouterDevices() {
    return totalRouterDevices;
  }

  public void setTotalRouterDevices(final String totalRouterDevices) {
    this.totalRouterDevices = totalRouterDevices;
  }

  public String getTotalOtherDevices() {
    return totalOtherDevices;
  }

  public void setTotalOtherDevices(final String totalOtherDevices) {
    this.totalOtherDevices = totalOtherDevices;
  }

  public String getTotalOrderEvents() {
    return totalOrderEvents;
  }

  public void setTotalOrderEvents(final String totalOrderEvents) {
    this.totalOrderEvents = totalOrderEvents;
  }

  public String getTotalAlarmEvents() {
    return totalAlarmEvents;
  }

  public void setTotalAlarmEvents(final String totalAlarmEvents) {
    this.totalAlarmEvents = totalAlarmEvents;
  }

  public String getDailyAverageRate() {
    return dailyAverageRate;
  }

  public void setDailyAverageRate(final String dailyAverageRate) {
    this.dailyAverageRate = dailyAverageRate;
  }

  public String getMaxRate() {
    return maxRate;
  }

  public void setMaxRate(final String maxRate) {
    this.maxRate = maxRate;
  }

  public String getTotalProviderAccounts() {
    return totalProviderAccounts;
  }

  public void setTotalProviderAccounts(final String totalProviderAccounts) {
    this.totalProviderAccounts = totalProviderAccounts;
  }

  public String getTotalApplicationAccounts() {
    return totalApplicationAccounts;
  }

  public void setTotalApplicationAccounts(final String totalApplicationAccounts) {
    this.totalApplicationAccounts = totalApplicationAccounts;
  }

  public String getTotalObsEvents() {
    return totalObsEvents;
  }

  public void setTotalObsEvents(final String totalObsEvents) {
    this.totalObsEvents = totalObsEvents;
  }
}
