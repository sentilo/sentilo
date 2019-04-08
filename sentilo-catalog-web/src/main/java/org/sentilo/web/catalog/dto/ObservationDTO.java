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

import org.sentilo.platform.client.core.domain.Observation;
import org.sentilo.web.catalog.domain.Sensor;

public class ObservationDTO {

  private String sensor;
  private String sensorType;
  private String sensorState;
  private String sensorSubState;
  private String sensorSubstateDesc;
  private String value;
  private String formattedValue;
  private String unit;
  private String dataType;
  private String timestamp;
  private long time;
  private String provider;
  private boolean found = false;
  private Integer chartNumberObs;

  public ObservationDTO(final Observation observation) {
    if (observation != null) {
      found = true;
      value = observation.getValue();
      timestamp = observation.getTimestamp();
      time = observation.getTime();
      provider = observation.getProvider();
    }
  }

  public ObservationDTO(final Sensor sensor, final Observation observation) {
    this(observation);
    if (sensor != null) {
      this.sensor = sensor.getSensorId();
      sensorType = sensor.getType();
      sensorState = sensor.getState().name();
      sensorSubState = sensor.getSubstate();
      sensorSubstateDesc = sensor.getSubstateDesc();
      unit = sensor.getUnit();
      dataType = sensor.getDataType().toString();
      chartNumberObs = (sensor.getVisualConfiguration() != null) ? sensor.getVisualConfiguration().getChartVisiblePointsNumber() : null;
    }
  }

  public String getSensor() {
    return sensor;
  }

  public void setSensor(final String sensor) {
    this.sensor = sensor;
  }

  public String getSensorType() {
    return sensorType;
  }

  public void setSensorType(final String sensorType) {
    this.sensorType = sensorType;
  }

  public String getSensorState() {
    return sensorState;
  }

  public void setSensorState(final String sensorState) {
    this.sensorState = sensorState;
  }

  public String getSensorSubState() {
    return sensorSubState;
  }

  public void setSensorSubState(final String sensorSubState) {
    this.sensorSubState = sensorSubState;
  }

  public String getSensorSubstateDesc() {
    return sensorSubstateDesc;
  }

  public void setSensorSubstateDesc(final String sensorSubstateDesc) {
    this.sensorSubstateDesc = sensorSubstateDesc;
  }

  public String getValue() {
    return value;
  }

  public void setValue(final String value) {
    this.value = value;
  }

  public String getFormattedValue() {
    return formattedValue;
  }

  public void setFormattedValue(final String formattedValue) {
    this.formattedValue = formattedValue;
  }

  public String getUnit() {
    return unit;
  }

  public void setUnit(final String unit) {
    this.unit = unit;
  }

  public String getDataType() {
    return dataType;
  }

  public void setDataType(final String dataType) {
    this.dataType = dataType;
  }

  public String getTimestamp() {
    return timestamp;
  }

  public void setTimestamp(final String timestamp) {
    this.timestamp = timestamp;
  }

  public long getTime() {
    return time;
  }

  public void setTime(final long time) {
    this.time = time;
  }

  public String getProvider() {
    return provider;
  }

  public void setProvider(final String provider) {
    this.provider = provider;
  }

  public boolean isFound() {
    return found;
  }

  public void setFound(final boolean found) {
    this.found = found;
  }

  public Integer getChartNumberObs() {
    return chartNumberObs;
  }

  public void setChartNumberObs(final Integer chartNumberObs) {
    this.chartNumberObs = chartNumberObs;
  }

}
