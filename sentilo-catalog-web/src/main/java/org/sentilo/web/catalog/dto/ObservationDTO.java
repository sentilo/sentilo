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
package org.sentilo.web.catalog.dto;

import org.sentilo.platform.client.core.domain.Observation;
import org.sentilo.web.catalog.domain.Sensor;

public class ObservationDTO {

  private String sensor;
  private String sensorType;
  private String value;
  private String unit;
  private String dataType;
  private String timestamp;
  private boolean found = false;

  public ObservationDTO(final Observation observation) {
    if (observation != null) {
      found = true;
      value = observation.getValue();
      timestamp = observation.getTimestamp();
    }
  }

  public ObservationDTO(final Sensor sensor, final Observation observation) {
    this(observation);
    if (sensor != null) {
      this.sensor = sensor.getSensorId();
      unit = sensor.getUnit();
      sensorType = sensor.getType();
      dataType = sensor.getDataType().toString();
    }
  }

  public String getSensor() {
    return sensor;
  }

  public String getValue() {
    return value;
  }

  public String getTimestamp() {
    return timestamp;
  }

  public boolean isFound() {
    return found;
  }

  public String getUnit() {
    return unit;
  }

  public String getSensorType() {
    return sensorType;
  }

  public String getDataType() {
    return dataType;
  }
}
