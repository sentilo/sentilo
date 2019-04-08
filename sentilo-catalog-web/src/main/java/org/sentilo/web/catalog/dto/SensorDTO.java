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

public class SensorDTO {

  private String sensor;
  private String sensorType;
  private String sensorState;
  private String sensorSubState;
  private String sensorSubstateDesc;
  private String unit;
  private String dataType;
  private String provider;

  public SensorDTO() {

  }

  public SensorDTO(final String sensor, final String sensorType, final String sensorState, final String sensorSubState,
      final String sensorSubstateDesc, final String unit, final String dataType, final String provider) {
    super();
    this.sensor = sensor;
    this.sensorType = sensorType;
    this.sensorState = sensorState;
    this.sensorSubState = sensorSubState;
    this.sensorSubstateDesc = sensorSubstateDesc;
    this.unit = unit;
    this.dataType = dataType;
    this.provider = provider;
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

  public String getProvider() {
    return provider;
  }

  public void setProvider(final String provider) {
    this.provider = provider;
  }

}
