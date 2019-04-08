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

import java.util.List;

import org.sentilo.web.catalog.domain.Component;

public class InfoBoxDTO {

  private String componentId;
  private String componentType;
  private String componentName;
  private String componentDesc;
  private List<SensorDTO> sensors;
  private List<ObservationDTO> sensorLastObservations;
  private String lastUpdateTimeMessage;

  public InfoBoxDTO() {
    super();
  }

  public InfoBoxDTO(final Component component, final List<SensorDTO> sensors, final List<ObservationDTO> sensorLastObservations,
      final String lastUpdateTimeMessage) {
    componentId = component.getId();
    componentName = component.getName();
    componentDesc = component.getDescription();
    componentType = component.getComponentType();
    this.sensors = sensors;
    this.sensorLastObservations = sensorLastObservations;
    this.lastUpdateTimeMessage = lastUpdateTimeMessage;
  }

  public String getComponentId() {
    return componentId;
  }

  public void setComponentId(final String componentId) {
    this.componentId = componentId;
  }

  public String getComponentType() {
    return componentType;
  }

  public void setComponentType(final String componentType) {
    this.componentType = componentType;
  }

  public String getComponentName() {
    return componentName;
  }

  public void setComponentName(final String componentName) {
    this.componentName = componentName;
  }

  public String getComponentDesc() {
    return componentDesc;
  }

  public void setComponentDesc(final String componentDesc) {
    this.componentDesc = componentDesc;
  }

  public List<SensorDTO> getSensors() {
    return sensors;
  }

  public void setSensors(final List<SensorDTO> sensors) {
    this.sensors = sensors;
  }

  public List<ObservationDTO> getSensorLastObservations() {
    return sensorLastObservations;
  }

  public void setSensorLastObservations(final List<ObservationDTO> sensorLastObservations) {
    this.sensorLastObservations = sensorLastObservations;
  }

  public String getLastUpdateTimeMessage() {
    return lastUpdateTimeMessage;
  }

  public void setLastUpdateTimeMessage(final String lastUpdateTimeMessage) {
    this.lastUpdateTimeMessage = lastUpdateTimeMessage;
  }

}
