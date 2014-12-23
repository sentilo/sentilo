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
package org.sentilo.common.domain;

import org.codehaus.jackson.map.annotate.JsonSerialize;

/**
 * Technical details about sensors and components.
 */
public class TechnicalDetails {

  @JsonSerialize(include = JsonSerialize.Inclusion.NON_NULL)
  private String producer;
  @JsonSerialize(include = JsonSerialize.Inclusion.NON_NULL)
  private String model;
  @JsonSerialize(include = JsonSerialize.Inclusion.NON_NULL)
  private String serialNumber;
  @JsonSerialize(include = JsonSerialize.Inclusion.NON_NULL)
  private String macAddress;
  @JsonSerialize(include = JsonSerialize.Inclusion.NON_NULL)
  private String energy;
  @JsonSerialize(include = JsonSerialize.Inclusion.NON_NULL)
  private String connectivity;

  public TechnicalDetails() {
    super();
  }

  public String getProducer() {
    return producer;
  }

  public void setProducer(final String producer) {
    this.producer = producer;
  }

  public String getModel() {
    return model;
  }

  public void setModel(final String model) {
    this.model = model;
  }

  public String getSerialNumber() {
    return serialNumber;
  }

  public void setSerialNumber(final String serialNumber) {
    this.serialNumber = serialNumber;
  }

  public String getEnergy() {
    return energy;
  }

  public void setEnergy(final String energy) {
    this.energy = energy;
  }

  public String getConnectivity() {
    return connectivity;
  }

  public void setConnectivity(final String connectivity) {
    this.connectivity = connectivity;
  }

  public String getMacAddress() {
    return macAddress;
  }

  public void setMacAddress(final String macAddress) {
    this.macAddress = macAddress;
  }
}
