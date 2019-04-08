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

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

public class SensorLocationElement implements CatalogElement, Comparable<SensorLocationElement> {

  @JsonInclude(value = Include.NON_NULL)
  private String sensor;
  @JsonInclude(value = Include.NON_NULL)
  private String provider;
  @JsonInclude(value = Include.NON_NULL)
  private String location;
  @JsonInclude(value = Include.NON_NULL)
  private Long fromTsTime;

  public SensorLocationElement() {
    super();
  }

  @Override
  public boolean equals(final Object obj) {
    // Two instances of SentiloLocationElement are equals if, and only if, sensor, provider,
    // and fromTsTime attributes are equals (locations must be equals if fromTsTimes are equals
    // because a sensor, at any given instant, can only be in one location)

    if (this == obj) {
      return true;
    }
    if (obj == null) {
      return false;
    }
    if (getClass() != obj.getClass()) {
      return false;
    }
    final SensorLocationElement other = (SensorLocationElement) obj;

    return areEquals(sensor, other.sensor) && areEquals(provider, other.provider) && areEquals(fromTsTime, other.fromTsTime);
  }

  @Override
  public int hashCode() {
    // Hashcode return must be consistent with the equals method
    final int prime = 97;
    int result = 1;
    result = prime * result + valueHashCode(sensor) + valueHashCode(provider) + valueHashCode(fromTsTime);
    return result;
  }

  @Override
  public int compareTo(final SensorLocationElement other) {
    return fromTsTime.compareTo(other.fromTsTime);
  }

  @Override
  public String toString() {
    return "SensorLocationElement [sensor=" + sensor + ", provider=" + provider + ", location=" + location + ", fromTsTime=" + fromTsTime + "]";
  }

  private boolean areEquals(final Object expected, final Object actual) {
    return expected == null ? actual == null : expected.equals(actual);
  }

  private int valueHashCode(final Object value) {
    return value == null ? 0 : value.hashCode();
  }

  public String getSensor() {
    return sensor;
  }

  public void setSensor(final String sensor) {
    this.sensor = sensor;
  }

  public String getProvider() {
    return provider;
  }

  public void setProvider(final String provider) {
    this.provider = provider;
  }

  public String getLocation() {
    return location;
  }

  public void setLocation(final String location) {
    this.location = location;
  }

  public Long getFromTsTime() {
    return fromTsTime;
  }

  public void setFromTsTime(final Long fromTsTime) {
    this.fromTsTime = fromTsTime;
  }

}
