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
package org.sentilo.platform.client.core.domain;

import java.util.List;

import org.codehaus.jackson.annotate.JsonProperty;
import org.codehaus.jackson.map.annotate.JsonSerialize;
import org.springframework.util.CollectionUtils;

public class ProviderObservations {

  @JsonSerialize(include = JsonSerialize.Inclusion.NON_NULL)
  @JsonProperty("sensors")
  private List<SensorObservations> sensorsObservations;

  public ProviderObservations() {
    super();
  }

  @Override
  public String toString() {
    final StringBuffer sb = new StringBuffer();
    sb.append("--- Provider observations ---");
    if (!CollectionUtils.isEmpty(sensorsObservations)) {
      for (final SensorObservations observations : sensorsObservations) {
        sb.append(observations.toString());
      }
    }
    return sb.toString();
  }

  public void setSensorsObservations(final List<SensorObservations> sensorsObservations) {
    this.sensorsObservations = sensorsObservations;
  }

  public List<SensorObservations> getSensorsObservations() {
    return sensorsObservations;
  }

}
