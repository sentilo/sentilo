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
package org.sentilo.web.catalog.domain;

import java.io.Serializable;

import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.util.StringUtils;

@Document
public class SensorSubstate implements AlphabeticalSortable, Serializable {

  private static final long serialVersionUID = 1L;

  private String id;
  private String code;
  private String defaultDesc = "Unknown substate";
  private String translateLocShort;
  private String translateLocLong;

  @Override
  public boolean equals(final Object obj) {
    if (!(obj instanceof SensorSubstate)) {
      return false;
    }

    if (code == null) {
      return false;
    }

    final SensorSubstate other = (SensorSubstate) obj;
    return code.equals(other.getCode());
  }

  @Override
  public int hashCode() {
    // Hashcode return must be consistent with the equals method
    final int prime = 137;
    int result = 1;
    result = prime * result + (code == null ? 0 : code.hashCode());
    return result;
  }

  public String getId() {
    return id;
  }

  public void setId(final String id) {
    this.id = id;
  }

  public String getCode() {
    return code;
  }

  public void setCode(final String code) {
    this.code = code;
  }

  public String getDefaultDesc() {
    return defaultDesc;
  }

  public void setDefaultDesc(final String defaultDesc) {
    this.defaultDesc = defaultDesc;
  }

  public String getTranslateDesc() {
    return StringUtils.hasText(translateLocLong) ? translateLocLong : translateLocShort;
  }

  public void setTranslateLocLong(final String translateLocLong) {
    this.translateLocLong = translateLocLong;
  }

  public void setTranslateLocShort(final String translateLocShort) {
    this.translateLocShort = translateLocShort;
  }

  public String getDescription() {
    return StringUtils.hasText(getTranslateDesc()) ? getTranslateDesc() : getDefaultDesc();
  }

  @Override
  public String getSortableValue() {
    return getDescription();
  }

}
