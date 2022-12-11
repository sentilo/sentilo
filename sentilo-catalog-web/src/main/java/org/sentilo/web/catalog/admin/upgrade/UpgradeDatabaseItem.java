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
package org.sentilo.web.catalog.admin.upgrade;

import java.util.Date;

import org.sentilo.web.catalog.admin.upgrade.UpgradeDatabaseHook.ItemState;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;

@Document
public class UpgradeDatabaseItem {

  @Id
  private String id;
  private ItemState state;
  private Date updatedAt;
  private String description;

  public UpgradeDatabaseItem() {
    super();
  }

  public UpgradeDatabaseItem(final String id) {
    this();
    this.id = id;
  }

  public UpgradeDatabaseItem(final String id, final ItemState state, final Date updatedAt) {
    this(id);
    this.state = state;
    this.updatedAt = updatedAt;
  }

  public void changeState(final ItemState newState) {
    state = newState;
    updatedAt = new Date();
  }

  public void changeState(final ItemState newState, final String description) {
    changeState(newState);
    this.description = description;
  }

  public String getId() {
    return id;
  }

  public void setId(final String id) {
    this.id = id;
  }

  public ItemState getState() {
    return state;
  }

  public void setState(final ItemState state) {
    this.state = state;
  }

  public Date getUpdatedAt() {
    return updatedAt;
  }

  public void setUpdatedAt(final Date updatedAt) {
    this.updatedAt = updatedAt;
  }

  public String getDescription() {
    return description;
  }

  public void setDescription(final String description) {
    this.description = description;
  }

}
