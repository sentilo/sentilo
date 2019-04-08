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
package org.sentilo.agent.relational.domain;

import java.sql.Timestamp;

import org.sentilo.common.domain.EventMessage;

public abstract class Data {

  /** Ds to use to persist the data. */
  private String targetDs;

  /** Event time stamp formatted as String, following the Sentilo pattern */
  private String timestamp;

  /** Same value as timestamp field but as long time */
  private long eventTimestamp;

  /** Time stamp when data was published on Sentilo as long time. */
  private long publishedAt;

  /** Who has published data on Sentilo */
  private String publisher;

  /** Source event received by the agent */
  private EventMessage sourceEvent;

  public void setTargetDs(final String targetDs) {
    this.targetDs = targetDs;
  }

  public String getTargetDs() {
    return targetDs;
  }

  public String getTimestamp() {
    return timestamp;
  }

  public void setTimestamp(final String timestamp) {
    this.timestamp = timestamp;
  }

  public long getPublishedAt() {
    return publishedAt;
  }

  public void setPublishedAt(final long publishedAt) {
    this.publishedAt = publishedAt;
  }

  public EventMessage getSourceEvent() {
    return sourceEvent;
  }

  public void setSourceEvent(final EventMessage sourceEvent) {
    this.sourceEvent = sourceEvent;
  }

  public String getPublisher() {
    return publisher;
  }

  public void setPublisher(final String publisher) {
    this.publisher = publisher;
  }

  public long getEventTimestamp() {
    return eventTimestamp;
  }

  public void setEventTimestamp(final long eventTimestamp) {
    this.eventTimestamp = eventTimestamp;
  }

  public void setPublished_At(final Timestamp published_at) {
    setPublishedAt(published_at.getTime());
  }

}
