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

import java.util.List;

import org.springframework.util.CollectionUtils;

/**
 * Class that encapsulates a sorted list of events and stores the timestamp from the first and last
 * events
 */
public class SortedEventsList<T> {

  private List<T> events;
  private Long from;
  private Long to;

  public SortedEventsList(final List<T> events) {
    this.events = events;
  }

  public int size() {
    return isEmpty() ? 0 : events.size();
  }

  public boolean isEmpty() {
    return CollectionUtils.isEmpty(events);
  }

  public T first() {
    return isEmpty() ? null : events.get(0);
  }

  public T last() {
    return isEmpty() ? null : events.get(size() - 1);
  }

  public Long getFrom() {
    return from;
  }

  public void setFrom(final Long from) {
    this.from = from;
  }

  public Long getTo() {
    return to;
  }

  public void setTo(final Long to) {
    this.to = to;
  }

  public List<T> getEvents() {
    return events;
  }

}
