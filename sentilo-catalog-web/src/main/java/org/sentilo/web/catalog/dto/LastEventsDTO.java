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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.sentilo.web.catalog.domain.SortedEventsList;
import org.sentilo.web.catalog.format.datetime.LocalDateFormatter;
import org.springframework.util.CollectionUtils;

public class LastEventsDTO<T> {

  private List<T> events;
  private Long fromTime;
  private String fromTimestamp;
  private Long toTime;
  private String toTimestamp;
  private final int size;

  public LastEventsDTO(final List<T> events, final Long from, final Long to, final LocalDateFormatter localDateFormatter) {
    this.events = new ArrayList<T>();
    this.fromTime = null;
    this.fromTimestamp = null;
    this.toTime = null;
    this.toTimestamp = null;

    if (!CollectionUtils.isEmpty(events)) {
      this.events = events;
      this.fromTime = from;
      this.fromTimestamp = localDateFormatter.printAsLocalTime(from);
      this.toTime = to;
      this.toTimestamp = localDateFormatter.printAsLocalTime(to);
    }

    this.size = events.size();
  }

  public LastEventsDTO(final SortedEventsList<T> events, final LocalDateFormatter localDateFormatter) {
    this(events.getEvents(), events.getFrom(), events.getTo(), localDateFormatter);
  }

  public List<T> getEvents() {
    return events;
  }

  public void setEvents(final List<T> events) {
    this.events = events;
  }

  public Long getFromTime() {
    return fromTime;
  }

  public void setFromTime(final Long fromTime) {
    this.fromTime = fromTime;
  }

  public String getFromTimestamp() {
    return fromTimestamp;
  }

  public void setFromTimestamp(final String fromTimestamp) {
    this.fromTimestamp = fromTimestamp;
  }

  public Long getToTime() {
    return toTime;
  }

  public void setToTime(final Long toTime) {
    this.toTime = toTime;
  }

  public String getToTimestamp() {
    return toTimestamp;
  }

  public void setToTimestamp(final String toTimestamp) {
    this.toTimestamp = toTimestamp;
  }

  public int getSize() {
    return size;
  }

  public void reverse() {
    if (!CollectionUtils.isEmpty(this.events)) {
      Collections.reverse(this.events);
    }
  }

  @Override
  public String toString() {
    return "\n--- LastEventsDTO ---" + "\n\tFrom: " + this.fromTime + " - " + this.fromTimestamp + "\n\tTo  : " + this.toTime + " - "
        + this.toTimestamp + "\n\tSize: " + this.size;
  }
}
