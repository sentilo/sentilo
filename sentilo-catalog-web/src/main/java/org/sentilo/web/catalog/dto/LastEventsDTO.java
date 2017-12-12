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
