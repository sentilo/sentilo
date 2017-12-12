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
