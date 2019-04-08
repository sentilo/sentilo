package org.sentilo.web.catalog.breadcrumb;

import java.util.ArrayDeque;
import java.util.Deque;

/**
 * Utility class used to store into session the trail followed by the user when navigates between
 * internals lists.
 *
 * Stored URLs are used to define the back URL button
 */
public class BreadcrumbsTrail {

  final Deque<String> trail = new ArrayDeque<String>();

  public void clear() {
    trail.clear();
  }

  public void push(final String link) {
    trail.push(link);
  }

  public String poll() {
    return trail.poll();
  }

  public String firstEntry() {
    return trail.peek();
  }

}
