package org.sentilo.web.catalog.domain;

/**
 * Interface to mark classes which could be sorted alphabetically. The method
 * {@link #getSortableValue()} returns the value to use by the sort process.
 */
public interface AlphabeticalSortable extends Identifiable {

  String getSortableValue();
}
