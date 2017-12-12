package org.sentilo.web.catalog.utils;

import java.text.Collator;
import java.util.Comparator;

import org.sentilo.web.catalog.domain.AlphabeticalSortable;
import org.springframework.context.i18n.LocaleContextHolder;

public class AlphabeticalComparator implements Comparator<AlphabeticalSortable> {

  private final Collator collator;

  public AlphabeticalComparator() {
    collator = Collator.getInstance(LocaleContextHolder.getLocale());
  }

  @Override
  public int compare(final AlphabeticalSortable o1, final AlphabeticalSortable o2) {
    final String sv1 = o1.getSortableValue();
    final String sv2 = o2.getSortableValue();

    if (sv1 != null && sv2 != null) {
      return collator.compare(sv1, sv2);
    } else {
      return compareNullableValues(sv1, sv2);
    }
  }

  private int compareNullableValues(final String sv1, final String sv2) {
    if (sv1 != null && sv2 == null) {
      return 1;
    } else if (sv1 == null && sv2 != null) {
      return -1;
    } else {
      return 0;
    }
  }

}
