package org.sentilo.web.catalog.dto;

import java.util.ArrayList;
import java.util.List;

import org.sentilo.web.catalog.domain.AlphabeticalSortable;
import org.springframework.context.MessageSource;
import org.springframework.context.i18n.LocaleContextHolder;

public class OptionListDTOFactory {

  public static <T> List<OptionDTO> build(final List<T> items) {
    final List<OptionDTO> target = new ArrayList<OptionDTO>();

    for (final T item : items) {
      // item could be either a String or an AlphabeticalSortable, ....
      if (item instanceof String) {
        target.add(build((String) item));
      } else if (item instanceof AlphabeticalSortable) {
        target.add(build((AlphabeticalSortable) item));
      }
    }

    return target;
  }

  public static List<OptionDTO> build(final String[] suffixes, final String prefix, final MessageSource messageSource) {
    final List<OptionDTO> target = new ArrayList<OptionDTO>();
    for (final String value : suffixes) {
      final String trimValue = value.trim();
      final String label = messageSource.getMessage(prefix.concat(".").concat(trimValue), null, trimValue, LocaleContextHolder.getLocale());
      target.add(new OptionDTO(label, trimValue));
    }

    return target;
  }

  private static OptionDTO build(final String source) {
    return new OptionDTO(source, source);
  }

  private static OptionDTO build(final AlphabeticalSortable source) {
    final String value = source.getId();
    final String label = source.getSortableValue();

    return new OptionDTO(label, value);
  }

}
