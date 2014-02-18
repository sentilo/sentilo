/*
 * Sentilo
 * 
 * Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de Barcelona.
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
package org.sentilo.web.catalog.search.resolver;

import java.beans.PropertyEditorSupport;
import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.util.HashSet;
import java.util.Set;

import javax.servlet.ServletRequest;
import javax.servlet.http.HttpServletRequest;

import org.sentilo.web.catalog.search.builder.SearchFilterUtils;
import org.springframework.beans.PropertyValue;
import org.springframework.beans.PropertyValues;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.core.MethodParameter;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.data.web.PageableArgumentResolver;
import org.springframework.data.web.PageableDefaults;
import org.springframework.validation.DataBinder;
import org.springframework.web.bind.ServletRequestDataBinder;
import org.springframework.web.bind.ServletRequestParameterPropertyValues;
import org.springframework.web.bind.support.WebArgumentResolver;
import org.springframework.web.context.request.NativeWebRequest;

/**
 * Based on {@link PageableArgumentResolver}. This resolver adds the translation of the sort column
 * position to the real name of the attribute for which to apply the sorting in the repository
 */
public class CatalogPageableArgumentResolver implements WebArgumentResolver {

  private static final Pageable DEFAULT_PAGE_REQUEST = new PageRequest(0, 10);
  private static final String DEFAULT_PREFIX = "page";
  private static final String DEFAULT_SEPARATOR = ".";

  private Pageable fallbackPagable = DEFAULT_PAGE_REQUEST;
  private String prefix = DEFAULT_PREFIX;
  private String separator = DEFAULT_SEPARATOR;

  /**
   * Setter to configure a fallback instance of {@link Pageable} that is being used to back missing
   * parameters. Defaults to {@value #DEFAULT_PAGE_REQUEST}.
   * 
   * @param fallbackPagable the fallbackPagable to set
   */
  public void setFallbackPagable(final Pageable fallbackPagable) {
    this.fallbackPagable = null == fallbackPagable ? DEFAULT_PAGE_REQUEST : fallbackPagable;
  }

  /**
   * Setter to configure the prefix of request parameters to be used to retrieve paging information.
   * Defaults to {@link #DEFAULT_PREFIX}.
   * 
   * @param prefix the prefix to set
   */
  public void setPrefix(final String prefix) {
    this.prefix = null == prefix ? DEFAULT_PREFIX : prefix;
  }

  /**
   * Setter to configure the separator between prefix and actual property value. Defaults to
   * {@link #DEFAULT_SEPARATOR}.
   * 
   * @param separator the separator to set
   */
  public void setSeparator(final String separator) {
    this.separator = null == separator ? DEFAULT_SEPARATOR : separator;
  }

  /*
   * (non-Javadoc)
   * 
   * @see
   * org.springframework.web.bind.support.WebArgumentResolver#resolveArgument(org.springframework
   * .core.MethodParameter, org.springframework.web.context.request.NativeWebRequest)
   */
  public Object resolveArgument(final MethodParameter methodParameter, final NativeWebRequest webRequest) {

    if (methodParameter.getParameterType().equals(Pageable.class)) {

      assertPageableUniqueness(methodParameter);

      Pageable request = getDefaultFromAnnotationOrFallback(methodParameter);
      final ServletRequest servletRequest = (ServletRequest) webRequest.getNativeRequest();
      final PropertyValues propertyValues = new ServletRequestParameterPropertyValues(servletRequest, getPrefix(methodParameter), separator);

      final DataBinder binder = new ServletRequestDataBinder(request);

      binder.initDirectFieldAccess();
      binder.registerCustomEditor(Sort.class, new SortPropertyEditor("sort.dir", propertyValues, webRequest));
      binder.bind(propertyValues);

      if (request.getPageNumber() > 0) {
        request = new PageRequest(request.getPageNumber() - 1, request.getPageSize(), request.getSort());
      }

      return request;
    }

    return UNRESOLVED;
  }

  private Pageable getDefaultFromAnnotationOrFallback(final MethodParameter methodParameter) {

    // search for PageableDefaults annotation
    for (final Annotation annotation : methodParameter.getParameterAnnotations()) {
      if (annotation instanceof PageableDefaults) {
        return getDefaultPageRequestFrom((PageableDefaults) annotation);
      }
    }

    // Construct request with fallback request to ensure sensible
    // default values. Create fresh copy as Spring will manipulate the
    // instance under the covers
    return new PageRequest(fallbackPagable.getPageNumber(), fallbackPagable.getPageSize(), fallbackPagable.getSort());
  }

  private static Pageable getDefaultPageRequestFrom(final PageableDefaults defaults) {

    // +1 is because we substract 1 later
    final int defaultPageNumber = defaults.pageNumber() + 1;
    final int defaultPageSize = defaults.value();

    if (defaults.sort().length == 0) {
      return new PageRequest(defaultPageNumber, defaultPageSize);
    }

    return new PageRequest(defaultPageNumber, defaultPageSize, defaults.sortDir(), defaults.sort());
  }

  /**
   * Resolves the prefix to use to bind properties from. Will prepend a possible {@link Qualifier}
   * if available or return the configured prefix otherwise.
   * 
   * @param parameter
   * @return
   */
  private String getPrefix(final MethodParameter parameter) {

    for (final Annotation annotation : parameter.getParameterAnnotations()) {
      if (annotation instanceof Qualifier) {
        return new StringBuilder(((Qualifier) annotation).value()).append("_").append(prefix).toString();
      }
    }

    return prefix;
  }

  /**
   * Asserts uniqueness of all {@link Pageable} parameters of the method of the given
   * {@link MethodParameter}.
   * 
   * @param parameter
   */
  private void assertPageableUniqueness(final MethodParameter parameter) {

    final Method method = parameter.getMethod();

    if (containsMoreThanOnePageableParameter(method)) {
      final Annotation[][] annotations = method.getParameterAnnotations();
      assertQualifiersFor(method.getParameterTypes(), annotations);
    }
  }

  /**
   * Returns whether the given {@link Method} has more than one {@link Pageable} parameter.
   * 
   * @param method
   * @return
   */
  private boolean containsMoreThanOnePageableParameter(final Method method) {

    boolean pageableFound = false;

    for (final Class<?> type : method.getParameterTypes()) {

      if (pageableFound && type.equals(Pageable.class)) {
        return true;
      }

      if (type.equals(Pageable.class)) {
        pageableFound = true;
      }
    }

    return false;
  }

  /**
   * Asserts that every {@link Pageable} parameter of the given parameters carries an
   * {@link Qualifier} annotation to distinguish them from each other.
   * 
   * @param parameterTypes
   * @param annotations
   */
  private void assertQualifiersFor(final Class<?>[] parameterTypes, final Annotation[][] annotations) {

    final Set<String> values = new HashSet<String>();

    for (int i = 0; i < annotations.length; i++) {

      if (Pageable.class.equals(parameterTypes[i])) {

        final Qualifier qualifier = findAnnotation(annotations[i]);

        if (null == qualifier) {
          throw new IllegalStateException("Ambiguous Pageable arguments in handler method. If you use multiple parameters of type Pageable you need to qualify them with @Qualifier");
        }

        if (values.contains(qualifier.value())) {
          throw new IllegalStateException("Values of the user Qualifiers must be unique!");
        }

        values.add(qualifier.value());
      }
    }
  }

  /**
   * Returns a {@link Qualifier} annotation from the given array of {@link Annotation}s. Returns
   * {@literal null} if the array does not contain a {@link Qualifier} annotation.
   * 
   * @param annotations
   * @return
   */
  private Qualifier findAnnotation(final Annotation[] annotations) {

    for (final Annotation annotation : annotations) {
      if (annotation instanceof Qualifier) {
        return (Qualifier) annotation;
      }
    }

    return null;
  }

  /**
   * {@link java.beans.PropertyEditor} to create {@link Sort} instances from textual
   * representations. The implementation interprets the string as a comma separated list where the
   * first entry is the sort direction ( {@code asc}, {@code desc}) followed by the properties to
   * sort by.
   * 
   * @author Oliver Gierke
   */
  private static class SortPropertyEditor extends PropertyEditorSupport {

    private final String orderProperty;
    private final PropertyValues values;
    private final HttpServletRequest httpRequest;

    /**
     * Creates a new {@link SortPropertyEditor}.
     * 
     * @param orderProperty
     * @param values
     */
    public SortPropertyEditor(final String orderProperty, final PropertyValues values, final NativeWebRequest webRequest) {

      this.orderProperty = orderProperty;
      this.values = values;
      httpRequest = (HttpServletRequest) webRequest.getNativeRequest();
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.beans.PropertyEditorSupport#setAsText(java.lang.String)
     */
    @Override
    public void setAsText(final String text) {

      final PropertyValue rawOrder = values.getPropertyValue(orderProperty);
      final Direction order = null == rawOrder ? Direction.ASC : Direction.fromString(rawOrder.getValue().toString());
      final String columnName = SearchFilterUtils.translateSortProperty(getListName(), text);

      setValue(new Sort(order, columnName));
    }

    private String getListName() {
      return SearchFilterUtils.getListName(httpRequest);
    }
  }

}
