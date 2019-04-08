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
package org.sentilo.common.test;

import java.io.IOException;
import java.lang.reflect.Method;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.io.Resource;
import org.springframework.core.io.support.PathMatchingResourcePatternResolver;
import org.springframework.core.io.support.ResourcePatternResolver;
import org.springframework.core.io.support.ResourcePatternUtils;
import org.springframework.util.ClassUtils;

public class TestClassesPatternResolver {

  private static final Logger LOGGER = LoggerFactory.getLogger(TestClassesPatternResolver.class);

  private final List<Class<?>> testClasses = new ArrayList<Class<?>>();
  private final String basePackage;
  private final ResourcePatternResolver resourcePatternResolver = new PathMatchingResourcePatternResolver();

  public TestClassesPatternResolver(final String basePackage, final List<String> resourceLocations) throws IOException, ClassNotFoundException {
    this.basePackage = basePackage;

    for (final String resourceLocation : resourceLocations) {
      testClasses.addAll(resolveClassesFromResourcePatterns(resourceLocation));
    }
  }

  public List<Class<?>> getClasses() {
    return testClasses;
  }

  private Collection<Class<?>> resolveClassesFromResourcePatterns(final String pattern) throws IOException, ClassNotFoundException {
    final Collection<Class<?>> classes = new ArrayList<Class<?>>();

    if (ResourcePatternUtils.isUrl(pattern)) {
      final Resource[] resources = resourcePatternResolver.getResources(pattern);

      for (final Resource resource : resources) {
        final URL url = resource.getURL();
        LOGGER.debug("Resolving Class Name for URL: {}", url.toString());
        final String className = resolveClassFromPath(url.toString());
        LOGGER.debug("Finding @Test methods in class with Name: {}", className);

        addIfNeedBe(classes, className);
      }
    }

    return classes;
  }

  /**
   * Add the class with name <code>className</code> to the classes list If at least one method is
   * annotated with the JUnit <code>@Test</code> annotation
   *
   * @param classes
   * @param className
   * @throws ClassNotFoundException
   */
  private void addIfNeedBe(final Collection<Class<?>> classes, final String className) throws ClassNotFoundException {
    if (className != null && className.length() > 0) {
      final Class<?> klass = Class.forName(className);
      for (final Method method : klass.getDeclaredMethods()) {
        if (method.getAnnotation(org.junit.Test.class) != null) {
          LOGGER.debug("Found @Test method {} in class {}", method.getName(), klass.getName());
          classes.add(klass);
          break;
        }
      }
    }
  }

  private String resolveClassFromPath(final String url) {
    final String classUrl = ClassUtils.convertResourcePathToClassName(url);
    final int begin = classUrl.indexOf(basePackage);
    if (begin < 0) {
      return null;
    }

    final String classSubpackageUrl = classUrl.substring(begin);
    return classSubpackageUrl.substring(0, classSubpackageUrl.indexOf(".class"));
  }

}
