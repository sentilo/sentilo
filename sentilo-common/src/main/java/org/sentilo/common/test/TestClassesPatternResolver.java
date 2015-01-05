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
package org.sentilo.common.test;

import java.io.IOException;
import java.lang.reflect.Method;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.springframework.core.io.Resource;
import org.springframework.core.io.support.PathMatchingResourcePatternResolver;
import org.springframework.core.io.support.ResourcePatternResolver;
import org.springframework.core.io.support.ResourcePatternUtils;
import org.springframework.util.ClassUtils;

public class TestClassesPatternResolver {

  private final List<Class<?>> _classes = new ArrayList<Class<?>>();
  private final String _basePackage;
  private final ResourcePatternResolver _resourcePatternResolver = new PathMatchingResourcePatternResolver();

  public TestClassesPatternResolver(final String basePackage, final List<String> resourceLocations) throws IOException, ClassNotFoundException {

    _basePackage = basePackage;

    for (final String resourceLocation : resourceLocations) {
      _classes.addAll(resolveClassesFromResourcePatterns(resourceLocation));
    }

  }

  public List<Class<?>> getClasses() {
    return _classes;
  }

  private Collection<Class<?>> resolveClassesFromResourcePatterns(final String pattern) throws IOException, ClassNotFoundException {

    final Collection<Class<?>> classes = new ArrayList<Class<?>>();

    if (ResourcePatternUtils.isUrl(pattern)) {

      final Resource[] resources = _resourcePatternResolver.getResources(pattern);

      for (final Resource resource : resources) {

        final URL url = resource.getURL();

        System.out.println("Resolving Class Name for URL: " + url.toString());

        final String className = resolveClassFromPath(url.toString());

        System.out.println("Finding methods in Class Name: " + className);

        if (className != null && className.length() > 0) {
          final Class<?> klass = Class.forName(className);
          for (final Method method : klass.getDeclaredMethods()) {
            // If at least one method is annotated with the JUnit
            // @Test annotation, we include the class as a Test Class.
            if (method.getAnnotation(org.junit.Test.class) != null) {
              System.out.println("Found @Test method " + method.getName() + " in class " + klass.getName());
              classes.add(klass);
              break;
            }
          }
        }
      }
    }

    return classes;
  }

  private String resolveClassFromPath(final String url) {
    final String classUrl = ClassUtils.convertResourcePathToClassName(url);
    final int begin = classUrl.indexOf(_basePackage);
    if (begin < 0) {
      return null;
    }
    return classUrl.substring(begin, classUrl.indexOf(".class"));
  }

}
