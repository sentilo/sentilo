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
package org.sentilo.platform.server.handler.impl;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import org.apache.http.HttpStatus;
import org.sentilo.platform.common.exception.PlatformException;
import org.sentilo.platform.server.handler.HandlerPath;
import org.sentilo.platform.server.http.HttpMethod;
import org.sentilo.platform.server.request.RequestUtils;
import org.sentilo.platform.server.request.SentiloRequest;
import org.sentilo.platform.server.request.SentiloResource;
import org.springframework.util.ReflectionUtils;

public abstract class AbstractBaseHandlerTest {

  protected void simulateRequest(final HttpMethod method, final String tokenProvider, final String path) throws PlatformException {
    String resourcePath = RequestUtils.extractResource(path, getHandlerPath().getPath());
    final String[] resourceParts = RequestUtils.splitResource(resourcePath);

    when(getSentiloResource().getParts()).thenReturn(resourceParts);

    if (resourceParts.length > 0) {
      when(getSentiloRequest().getResourcePart(0)).thenReturn(resourceParts[0]);
    }
    if (resourceParts.length > 1) {
      when(getSentiloRequest().getResourcePart(1)).thenReturn(resourceParts[1]);
    }
    if (resourceParts.length > 2) {
      when(getSentiloRequest().getResourcePart(2)).thenReturn(resourceParts[2]);
    }
    when(getSentiloRequest().getMethod()).thenReturn(method);
    when(getSentiloRequest().getEntitySource()).thenReturn(tokenProvider);

    try {
      when(getSentiloRequest().getBody()).thenReturn("");
    } catch (final Exception e) {
      throw new PlatformException(e);
    }
  }

  protected void assertForbiddenCall(final PlatformException e) {
    assertEquals("Must return 403 - Method not allowed", HttpStatus.SC_FORBIDDEN, e.getHttpStatus());
  }

  protected void assertMethodNotAllowed(final PlatformException e) {
    assertEquals("Must return 405 - Method not allowed", HttpStatus.SC_METHOD_NOT_ALLOWED, e.getHttpStatus());
  }

  protected void assertBadRequest(final PlatformException e) {
    assertEquals("Must return 400 - Method not allowed", HttpStatus.SC_BAD_REQUEST, e.getHttpStatus());
  }

  protected void assertInternalServerError(final PlatformException e) {
    assertEquals("Must return 500 - Method not allowed", HttpStatus.SC_INTERNAL_SERVER_ERROR, e.getHttpStatus());
  }

  protected <T> void injectField(final String fieldName, final Object valueToInject, final T instance, final Class<T> instanceClass) throws Exception {
    final Field field = instanceClass.getDeclaredField(fieldName);
    ReflectionUtils.makeAccessible(field);
    field.set(instance, valueToInject);
  }

  protected <T> List<T> generateRandomList(final Class<T> clazz) throws InstantiationException, IllegalAccessException {
    final Random randomGenerator = new Random();
    int size = 0;
    do {
      size = randomGenerator.nextInt(5);
    } while (size == 0);

    final List<T> randomList = new ArrayList<T>();
    for (int i = 0; i < size; i++) {
      randomList.add(clazz.newInstance());
    }

    return randomList;
  }

  protected abstract HandlerPath getHandlerPath();

  protected abstract SentiloResource getSentiloResource();

  protected abstract SentiloRequest getSentiloRequest();
}
