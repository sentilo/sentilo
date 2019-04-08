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
package org.sentilo.platform.common.test.exception;

import java.util.Collections;
import java.util.List;

import org.apache.http.HttpStatus;
import org.junit.Assert;
import org.junit.Test;
import org.mockito.Mockito;
import org.sentilo.common.enums.EventType;
import org.sentilo.platform.common.exception.CatalogAccessException;
import org.sentilo.platform.common.exception.EventRejectedException;
import org.sentilo.platform.common.exception.JsonConverterException;
import org.sentilo.platform.common.exception.PlatformException;
import org.sentilo.platform.common.exception.RejectedResourcesContext;
import org.sentilo.platform.common.exception.ResourceNotFoundException;
import org.sentilo.platform.common.exception.ResourceOfflineException;
import org.sentilo.platform.common.exception.SentiloDataAccessException;
import org.springframework.core.NestedExceptionUtils;
import org.springframework.util.CollectionUtils;

public class SentiloPlatformExceptionsTest {

  final String message = "mock exception message";
  final String causeMsg = "mock cause message";
  final Throwable cause = new Exception(causeMsg);
  final List<String> errorDetails = Collections.<String>emptyList();

  @Test
  public void catalogAccessException() {
    final Exception ex1 = new CatalogAccessException(message, cause);
    Assert.assertEquals(getMessage(message, cause), ex1.getMessage());
  }

  @Test
  public void sentiloDataAccessException() {
    final Exception ex = new SentiloDataAccessException(message);
    Assert.assertEquals(getMessage(message, null), ex.getMessage());

    final Exception ex1 = new SentiloDataAccessException(message, cause);
    Assert.assertEquals(getMessage(message, cause), ex1.getMessage());
  }

  @Test
  public void platformException() {
    final Exception ex1 = new PlatformException(HttpStatus.SC_BAD_REQUEST, message);
    Assert.assertEquals(HttpStatus.SC_BAD_REQUEST, ((PlatformException) ex1).getHttpStatus());
    Assert.assertEquals(message, ex1.getMessage());

    final Exception ex2 = new PlatformException(HttpStatus.SC_BAD_REQUEST, message, errorDetails);
    Assert.assertEquals(HttpStatus.SC_BAD_REQUEST, ((PlatformException) ex2).getHttpStatus());
    Assert.assertEquals(message, ex2.getMessage());
    Assert.assertTrue(CollectionUtils.isEmpty(((PlatformException) ex2).getErrorDetails()));

    final Exception ex3 = new PlatformException(message, cause);
    Assert.assertEquals(0, ((PlatformException) ex3).getHttpStatus());
    Assert.assertEquals(cause, ex3.getCause());

    final Exception ex4 = new PlatformException(HttpStatus.SC_BAD_REQUEST, cause);
    Assert.assertEquals(HttpStatus.SC_BAD_REQUEST, ((PlatformException) ex4).getHttpStatus());

    final Exception ex5 = new PlatformException(cause);
    Assert.assertEquals(0, ((PlatformException) ex5).getHttpStatus());
    Assert.assertEquals(cause, ex5.getCause());
  }

  @Test
  public void jsonConverterException() {
    final Exception ex = new JsonConverterException(message);
    Assert.assertEquals(message, ex.getMessage());

    final Exception ex1 = new JsonConverterException(message, cause);
    Assert.assertEquals(HttpStatus.SC_BAD_REQUEST, ((JsonConverterException) ex1).getHttpStatus());
    Assert.assertFalse(CollectionUtils.isEmpty(((PlatformException) ex1).getErrorDetails()));

    final Exception ex2 = new JsonConverterException(message, errorDetails);
    Assert.assertEquals(HttpStatus.SC_BAD_REQUEST, ((PlatformException) ex2).getHttpStatus());
    Assert.assertEquals(message, ex2.getMessage());
    Assert.assertTrue(CollectionUtils.isEmpty(((PlatformException) ex2).getErrorDetails()));
  }

  @Test
  public void eventRejectedException() {
    final RejectedResourcesContext mockContext = Mockito.mock(RejectedResourcesContext.class);
    final Exception ex = new EventRejectedException(EventType.DATA, mockContext);
    Assert.assertEquals(HttpStatus.SC_NOT_FOUND, ((PlatformException) ex).getHttpStatus());
  }

  @Test
  public void resourceNotFoundException() {
    final String resourceId = "mockResource";
    final String resourceType = "mockType";
    final Exception ex = new ResourceNotFoundException(resourceId, resourceType);
    Assert.assertEquals(HttpStatus.SC_NOT_FOUND, ((PlatformException) ex).getHttpStatus());
    Assert.assertEquals("mockType [mockResource] not found on Sentilo (404.1).", ex.getMessage());
  }

  @Test
  public void resourceOffException() {
    final String resourceId = "mockResource";
    final String resourceType = "mockType";
    final Exception ex = new ResourceOfflineException(resourceId, resourceType);
    Assert.assertEquals(HttpStatus.SC_NOT_FOUND, ((PlatformException) ex).getHttpStatus());
    Assert.assertEquals("mockType [mockResource] is not online (404.2).", ex.getMessage());
  }

  private String getMessage(final String exceptionMessage, final Throwable exceptionCause) {
    return NestedExceptionUtils.buildMessage(exceptionMessage, exceptionCause);
  }
}
