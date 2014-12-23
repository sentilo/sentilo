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
package org.sentilo.platform.common.test.exception;

import java.util.Collections;
import java.util.List;

import org.junit.Assert;
import org.junit.Test;
import org.sentilo.platform.common.exception.CatalogAccessException;
import org.sentilo.platform.common.exception.JsonConverterException;
import org.sentilo.platform.common.exception.PlatformException;
import org.sentilo.platform.common.exception.SentiloDataAccessException;
import org.springframework.core.NestedExceptionUtils;
import org.springframework.util.CollectionUtils;

public class SentiloPlatformExceptionsTest {

  final String message = "mock exception message";
  final String causeMsg = "mock cause message";
  final Throwable cause = new Exception(causeMsg);
  final int httpStatus = 400;
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
    final Exception ex = new PlatformException(httpStatus);
    Assert.assertEquals(httpStatus, ((PlatformException) ex).getHttpStatus());

    final Exception ex1 = new PlatformException(httpStatus, message);
    Assert.assertEquals(httpStatus, ((PlatformException) ex1).getHttpStatus());
    Assert.assertEquals(message, ex1.getMessage());

    final Exception ex2 = new PlatformException(httpStatus, message, errorDetails);
    Assert.assertEquals(httpStatus, ((PlatformException) ex2).getHttpStatus());
    Assert.assertEquals(message, ex2.getMessage());
    Assert.assertTrue(CollectionUtils.isEmpty(((PlatformException) ex2).getErrorDetails()));

    final Exception ex3 = new PlatformException(message, cause);
    Assert.assertEquals(0, ((PlatformException) ex3).getHttpStatus());
    Assert.assertEquals(cause, ex3.getCause());

    final Exception ex4 = new PlatformException(httpStatus, cause);
    Assert.assertEquals(httpStatus, ((PlatformException) ex4).getHttpStatus());

    final Exception ex5 = new PlatformException();
    ((PlatformException) ex5).setHttpStatus(httpStatus);
    Assert.assertEquals(httpStatus, ((PlatformException) ex5).getHttpStatus());
    Assert.assertNull(ex5.getCause());

    final Exception ex6 = new PlatformException(cause);
    Assert.assertEquals(0, ((PlatformException) ex6).getHttpStatus());
    Assert.assertEquals(cause, ex6.getCause());
  }

  @Test
  public void jsonConverterException() {
    final Exception ex = new JsonConverterException(message);
    Assert.assertEquals(message, ex.getMessage());

    final Exception ex1 = new JsonConverterException(message, cause);
    Assert.assertEquals(httpStatus, ((JsonConverterException) ex1).getHttpStatus());
    Assert.assertFalse(CollectionUtils.isEmpty(((PlatformException) ex1).getErrorDetails()));

    final Exception ex2 = new JsonConverterException(message, errorDetails);
    Assert.assertEquals(httpStatus, ((PlatformException) ex2).getHttpStatus());
    Assert.assertEquals(message, ex2.getMessage());
    Assert.assertTrue(CollectionUtils.isEmpty(((PlatformException) ex2).getErrorDetails()));
  }

  private String getMessage(final String exceptionMessage, final Throwable exceptionCause) {
    return NestedExceptionUtils.buildMessage(exceptionMessage, exceptionCause);
  }
}
