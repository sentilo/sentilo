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
package org.sentilo.common.test.exception;

import org.junit.Assert;
import org.junit.Test;
import org.sentilo.common.exception.MessageConversionException;
import org.sentilo.common.exception.MessageNotReadableException;
import org.sentilo.common.exception.MessageNotWritableException;
import org.sentilo.common.exception.PlatformAccessException;
import org.sentilo.common.exception.RESTClientException;
import org.springframework.core.NestedExceptionUtils;

public class SentiloExceptionsTest {

  final String message = "mock exception message";
  final String causeMsg = "mock cause message";
  final Throwable cause = new Exception(causeMsg);

  @Test
  public void messageConversionException() {
    final Exception ex = new MessageConversionException(message);
    Assert.assertEquals(getMessage(message, null), ex.getMessage());

    final Exception ex1 = new MessageConversionException(message, cause);
    Assert.assertEquals(getMessage(message, cause), ex1.getMessage());
  }

  @Test
  public void messageNotReadableException() {
    final Exception ex = new MessageNotReadableException(message);
    Assert.assertEquals(getMessage(message, null), ex.getMessage());

    final Exception ex1 = new MessageNotReadableException(message, cause);
    Assert.assertEquals(getMessage(message, cause), ex1.getMessage());

    final Exception ex2 = new MessageNotReadableException(cause);
    Assert.assertEquals(getMessage(cause.getMessage(), cause), ex2.getMessage());
  }

  @Test
  public void messageNotWritableException() {
    final Exception ex = new MessageNotWritableException(message);
    Assert.assertEquals(getMessage(message, null), ex.getMessage());

    final Exception ex1 = new MessageNotWritableException(message, cause);
    Assert.assertEquals(getMessage(message, cause), ex1.getMessage());

    final Exception ex2 = new MessageNotWritableException(cause);
    Assert.assertEquals(getMessage(cause.getMessage(), cause), ex2.getMessage());
  }

  @Test
  public void platformAccessException() {
    final Exception ex = new PlatformAccessException(message);
    Assert.assertEquals(getMessage(message, null), ex.getMessage());

    final Exception ex1 = new PlatformAccessException(message, cause);
    Assert.assertEquals(getMessage(message, cause), ex1.getMessage());
  }

  @Test
  public void restClientException() {
    final int status = 1;
    final Exception ex = new RESTClientException(status, message);
    Assert.assertEquals(getMessage(message, null), ex.getMessage());
    Assert.assertTrue(status == ((RESTClientException) ex).getStatus());

    final Exception ex1 = new RESTClientException(message, cause);
    Assert.assertEquals(getMessage(message, cause), ex1.getMessage());
    Assert.assertTrue(0 == ((RESTClientException) ex1).getStatus());

    final Exception ex2 = new RESTClientException(cause);
    Assert.assertEquals(getMessage(cause.getMessage(), cause), ex2.getMessage());
    Assert.assertTrue(0 == ((RESTClientException) ex2).getStatus());
  }

  private String getMessage(final String exceptionMessage, final Throwable exceptionCause) {
    return NestedExceptionUtils.buildMessage(exceptionMessage, exceptionCause);
  }

}
