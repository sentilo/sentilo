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
package org.sentilo.platform.common.exception;

import java.util.List;

public class PlatformException extends Exception {

  /**
   * 
   */
  private static final long serialVersionUID = 1L;

  private int httpStatus;

  private List<String> errorDetails;

  public PlatformException() {
  }

  public PlatformException(final int status) {
    httpStatus = status;
  }

  public PlatformException(final String message) {
    super(message);
  }

  public PlatformException(final int status, final String message) {
    this(message);
    httpStatus = status;
  }

  public PlatformException(final int status, final String message, final List<String> errorDetails) {
    this(status, message);
    this.errorDetails = errorDetails;
  }

  public PlatformException(final Throwable cause) {
    super(cause);
  }

  public PlatformException(final String message, final Throwable cause) {
    super(message, cause);
  }

  public PlatformException(final int status, final Throwable cause) {
    super(cause);
    httpStatus = status;
  }

  public int getHttpStatus() {
    return httpStatus;
  }

  public void setHttpStatus(final int httpStatus) {
    this.httpStatus = httpStatus;
  }

  public void setErrorDetails(final List<String> errorDetails) {
    this.errorDetails = errorDetails;
  }

  public List<String> getErrorDetails() {
    return errorDetails;
  }
}
