/*
 * Sentilo
 *   
 * Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de  Barcelona.
 *   
 * This program is licensed and may be used, modified and redistributed under the
 * terms  of the European Public License (EUPL), either version 1.1 or (at your 
 * option) any later version as soon as they are approved by the European 
 * Commission.
 *   
 * Alternatively, you may redistribute and/or modify this program under the terms
 * of the GNU Lesser General Public License as published by the Free Software 
 * Foundation; either  version 3 of the License, or (at your option) any later 
 * version. 
 *   
 * Unless required by applicable law or agreed to in writing, software distributed
 * under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR 
 * CONDITIONS OF ANY KIND, either express or implied. 
 *   
 * See the licenses for the specific language governing permissions, limitations 
 * and more details.
 *   
 * You should have received a copy of the EUPL1.1 and the LGPLv3 licenses along 
 * with this program; if not, you may find them at: 
 *   
 *   https://joinup.ec.europa.eu/software/page/eupl/licence-eupl
 *   http://www.gnu.org/licenses/ 
 *   and 
 *   https://www.gnu.org/licenses/lgpl.txt
 */
package org.sentilo.platform.common.exception;

import org.apache.http.HttpStatus;

@SuppressWarnings("serial")
public class PlatformException extends Exception {

	private int httpStatus;

	public PlatformException(int status, String message) {
		super(message);
		this.httpStatus = status;
	}

	public static PlatformException badRequest(String message, Throwable cause) {
		return new PlatformException(HttpStatus.SC_BAD_REQUEST, message, cause);
	}

	public PlatformException(int status, String message, Throwable cause) {
		super(cause);
		this.httpStatus = status;
	}

	public PlatformException(int status) {
		this.httpStatus = status;
	}

	public PlatformException() {
	}

	public PlatformException(String message) {
		super(message);
	}

	public PlatformException(Throwable cause) {
		super(cause);
	}

	public PlatformException(String message, Throwable cause) {
		super(message, cause);
	}

	public PlatformException(int status, Throwable cause) {
		super(cause);
		this.httpStatus = status;
	}

	public int getHttpStatus() {
		return httpStatus;
	}

	public void setHttpStatus(int httpStatus) {
		this.httpStatus = httpStatus;
	}
}
