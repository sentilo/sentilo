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
package org.sentilo.common.domain;

import java.util.List;

import org.codehaus.jackson.map.annotate.JsonSerialize;

public class CatalogResponseMessage {
	public static final String OK = "200";
	public static final String INTERNAL_SERVER_ERROR = "500";
	public static final String BAD_REQUEST = "400";
	
	@JsonSerialize(include = JsonSerialize.Inclusion.NON_EMPTY)
	private List<AuthorizedProvider> providers;
	
	@JsonSerialize(include = JsonSerialize.Inclusion.NON_EMPTY)
	private String code;	
	@JsonSerialize(include = JsonSerialize.Inclusion.NON_EMPTY)
	private String errorMessage;	
	
	public CatalogResponseMessage() {
		super();
		this.code = OK;
		this.errorMessage = "";
	}
	
	public CatalogResponseMessage(String errorMessage) {
		this();
		this.code = INTERNAL_SERVER_ERROR;
		this.errorMessage = errorMessage;
	}
	
	public CatalogResponseMessage(String errorCode, String errorMessage) {
		this(errorMessage);
		this.code = errorCode;		
	}
	
	public CatalogResponseMessage(List<AuthorizedProvider> providers) {
		this();
		this.providers = providers;		
	}
	
	public String getCode() {
		return code;
	}
	public void setCode(String code) {
		this.code = code;
	}
	public String getErrorMessage() {
		return errorMessage;
	}
	public void setErrorMessage(String errorMessage) {
		this.errorMessage = errorMessage;
	}

	public void setProviders(List<AuthorizedProvider> providers) {
		this.providers = providers;
	}

	public List<AuthorizedProvider> getProviders() {
		return providers;
	}	
}
