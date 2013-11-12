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
package org.sentilo.web.catalog.exception.builder;

import org.sentilo.web.catalog.exception.DuplicateKeyException;

/**
 * Builder strategy for DuplicateKeyException errors when the key is compound with 2 tokens separated by default with the char '@'.  
 */
public class CompoundDuplicateKeyExceptionBuilder implements DuplicateKeyExceptionBuilder {

	private static final String DEFAULT_SEPARATOR_TOKEN = "@";
	private final String errorMessageKey;	
	private String separatorToken = DEFAULT_SEPARATOR_TOKEN;
	
	public CompoundDuplicateKeyExceptionBuilder(String errorMessageKey){
		this.errorMessageKey = errorMessageKey;
	}
	
	public CompoundDuplicateKeyExceptionBuilder(String errorMessageKey, String separatorToken){
		this.errorMessageKey = errorMessageKey;
		this.separatorToken = separatorToken;
	}
	
	@Override
	public DuplicateKeyException buildDuplicateKeyException(String invalidKey) {
		// compoundInvalidKey has the format token1'separatorToken'token2.
		String[] tokens = invalidKey.split(separatorToken);		
		Object[] args = {tokens[0],tokens[1]};
		throw new DuplicateKeyException(errorMessageKey, args);
	}

}
