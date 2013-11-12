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
package org.sentilo.platform.server.request;

import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

public abstract class RequestUtils {			
			
	public static final String TOKEN_PATH_SEPARATOR = "/";
	public static final String EMPTY_STRING = "";
		
	/**
	 * Dado un path del tipo /handler/token1/token2 retorna /handler
	 * @param path Path del cual extraer el identificador del handler,que corresponde al primer token del path.
	 * @return 
	 */	
	public static String extractHandlerPath(String path) {
		Assert.isTrue(path!=null && path.startsWith(TOKEN_PATH_SEPARATOR) , "Invalid path value. Must start with /");
		String[] tokens =  path.split(TOKEN_PATH_SEPARATOR);		
		return (tokens.length>=2)?TOKEN_PATH_SEPARATOR+tokens[1]:TOKEN_PATH_SEPARATOR;
	}

	/**
	 * Dado un path del tipo /handler/token1/token2 retorna token1/token2
	 * @param path Path del cual extraer la parte que no corresponde al identificador del handler.
	 * @return 
	 */
	public static String extractResource(String path) {
		Assert.isTrue(path!=null && path.startsWith(TOKEN_PATH_SEPARATOR),"Invalid path value. Must start with /");
		
		String handlerPath = extractHandlerPath(path);				
		return (handlerPath.length() == path.length())?EMPTY_STRING:path.substring(handlerPath.length()+1);						
	}

	public static String[] splitResource(String resource) {
		if (StringUtils.hasText(resource)) {
			return resource.split("/");
		}
		return null;
	}
}
