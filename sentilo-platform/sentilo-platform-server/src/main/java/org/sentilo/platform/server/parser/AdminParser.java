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
package org.sentilo.platform.server.parser;

import java.io.IOException;

import org.apache.http.HttpStatus;
import org.sentilo.platform.common.domain.AdminInputMessage;
import org.sentilo.platform.common.domain.Statistics;
import org.sentilo.platform.common.domain.AdminInputMessage.AdminType;
import org.sentilo.platform.common.exception.PlatformException;
import org.sentilo.platform.server.request.SentiloRequest;
import org.sentilo.platform.server.response.SentiloResponse;
import org.springframework.util.StringUtils;



public class AdminParser extends PlatformJsonMessageConverter {
	
	public AdminInputMessage parseGetRequest(SentiloRequest request) throws PlatformException{						
		AdminType type = getAdminType(request);
		AdminInputMessage inputMessage = new AdminInputMessage(type);
		inputMessage.setEntity(request.getResourcePart(1));	
		
		return inputMessage;
	}	
	
	public void writeResponse(SentiloRequest request, SentiloResponse response, Statistics stats) throws PlatformException{				
		try{
			writeInternal(stats, response);
		}catch(IOException ex){
			throw new PlatformException(HttpStatus.SC_INTERNAL_SERVER_ERROR, ex);
		}				
	}		

	public AdminType getAdminType(SentiloRequest request){
		AdminType adminType = null;
		try{
			String resourcePart = request.getResourcePart(0);
			if(StringUtils.hasText(resourcePart)){
				adminType = AdminType.valueOf(resourcePart.toLowerCase());				
			}						
		}catch(IllegalArgumentException e){						
		}
		
		return adminType;
	}
	
}
