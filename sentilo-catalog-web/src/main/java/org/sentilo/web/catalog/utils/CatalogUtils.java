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
package org.sentilo.web.catalog.utils;

import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;

import org.sentilo.web.catalog.domain.Location;
import org.springframework.util.StringUtils;


public abstract class CatalogUtils {
	
	public static String decodeAjaxParam(String source){
		String target = source; 
		try{	
			target = (source==null?source:URLDecoder.decode(source, "ISO-8859-1"));
		}catch(UnsupportedEncodingException ex){
			//ignore
		}
		
		return target;
	}	
	
	public static boolean arrayIsEmpty(Object[] source){
		return (source == null)  || (source.length == 0); 
	}
	
	public static boolean stringIsNotEmptyOrNull(String value){
		return StringUtils.hasText(value) && !value.toLowerCase().equals("null"); 
	}
	
	public static Location convertStringLocation(String coordinates){
		//Coordinates has the format "latitude longitude"		
		if(!StringUtils.hasText(coordinates)){
			return null;
		}
		
		Location location = null;
		//Control double whitespace 
		if(coordinates.indexOf(" ") != -1){
			int pos = coordinates.indexOf(" ");
			String latitude = coordinates.substring(0, pos).trim();
			String longitude = coordinates.substring(pos+1).trim();
			
			location =  new Location(latitude, longitude);
		}
		
		return location;
	}
	
	public static String locationToString(Location location){				
		return (location == null?null:location.getLatitude() + " " + location.getLongitude());
	}
	
	private CatalogUtils(){
		//this prevents even the native class from calling this ctor as well :
	    throw new AssertionError();
	}
}
