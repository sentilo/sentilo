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
package org.sentilo.web.demo.common.utils;

import java.lang.reflect.Constructor;
import java.util.ArrayList;
import java.util.List;

import org.codehaus.jackson.JsonEncoding;
import org.codehaus.jackson.map.ObjectMapper;
import org.codehaus.jackson.type.JavaType;
import org.sentilo.common.exception.MessageNotReadableException;
import org.sentilo.platform.client.core.domain.Observation;
import org.sentilo.platform.client.core.domain.ObservationsOutputMessage;
import org.sentilo.web.demo.common.domain.ObservationData;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.StringUtils;


public class ConvertUtils {

	private final Logger logger = LoggerFactory.getLogger(ConvertUtils.class);
	
	public static List<ObservationData> convert(String sensorId, String providerId, ObservationsOutputMessage obsMessage) {
		List<ObservationData> observations =  new ArrayList<ObservationData>();
		for(Observation observation: obsMessage.getObservations()){
			ObservationData obs = new ObservationData(sensorId, providerId, observation.getValue(), observation.getTimestamp());
			observations.add(obs);
		}
		return observations;
	}
	
	protected static final JsonEncoding DEFAULT_ENCODING = JsonEncoding.UTF8;
	private ObjectMapper objectMapper = new ObjectMapper();
	
	/**
	 * Serializa el contenido de <code>json</code> en un objeto de tipo <code>clazz</code>.
	 * Si json es null o no tiene contenido, retorna el resultado de invocar al constructor 
	 * vacio de <code>clazz</code>
	 * @param clazz
	 * @param json
	 * @return
	 * @throws MessageNotReadableException
	 */
	public Object readInternal(Class<?> clazz, String json) throws MessageNotReadableException {
		JavaType javaType = getJavaType(clazz);
		try {
			if(StringUtils.hasText(json)){
				return this.objectMapper.readValue(json, javaType);
			}else{
				return buildDefaultInstance(clazz);
			}
		} catch (Exception e) {
			throw new MessageNotReadableException(e);
		}
	}
	
	@SuppressWarnings("rawtypes")
	protected Object buildDefaultInstance(Class<?> clazz){		
        try {
        	Class[] empty = {};		
        	Constructor<?> defaultConstructor =  clazz.getConstructor(empty);    	    		
            return defaultConstructor.newInstance((Object[])null);
        } catch (Throwable e) {     
        	logger.warn("Error calling default constructor of class {}: {}", clazz.getName(), e);
            return null;
        }		
	}
	
	protected JavaType getJavaType(Class<?> clazz) {
		return objectMapper.getTypeFactory().constructType(clazz);
	}


}
