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
package org.sentilo.platform.client.core.domain;

import java.util.Date;

import org.codehaus.jackson.annotate.JsonIgnore;
import org.codehaus.jackson.map.annotate.JsonSerialize;
import org.sentilo.common.utils.DateUtils;
import org.springframework.util.StringUtils;


public class Observation {

	@JsonSerialize(include = JsonSerialize.Inclusion.NON_NULL)
	private String value;

	@JsonSerialize(include = JsonSerialize.Inclusion.NON_NULL)
	private String sensor;
	
	@JsonSerialize(include = JsonSerialize.Inclusion.NON_NULL)
	private String provider;

	@JsonSerialize(include = JsonSerialize.Inclusion.NON_NULL)
	private String timestamp;

	@JsonSerialize(include = JsonSerialize.Inclusion.NON_NULL)
	private String location;
		

	public Observation() {
		super();
	}

	public Observation(String value) {
		this();		
		this.value = value;
	}

	public Observation(String value, String timestamp) {
		this(value);
		this.timestamp = timestamp;
	}		
	
	public Observation(String value, String timestamp, String location) {
		this(value, timestamp);
		this.location = location;
	}		
	
	public Observation(String value, Date timestamp, String location) {
		this(value, DateUtils.toStringTimestamp(timestamp), location);		
	}

	public Observation(String value, Date timestamp) {
		this(value, timestamp, null);		
	}
	
	public Observation(String value, long timestamp, String location) {
		this(value, new Date(timestamp), location);		
	}

	public Observation(String value, long timestamp) {
		this(value, timestamp, null);		
	}
	
	@Override
	public String toString() {
		StringBuffer sb = new StringBuffer();
		sb.append("--- Observation ---");
		if(StringUtils.hasText(provider)){
			sb.append("\n\t provider:" + provider);
		}
		if(StringUtils.hasText(sensor)){
			sb.append("\n\t sensor:" + sensor);
		}
		sb.append("\n\t value:" + value);
		sb.append("\n\t timestamp:" + timestamp);
		sb.append("\n\t location:" + location);
		
		return sb.toString();
	}

	@JsonIgnore
	public Long getTimestampToMillis() {
		return DateUtils.toMillis(getTimestamp());
	}
	
	public String getValue() {
		return value;
	}

	public void setValue(String value) {
		this.value = value;
	}

	public String getSensor() {
		return sensor;
	}

	public void setSensor(String sensor) {
		this.sensor = sensor;
	}

	public String getTimestamp() {
		return timestamp;
	}

	public void setTimestamp(String timestamp) {
		this.timestamp = timestamp;
	}		

	public String getLocation() {
		return location;
	}

	public void setLocation(String location) {
		this.location = location;
	}

	public void setProvider(String provider) {
		this.provider = provider;
	}

	public String getProvider() {
		return provider;
	}
}
