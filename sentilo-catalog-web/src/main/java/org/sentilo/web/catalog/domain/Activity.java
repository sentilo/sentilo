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
package org.sentilo.web.catalog.domain;

import java.util.Date;

import org.codehaus.jackson.annotate.JsonIgnore;
import org.sentilo.common.utils.DateUtils;
import org.sentilo.web.catalog.domain.Statistics.Events;
import org.springframework.data.annotation.Id;


public class Activity implements CatalogDocument, Comparable<Activity>{		
	private static final long serialVersionUID = 1L;
	
	@JsonIgnore
	@Id
	private int id;
	private long observations;
	@JsonIgnore
	private long totalObservations;
	private long alarms;	
	@JsonIgnore
	private long totalAlarms;	
	private long orders;
	@JsonIgnore
	private long totalOrders;
	private long timestamp;	
	
	public Activity(){
		super();
		
		timestamp = System.currentTimeMillis();
	}
	
	public Activity(Events events, Activity lastActivity){
		this();
		this.totalObservations = events.getObservations();
		this.totalOrders = events.getOrders();
		this.totalAlarms = events.getAlarms();
		
		this.observations = totalObservations - lastActivity.getTotalObservations();
		this.alarms = totalAlarms - lastActivity.getTotalAlarms();
		this.orders = totalOrders - lastActivity.getTotalOrders();
		
		this.id = getNextId(lastActivity.id);
		
		
	}
	
	public Activity(Events events){
		this(events, new Activity());		
	}
	
	private int getNextId(int lastId){
//		if(StringUtils.hasText(lastId)){
//			int iNextId = Integer.parseInt(lastId) + 1;
//			return Integer.toString(iNextId);
//		}else{
//			return "1";
//		}
		
		return lastId+1;
		
	}
	
	public String getTimestampToString() {
		return DateUtils.toStringTimestamp(new Date(timestamp));
	}
	
	@Override
	public String getId() {		
		return Integer.toString(id);
	}

	@Override
	public void setUpdateAt(Date date) {		
	}

	@Override
	public void setCreatedAt(Date date) {		
	}

	@JsonIgnore
	@Override
	public Date getCreatedAt() {
		return null;
	}

	public long getObservations() {
		return observations;
	}

	public void setObservations(long observations) {
		this.observations = observations;
	}
	
	public long getTotalObservations() {
		return totalObservations;
	}

	public void setTotalObservations(long totalObservations) {
		this.totalObservations = totalObservations;
	}

	public long getAlarms() {
		return alarms;
	}

	public void setAlarms(long alarms) {
		this.alarms = alarms;
	}

	public long getTotalAlarms() {
		return totalAlarms;
	}

	public void setTotalAlarms(long totalAlarms) {
		this.totalAlarms = totalAlarms;
	}

	public long getOrders() {
		return orders;
	}

	public void setOrders(long orders) {
		this.orders = orders;
	}

	public long getTotalOrders() {
		return totalOrders;
	}

	public void setTotalOrders(long totalOrders) {
		this.totalOrders = totalOrders;
	}

	public long getTimestamp() {
		return timestamp;
	}

	public void setTimestamp(long timestamp) {
		this.timestamp = timestamp;
	}

	public void setId(int id) {
		this.id = id;
	}

	@Override
	public int compareTo(Activity o1) {				
		return (new Long(timestamp)).compareTo(new Long(o1.getTimestamp()));
	}
	
		
}
