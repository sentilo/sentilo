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
package org.sentilo.platform.common.domain;

public class Statistics {
	private Events events;
	private Performance performance;
	
	public Statistics(){
		super();
	}

	public Statistics(Events events, Performance performance) {
		super();
		this.events = events;	
		this.performance = performance;
	}
		

	public Events getEvents() {
		return events;
	}	
	
	public Performance getPerformance() {
		return performance;
	}
	
	public static class Events{
		private Long total;
		private Long observations;
		private Long alarms;	
		private Long orders;
		
		public Events(Long total, Long alarms, Long observations, Long orders) {
			super();
			this.total = total;
			this.alarms = alarms;
			this.observations = observations;
			this.orders = orders;
		}

		public Long getTotal() {
			return total;
		}

		public Long getObservations() {
			return observations;
		}

		public Long getAlarms() {
			return alarms;
		}

		public Long getOrders() {
			return orders;
		}		
	}
	
	public static class Performance{
		private Float instantAvg;
		private Float dailyAvg;
		private Float maxAvg;
		
		public Performance(Float instantAvg, Float dailyAvg, Float maxAvg) {
			super();
			this.instantAvg = instantAvg;
			this.dailyAvg = dailyAvg;
			this.maxAvg = maxAvg;
		}

		public Float getInstantAvg() {
			return instantAvg;
		}

		public Float getDailyAvg() {
			return dailyAvg;
		}

		public Float getMaxAvg() {
			return maxAvg;
		}
		
		
	}

	

}
