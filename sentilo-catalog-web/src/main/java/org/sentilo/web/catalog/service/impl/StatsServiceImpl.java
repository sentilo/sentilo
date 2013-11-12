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
package org.sentilo.web.catalog.service.impl;

import java.util.Set;

import org.sentilo.common.rest.RESTClient;
import org.sentilo.web.catalog.domain.Application;
import org.sentilo.web.catalog.domain.Component;
import org.sentilo.web.catalog.domain.PlatformStatsMessage;
import org.sentilo.web.catalog.domain.Provider;
import org.sentilo.web.catalog.domain.Sensor;
import org.sentilo.web.catalog.domain.Statistics;
import org.sentilo.web.catalog.domain.User;
import org.sentilo.web.catalog.domain.Statistics.Accounts;
import org.sentilo.web.catalog.domain.Statistics.Devices;
import org.sentilo.web.catalog.domain.Statistics.Events;
import org.sentilo.web.catalog.domain.Statistics.Performance;
import org.sentilo.web.catalog.parser.PlatformStatsParser;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.service.ActivityService;
import org.sentilo.web.catalog.service.StatsService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.mongodb.core.MongoOperations;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;


@Service
public class StatsServiceImpl implements StatsService {

	@Autowired
	private MongoOperations mongoOps;
	
	@Autowired
	private RESTClient restClient;
	
	@Autowired 
	private ActivityService activityService;
	
	private PlatformStatsParser parser = new PlatformStatsParser();
	
	private PlatformStatsMessage currentPlatformStats;
	
	
	@Override
	public Statistics getCurrentStats() {
		Devices devices = getDevices();
		Accounts accounts = getAccounts();
		//PlatformStatsMessage platformStats = getPlatformStats();
		Performance performance = getPerformance(currentPlatformStats);
		Events events = getEvents(currentPlatformStats);				
		
		return new Statistics(devices, performance, events, accounts);
	}
	
	private Devices getDevices(){
		long sensors = mongoOps.count(buildCountQuery(),Sensor.class);
		long routersAndGateways = mongoOps.count(buildCountQuery(getGatewayAndRoutersFilter()), Component.class);
		long components = mongoOps.count(buildCountQuery(), Component.class);
		long others = components - routersAndGateways;
		
		return new Devices(sensors, routersAndGateways, others);
	}
	
	private Accounts getAccounts(){
		long users = mongoOps.count(buildCountQuery(),User.class);
		long applications = mongoOps.count(buildCountQuery(),Application.class);
		long providers = mongoOps.count(buildCountQuery(),Provider.class);
		
		return new Accounts(users, applications, providers);
	}
	
	private Performance getPerformance(PlatformStatsMessage message){
		float instantAvg = (message!=null?message.getPerformance().getInstantAvg():0);
		float dailyAvg = (message!=null?message.getPerformance().getDailyAvg():0);
		float maxAvg = (message!=null?message.getPerformance().getMaxAvg():0);
		
		return new Performance(instantAvg, dailyAvg, maxAvg);		
	}
	
	private Events getEvents(PlatformStatsMessage message){
		long total = (message!=null?message.getEvents().getTotal():0);
		long observations = (message!=null?message.getEvents().getObservations():0);
		long alarms = (message!=null?message.getEvents().getAlarms():0);	
		long orders = (message!=null?message.getEvents().getOrders():0);
		
		return new Events(total, alarms, observations, orders);	
	}
	
	
	@Scheduled(initialDelay=10000, fixedRate=300000)
	public void getAndSavePlatformActivityAndStats(){
		String response = restClient.get("admin/stats");		
		currentPlatformStats =  parser.unmarshall(response);
		Events events = getEvents(currentPlatformStats);		
		activityService.saveCurrentActivity(events);
	}
	
	protected Query buildCountQuery(){
		return this.buildCountQuery(new SearchFilter());
	}
	
	protected Query buildCountQuery(SearchFilter filter){		
		Criteria queryCriteria = new Criteria();				
				
		if(!filter.paramsIsEmpty()){
			// params contiene la lista de filtros a aplicar en modo disjuncion, es decir, con el operador OR
			// Además, la comparativa del valor siempre es mediante "contiene la palabra buscada", es decir, se debe comportar como un LIKE %value% en SQL 
			Set<String> params = filter.getParams().keySet();		
			Criteria[] aCriteria = new Criteria[params.size()]; 
			int i=0;
			for(String param: params){
				String regexp = ".*"+filter.getParams().get(param)+".*";			
				aCriteria[i] = Criteria.where(param).regex(regexp);
				i++;										
			}
			
			queryCriteria = queryCriteria.orOperator(aCriteria);
		}
		
		Query query = new Query(queryCriteria);
						
		return query;
	}
	
	private SearchFilter getGatewayAndRoutersFilter(){
		SearchFilter filter = new SearchFilter();
		filter.addParam("componentType", "gateway");
		filter.addParam("componentType", "router");
		
		return filter;
	}

}
