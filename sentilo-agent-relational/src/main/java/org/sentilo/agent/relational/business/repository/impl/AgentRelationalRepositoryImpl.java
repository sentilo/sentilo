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
package org.sentilo.agent.relational.business.repository.impl;

import java.util.HashMap;
import java.util.Map;

import javax.annotation.Resource;
import javax.sql.DataSource;

import org.sentilo.agent.relational.business.repository.AgentRelationalRepository;
import org.sentilo.agent.relational.common.domain.Alarm;
import org.sentilo.agent.relational.common.domain.Data;
import org.sentilo.agent.relational.common.domain.Observation;
import org.sentilo.agent.relational.common.domain.Order;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Repository;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;


@Repository
public class AgentRelationalRepositoryImpl implements AgentRelationalRepository {
		
	private final Logger logger = LoggerFactory.getLogger(AgentRelationalRepositoryImpl.class);
	
	private Map<String,JdbcTemplate> jdbcTemplates;	
	
	private static final String OBSERVATION_PS = "insert into sentilo_observations (provider, sensor, value, timestamp) values (?,?,?,?)";
    private static final String ORDER_PS = "insert into sentilo_orders (provider, sensor, message, timestamp) values (?,?,?,?)";
    private static final String ALARM_PS = "insert into sentilo_alarms (alarm, message, timestamp) values (?,?,?)";
	
    @Resource
    public void setDataSources(Map<String, DataSource> dataSources) {		
    	// For every Ds this method creates and initializes a new JdbcTemplate and associates it with the dataSource key.
		if(!CollectionUtils.isEmpty(dataSources)){
			logger.debug("Number of dataSources to register: {}", dataSources.size());
			jdbcTemplates = new HashMap<String, JdbcTemplate>();
			for(String key: dataSources.keySet()){
				logger.debug("Registering dataSource {}", key);
				jdbcTemplates.put(key, new JdbcTemplate(dataSources.get(key)));
			}
		}
    }
    
            	
	/*
	 * (non-Javadoc)
	 * @see org.sentilo.agent.relational.business.repository.AgentRelationalRepository#save(org.sentilo.agent.relational.common.domain.Observation)
	 */
	public void save(Observation observation) {		
		JdbcTemplate jdbcTemplate = getJdbcTemplate(observation);
		if(jdbcTemplate!=null){
			jdbcTemplate.update(OBSERVATION_PS, observation.getProvider(), observation.getSensor(), observation.getValue(), observation.getTimestamp());
		}
	}

	/*
	 * (non-Javadoc)
	 * @see org.sentilo.agent.relational.business.repository.AgentRelationalRepository#save(org.sentilo.agent.relational.common.domain.Alarm)
	 */
	public void save(Alarm alarm) {
		JdbcTemplate jdbcTemplate = getJdbcTemplate(alarm);
		if(jdbcTemplate!=null){
			jdbcTemplate.update(ALARM_PS, alarm.getAlarm(), alarm.getMessage(), alarm.getTimestamp());
		}
	}

	/*
	 * (non-Javadoc)
	 * @see org.sentilo.agent.relational.business.repository.AgentRelationalRepository#save(org.sentilo.agent.relational.common.domain.Order)
	 */
	public void save(Order order) {
		JdbcTemplate jdbcTemplate = getJdbcTemplate(order);
		if(jdbcTemplate!=null){
			jdbcTemplate.update(ORDER_PS, order.getProvider(), order.getSensor(), order.getMessage(), order.getTimestamp());
		}
	}
	
	private JdbcTemplate getJdbcTemplate(Data data){
		JdbcTemplate jdbcTemplate = null;
		logger.debug("Will retrieve jdbcTemplate for targetDs {}:", data.getTargetDs());
		if(StringUtils.hasText(data.getTargetDs()) && !CollectionUtils.isEmpty(jdbcTemplates)){
			logger.debug("Retrieving jdbcTemplate associated to dataSource : {}", data.getTargetDs());
			jdbcTemplate =  jdbcTemplates.get(data.getTargetDs());
		}else{
			logger.warn("Not found jdbcTemplate for this targetDs {}. Data will not be persist.");
		}
		
		return jdbcTemplate;
	}					
}
