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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import org.sentilo.common.domain.OrderMessage;
import org.sentilo.common.domain.QueryFilterParams;
import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.platform.client.core.PlatformTemplate;
import org.sentilo.platform.client.core.domain.AlarmInputMessage;
import org.sentilo.platform.client.core.domain.AlarmMessage;
import org.sentilo.platform.client.core.domain.AlarmsOutputMessage;
import org.sentilo.platform.client.core.domain.DataInputMessage;
import org.sentilo.platform.client.core.domain.Observation;
import org.sentilo.platform.client.core.domain.ObservationsOutputMessage;
import org.sentilo.platform.client.core.domain.OrderInputMessage;
import org.sentilo.platform.client.core.domain.OrdersOutputMessage;
import org.sentilo.web.catalog.domain.Alarm;
import org.sentilo.web.catalog.domain.Sensor;
import org.sentilo.web.catalog.exception.builder.CompoundDuplicateKeyExceptionBuilder;
import org.sentilo.web.catalog.repository.SensorRepository;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.service.AlarmService;
import org.sentilo.web.catalog.service.SensorService;
import org.sentilo.web.catalog.validator.DefaultEntityKeyValidatorImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.data.mongodb.core.query.Update;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;


@Service
public class SensorServiceImpl extends AbstractBaseServiceImpl<Sensor> implements SensorService {

	@Autowired
	private SensorRepository repository;	

	@Autowired
	private PlatformTemplate platformTemplate;
			
	@Autowired
	private AlarmService alarmService;
	
	public SensorServiceImpl() {
		super(Sensor.class);
	}

	/*
	 * (non-Javadoc)
	 * @see org.sentilo.web.catalog.service.impl.AbstractBaseServiceImpl#doAfterInit()
	 */
	protected void doAfterInit() {
		this.entityKeyValidator = new DefaultEntityKeyValidatorImpl(getRepository(), new CompoundDuplicateKeyExceptionBuilder("error.sensor.duplicate.key"));
		super.doAfterInit();
	}

	/*
	 * (non-Javadoc)
	 * @see org.sentilo.web.catalog.service.impl.AbstractBaseServiceImpl#getRepository()
	 */
	public SensorRepository getRepository() {
		return repository;
	}

	/*
	 * (non-Javadoc)
	 * @see org.sentilo.web.catalog.service.impl.AbstractBaseServiceImpl#getEntityId(org.sentilo.web.catalog.domain.CatalogDocument)
	 */
	public String getEntityId(Sensor entity) {
		return entity.getId();
	}	
			

	/*
	 * (non-Javadoc)
	 * @see org.sentilo.web.catalog.service.SensorService#getLastObservations(org.sentilo.web.catalog.domain.Sensor)
	 */
	public List<Observation> getLastObservations(Sensor sensor) {
		DataInputMessage message = new DataInputMessage(sensor.getProviderId(), sensor.getSensorId(), new QueryFilterParams(SentiloConstants.NUM_MAXIM_ELEMENTS));
		ObservationsOutputMessage outMessage = platformTemplate.getDataOps().getLastObservations(message);
		return outMessage.getObservations();
		//TODO Mikel: falta transformar la excepcion RESTClientException en una propia del catalogo.		
	}

	/*
	 * (non-Javadoc)
	 * @see org.sentilo.web.catalog.service.SensorService#getLastObservation(org.sentilo.web.catalog.domain.Sensor)
	 */
	public Observation getLastObservation(Sensor sensor) {
		DataInputMessage message = new DataInputMessage(sensor.getProviderId(), sensor.getSensorId());
		ObservationsOutputMessage outMessage = platformTemplate.getDataOps().getLastObservations(message);
		//Observations observations = PlatformMessageConverter.convertPlatformObservations(outMessage);
		return (CollectionUtils.isEmpty(outMessage.getObservations()) ? null : outMessage.getObservations().get(0));
		//TODO Mikel: falta transformar la excepcion RESTClientException en una propia del catalogo.
	}
	
	/*
	 * (non-Javadoc)
	 * @see org.sentilo.web.catalog.service.SensorService#updateMulti(java.util.Collection, java.lang.String, java.lang.Object)
	 */
	public void updateMulti(Collection<String> sensorIds, String param, Object value){		
		Update update = Update.update(param, value);
		getMongoOps().updateMulti(buildQueryForIdInCollection(sensorIds), update, Sensor.class);
	}
	
	/*
	 * (non-Javadoc)
	 * @see org.sentilo.web.catalog.service.SensorService#getLastAlarmsMessages(org.sentilo.web.catalog.domain.Sensor)
	 */
	public List<AlarmMessage> getLastAlarmsMessages(Sensor sensor) {
		// Para recuperar las últimos alarmas asociadas a las alertas de un sensor, primero debemos recuperar las alertas  del sensor.
		// y despues, para cada alerta, recuperar sus ultimas alarmas.
		List<AlarmMessage> lastAlarmMessages = new ArrayList<AlarmMessage>();
		
		List<Alarm> sensorAlarms = getSensorAlerts(sensor);
		
		for(Alarm alarm: sensorAlarms){
			AlarmInputMessage message = new AlarmInputMessage(alarm.getId(), new QueryFilterParams(SentiloConstants.NUM_MAXIM_ELEMENTS));
			AlarmsOutputMessage outMessage = platformTemplate.getAlarmOps().getLastAlarmMessages(message);
			if(!CollectionUtils.isEmpty(outMessage.getMessages())){
				lastAlarmMessages.addAll(outMessage.getMessages());
			}
		}
				
		//TODO Mikel: falta transformar la excepcion RESTClientException en una propia del catalogo.		
		
		return lastAlarmMessages;
	}
	
		
	/*
	 * (non-Javadoc)
	 * @see org.sentilo.web.catalog.service.SensorService#getLastOrderMessages(org.sentilo.web.catalog.domain.Sensor)
	 */
	public List<OrderMessage> getLastOrderMessages(Sensor sensor) {
		OrderInputMessage message = new OrderInputMessage(sensor.getProviderId(), sensor.getSensorId(), new QueryFilterParams(SentiloConstants.NUM_MAXIM_ELEMENTS));
		OrdersOutputMessage outMessage = platformTemplate.getOrderOps().getLastOrders(message);
		return outMessage.getOrders();
		//TODO Mikel: falta transformar la excepcion RESTClientException en una propia del catalogo.		
	}
	
	/*
	 * (non-Javadoc)
	 * @see org.sentilo.web.catalog.service.SensorService#deleteSensors(java.lang.String[])
	 */
	public void deleteSensors(String[] sensorsNames){
		//Al borrar sensores, tambien hay que eliminar las alertas que tienen  asociadas.		
		List<String> values = Arrays.asList(sensorsNames);
		Query sensorIdFilter = buildQueryForParamInCollection("sensorId", values);
		getMongoOps().remove(sensorIdFilter, Alarm.class);
		getMongoOps().remove(sensorIdFilter, Sensor.class);		
	}
	
	/*
	 * (non-Javadoc)
	 * @see org.sentilo.web.catalog.service.SensorService#deleteSensorsFromComponents(java.util.List)
	 */
	public void deleteSensorsFromComponents(List<String> componentsIds){		
		//Al borrar sensores, tambien hay que eliminar las alertas que tienen  asociadas.
		Query componentIdFilter = buildQueryForParamInCollection("componentId", componentsIds);
		getMongoOps().remove(componentIdFilter, Alarm.class);
		getMongoOps().remove(componentIdFilter, Sensor.class);
	}
			
	private List<Alarm> getSensorAlerts(Sensor sensor){
		SearchFilter sensorAlertsFilter = new SearchFilter();		
		sensorAlertsFilter.addAndParam("sensorId", sensor.getSensorId());
		return alarmService.search(sensorAlertsFilter).getContent();				
	}
		

}


