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
package org.sentilo.web.demo.business.service.impl;

import java.util.ArrayList;
import java.util.List;

import org.sentilo.common.domain.CatalogSensor;
import org.sentilo.common.domain.OrderMessage;
import org.sentilo.common.domain.QueryFilterParams;
import org.sentilo.common.domain.SubscribeType;
import org.sentilo.platform.client.core.PlatformTemplate;
import org.sentilo.platform.client.core.domain.AlarmInputMessage;
import org.sentilo.platform.client.core.domain.CatalogInputMessage;
import org.sentilo.platform.client.core.domain.DataInputMessage;
import org.sentilo.platform.client.core.domain.Endpoint;
import org.sentilo.platform.client.core.domain.ObservationsOutputMessage;
import org.sentilo.platform.client.core.domain.OrderInputMessage;
import org.sentilo.platform.client.core.domain.SubscribeInputMessage;
import org.sentilo.platform.client.core.domain.factory.SubscribeInputMessageFactory;
import org.sentilo.web.demo.business.service.RemoteService;
import org.sentilo.web.demo.common.domain.Alarm;
import org.sentilo.web.demo.common.domain.ExternalSensor;
import org.sentilo.web.demo.common.domain.ObservationData;
import org.sentilo.web.demo.common.domain.Order;
import org.sentilo.web.demo.common.domain.VirtualSensor;
import org.sentilo.web.demo.common.utils.Constants;
import org.sentilo.web.demo.common.utils.ConvertUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;


@Service
public class RemoteServiceImpl implements RemoteService{
	
	@Autowired
	private PlatformTemplate platformTemplate;
	
	@Value("${publish.client.endpoint}")
	private String endpointUrl;
	
	@Value("${ws.client.adress}")
	private String wsUrl;


	@Override
	public void createSensor(VirtualSensor virtualSensor){
		List<CatalogSensor> sensors = new ArrayList<CatalogSensor>();
		CatalogSensor sensor = new CatalogSensor();	
		sensor.setSensor(virtualSensor.getSensorId());
		sensor.setProvider(virtualSensor.getProviderId());
		sensor.setComponent(virtualSensor.getComponentId());
		sensor.setDescription(virtualSensor.getDescription());
		sensor.setType(virtualSensor.getType().toString());
		sensor.setDataType(virtualSensor.getDataType().toString());
		sensor.setUnit(virtualSensor.getUnit());		
		sensor.setLocation(virtualSensor.getLocation());
		sensor.setComponentType(virtualSensor.getComponentType().toString());
		
		sensors.add(sensor);
		CatalogInputMessage message = new CatalogInputMessage(virtualSensor.getProviderId(), sensors);
		message.setIdentityToken(virtualSensor.getTokenAuth());
		platformTemplate.getCatalogOps().registerSensors(message);
	}

	@Override
	public void sendData(ObservationData observation){
		DataInputMessage message = new DataInputMessage(observation.getProviderId(), observation.getSensorId(), observation.getValue()); 
		message.setIdentityToken(observation.getTokenAuth());
		platformTemplate.getDataOps().sendObservations(message);
	}
	
	@Override
	public List<ObservationData> recoverData(ExternalSensor externalsensor){
		DataInputMessage message = new DataInputMessage(externalsensor.getProviderId(), externalsensor.getId(), new QueryFilterParams(new Integer(externalsensor.getNumber())));
		message.setIdentityToken(externalsensor.getTokenAuth());
		ObservationsOutputMessage obsMessage = platformTemplate.getDataOps().getLastObservations(message);
		List<ObservationData> observations =  ConvertUtils.convert(externalsensor.getProviderId(), externalsensor.getId(),obsMessage);
		return observations;
	}

	@Override
	public void publishAlarm(Alarm alarm) {
		AlarmInputMessage message = new AlarmInputMessage(alarm.getAlarmId(), alarm.getMessage());
		message.setIdentityToken(alarm.getTokenAuth());
		platformTemplate.getAlarmOps().publish(message);	
	}

	@Override
	public void publishOrder(Order order) {
		OrderInputMessage message = new OrderInputMessage(order.getProviderId(), new OrderMessage(order.getMessage()));
		message.setIdentityToken(order.getTokenAuth());
		platformTemplate.getOrderOps().publish(message);
	}
	
	@Override
	public void subscribe(ExternalSensor externalsensor) {
		SubscribeInputMessage message = SubscribeInputMessageFactory.buildSubscription(SubscribeType.DATA, 
				new Endpoint(endpointUrl+Constants.ENDPOINT_SENSOR), externalsensor.getProviderId(), externalsensor.getId());
		message.setIdentityToken(externalsensor.getTokenAuth());
		platformTemplate.getSubscribeOps().subscribe(message);
	}

	@Override
	public void subscribeAlarm(Alarm alarm) {
		SubscribeInputMessage message = SubscribeInputMessageFactory.buildSubscription(SubscribeType.ALARM, 
				new Endpoint(endpointUrl+Constants.ENDPOINT_ALARM), alarm.getId());
		message.setIdentityToken(alarm.getTokenAuth());
		platformTemplate.getSubscribeOps().subscribe(message);
	}

	@Override
	public void subscribeOrder(Order order) {
		SubscribeInputMessage message = SubscribeInputMessageFactory.buildSubscription(SubscribeType.ORDER, 
				new Endpoint(endpointUrl+Constants.ENDPOINT_ORDER), order.getProviderId(), order.getId());
		message.setIdentityToken(order.getTokenAuth());
		platformTemplate.getSubscribeOps().subscribe(message);
	}
	
	@Override
	public void unsubscribe(ObservationData observation) {
		SubscribeInputMessage message = SubscribeInputMessageFactory.buildSubscription(SubscribeType.DATA, observation.getProviderId(), observation.getSensorId());
		platformTemplate.getSubscribeOps().remove(message);
		
	}

	@Override
	public void unsubscribe(Alarm alarm) {
		SubscribeInputMessage message = SubscribeInputMessageFactory.buildSubscription(SubscribeType.ALARM, alarm.getId());
		platformTemplate.getSubscribeOps().remove(message);
	}

	@Override
	public void unsubscribe(Order order) {
		SubscribeInputMessage message =  SubscribeInputMessageFactory.buildSubscription(SubscribeType.ORDER, order.getProviderId(), order.getId());
		platformTemplate.getSubscribeOps().remove(message);
		
	}

	@Override
	public String getWsUrl() {
		return wsUrl;
	}	

}
