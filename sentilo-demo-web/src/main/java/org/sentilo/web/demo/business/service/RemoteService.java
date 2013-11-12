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
package org.sentilo.web.demo.business.service;

import java.util.List;

import org.sentilo.web.demo.common.domain.Alarm;
import org.sentilo.web.demo.common.domain.ExternalSensor;
import org.sentilo.web.demo.common.domain.ObservationData;
import org.sentilo.web.demo.common.domain.Order;
import org.sentilo.web.demo.common.domain.VirtualSensor;



/**
 * Servei d'accés als serveis remots de catàleg i de dades
 */
public interface RemoteService {
	
	/**
	 * Registra un sensor virtual al catàleg
	 * 
	 * @param sensor
	 * 		El sensor a registrar
	 */
	void createSensor(VirtualSensor sensor);

	/**
	 * Envia dades del sensor virtual (registrat o no)
	 * 
	 * @param observation
	 * 		conté la dada a enviar.
	 */
	void sendData(ObservationData observation);

	/**
	 * Recupera la darrara dada d'un sensor
	 * 
	 * @param externalsensor
	 * 		objecte que representa al sensor
	 * @return la dada recuperada
	 */
	List<ObservationData> recoverData(ExternalSensor externalsensor);

	/**
	 * Es subscriu a un sensor
	 * 
	 * @param externalsensor
	 * 	 	objecte que representa al sensor
	 */
	void subscribe(ExternalSensor externalsensor);

	/**
	 * Publica una alarma a la plataforma
	 * 
	 * @param alarm
	 * 		alarma a publicar
	 */
	void publishAlarm(Alarm alarm);

	/**
	 * Publica una ordre a la plataforma
	 * 
	 * @param order
	 * 		ordre a publicar
	 */
	void publishOrder(Order order);
	
	/**
	 * Es subscriu a Alarmes
	 * 
	 * @param alarm
	 * 		tipus d'alarma a subscirure's
	 */
	void subscribeAlarm(Alarm alarm);

	/**
	 * Es subscriu a ordres
	 * 
	 * @param order
	 * 		tipus d'ordre a subscirure's
	 */
	void subscribeOrder(Order order);

	/**
	 * Elimina la subscripció d'un sensor
	 * 
	 * @param observation
	 * 		Una dada del sensor a desubscriure's
	 */
	void unsubscribe(ObservationData observation);
	
	/**
	 * Elimina la subscripció d'una alarma
	 * 
	 * @param alarm
	 * 		L'alarma a desubscriure's
	 */
	void unsubscribe(Alarm alarm);
	
	/**
	 * Elimina la subscripció d'una ordre
	 * 
	 * @param order
	 * 		L'ordre a desubscriure's
	 */
	void unsubscribe(Order order);

	String getWsUrl();

}
