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
package org.sentilo.platform.service.listener;

import org.sentilo.common.domain.NotificationMessage;
import org.sentilo.common.parser.NotificationMessageConverter;
import org.sentilo.common.rest.RESTClient;
import org.sentilo.common.rest.impl.RESTClientImpl;
import org.sentilo.platform.service.utils.ChannelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class NotificationSender {
	private final Logger logger = LoggerFactory.getLogger(NotificationSender.class);
	
	private RESTClient restClient = new RESTClientImpl();
	private NotificationMessageConverter converter = new NotificationMessageConverter();
	
	public NotificationSender(){
		try{
			((RESTClientImpl)restClient).afterPropertiesSet();
		}catch(Exception e){
			
		}
	}
	
	public void sendNotification(String endpoint, String channel, String info){
		NotificationMessage notification = new NotificationMessage(info,ChannelUtils.buildSubscriptionPathFromTopic(channel));						
		((RESTClientImpl)restClient).setHost(endpoint);			
		String body = converter.marshall(notification);
		logger.debug("Enviando notificacion {} a endpoint {}", body, endpoint);
		restClient.post("", body);
	}
}
