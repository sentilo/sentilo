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
package org.sentilo.agent.alert.test.listener;

import static org.mockito.Mockito.when;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.times;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.agent.alert.domain.Alarm;
import org.sentilo.agent.alert.listener.MessageListenerImpl;
import org.sentilo.agent.alert.utils.enums.AlarmTriggerType;
import org.springframework.data.redis.connection.Message;
import org.springframework.data.redis.core.RedisTemplate;


public class MessageListenerTest {

	private MessageListenerImpl messageListener;
	
	@Mock private RedisTemplate<String, String> redisTemplate;
	@Mock private Message message;
	
	@Before
	public void setUp() throws Exception{						
		MockitoAnnotations.initMocks(this);
		messageListener = new MessageListenerImpl("test");	
		messageListener.setRedisTemplate(redisTemplate);
		
		Alarm alarm1 = new Alarm("alarmGT");
		alarm1.setTrigger(AlarmTriggerType.GT);
		alarm1.setExpression("20");
		
		Alarm alarm2 = new Alarm("alarmDELTA");
		alarm2.setTrigger(AlarmTriggerType.CHANGE_DELTA);
		alarm2.setExpression("40");
						
		messageListener.addAlarm(alarm1);
		messageListener.addAlarm(alarm2);
	}
	
	@Test
	public void publishAlarm(){
		String info = "25/07/2013T13:41:59#@#21";
		String channel ="data:provider1:sensor1";
		when(message.getBody()).thenReturn(info.getBytes());
		when(message.getChannel()).thenReturn(channel.getBytes());
		
		messageListener.onMessage(message, null);
		
		verify(redisTemplate).convertAndSend(anyString(), anyString());				
	}
	
	@Test
	public void publishTwoAlarms(){						
		String info = "25/07/2013T13:41:59#@#36";
		String channel ="data:provider1:sensor1";
		
		Alarm alarm3 = new Alarm("alarmLT");
		alarm3.setTrigger(AlarmTriggerType.LT);
		alarm3.setExpression("40");
						
		messageListener.addAlarm(alarm3);
		
		
		when(message.getBody()).thenReturn(info.getBytes());
		when(message.getChannel()).thenReturn(channel.getBytes());
		
		messageListener.onMessage(message, null);
		
		verify(redisTemplate,times(2)).convertAndSend(anyString(), anyString());				
	}
	
}
