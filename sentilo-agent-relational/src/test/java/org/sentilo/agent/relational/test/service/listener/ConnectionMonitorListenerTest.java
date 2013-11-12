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
package org.sentilo.agent.relational.test.service.listener;

import static org.mockito.Matchers.any;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.agent.relational.listener.ConnectionMonitorListener;
import org.sentilo.agent.relational.listener.MessageListenerImpl;
import org.springframework.data.redis.RedisConnectionFailureException;
import org.springframework.data.redis.listener.RedisMessageListenerContainer;
import org.springframework.data.redis.listener.Topic;
import org.springframework.util.ReflectionUtils;

import redis.clients.jedis.exceptions.JedisConnectionException;

public class ConnectionMonitorListenerTest {
	
	private ConnectionMonitorListener connectionMonitorListener;
	
	// Como esta clase tiene metodos finales que queremos validar, usamos la instancia real en vez del mock
	// http://mockito.googlecode.com/svn/branches/1.7/javadoc/org/mockito/Mockito.html#spy%28T%29
	@Mock private RedisMessageListenerContainer listenerContainer;
	
	@Before
	public void setUp() {
		MockitoAnnotations.initMocks(this);
		
		connectionMonitorListener = new ConnectionMonitorListener();
		connectionMonitorListener.setListenerContainer(listenerContainer);
	}		
			
	@Test
	public void start() throws Exception{
		Field initialized = RedisMessageListenerContainer.class.getDeclaredField("initialized");		
		ReflectionUtils.makeAccessible(initialized);		
		initialized.set(listenerContainer, true);
		
		when(listenerContainer.isRunning()).thenReturn(true);
						       
		connectionMonitorListener.start();
								
		verify(listenerContainer).isRunning();		
		Assert.assertTrue(connectionMonitorListener.isRunning());
	}
	
	@Test
	public void startAndNoRunning(){		
		connectionMonitorListener.start();
				
		verify(listenerContainer, times(0)).isRunning();		
		Assert.assertFalse(connectionMonitorListener.isRunning());
	}
	
	@Test
	public void validateConnectionWhenNoRunning() throws Exception{		
		connectionMonitorListener.validateConnection();
		
		Assert.assertFalse(connectionMonitorListener.isRunning());	
	}
	
	@Test
	public void validateConnection() throws Exception{				
		Field initialized = RedisMessageListenerContainer.class.getDeclaredField("initialized");				
		ReflectionUtils.makeAccessible(initialized);		
		initialized.set(listenerContainer, true);
				
		when(listenerContainer.isRunning()).thenReturn(true);
		
		connectionMonitorListener.validateConnection();
		
		verify(listenerContainer).isRunning();	
		verify(listenerContainer).addMessageListener(any(MessageListenerImpl.class), any(Topic.class));
		verify(listenerContainer).removeMessageListener(any(MessageListenerImpl.class), any(Topic.class));		
	}
	
	@Test
	public void validateConnectionWithRedisConnectionFailureException() throws Exception{				
		Field initialized = RedisMessageListenerContainer.class.getDeclaredField("initialized");				
		ReflectionUtils.makeAccessible(initialized);		
		initialized.set(listenerContainer, true);
				
		when(listenerContainer.isRunning()).thenReturn(true);
		doThrow(RedisConnectionFailureException.class).when(listenerContainer).addMessageListener(any(MessageListenerImpl.class), any(Topic.class));
		
		connectionMonitorListener.validateConnection();
		
		verify(listenerContainer,times(2)).isRunning();	
		verify(listenerContainer).addMessageListener(any(MessageListenerImpl.class), any(Topic.class));
		verify(listenerContainer).stop();
			
	}
	
	@Test
	public void validateConnectionWithJedisConnectionException() throws Exception{				
		Field initialized = RedisMessageListenerContainer.class.getDeclaredField("initialized");				
		ReflectionUtils.makeAccessible(initialized);		
		initialized.set(listenerContainer, true);
				
		when(listenerContainer.isRunning()).thenReturn(true);
		doThrow(JedisConnectionException.class).when(listenerContainer).addMessageListener(any(MessageListenerImpl.class), any(Topic.class));
		
		connectionMonitorListener.validateConnection();
		
		verify(listenerContainer,times(2)).isRunning();	
		verify(listenerContainer).addMessageListener(any(MessageListenerImpl.class), any(Topic.class));
		verify(listenerContainer).stop();
			
	}
	
	@Test
	public void validateConnectionWithException() throws Exception{				
		Field initialized = RedisMessageListenerContainer.class.getDeclaredField("initialized");				
		ReflectionUtils.makeAccessible(initialized);		
		initialized.set(listenerContainer, true);
				
		when(listenerContainer.isRunning()).thenReturn(true);
		doThrow(Exception.class).when(listenerContainer).addMessageListener(any(MessageListenerImpl.class), any(Topic.class));
		
		connectionMonitorListener.validateConnection();
				
		verify(listenerContainer,times(0)).stop();		
	}
	
}
