/*
 * Sentilo
 *  
 * Original version 1.4 Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de
 * Barcelona. Modified by Opentrends adding support for multitenant deployments and SaaS. 
 * Modifications on version 1.5 Copyright (C) 2015 Opentrends Solucions i Sistemes, S.L.
 * 
 *   
 * This program is licensed and may be used, modified and redistributed under the terms  of the 
 * European Public License (EUPL), either version 1.1 or (at your option) any later version as soon 
 * as they are approved by the European Commission.
 *   
 * Alternatively, you may redistribute and/or modify this program under the terms of the GNU Lesser 
 * General Public License as published by the Free Software Foundation; either  version 3 of the 
 * License, or (at your option) any later version. 
 *   
 * Unless required by applicable law or agreed to in writing, software distributed under the License 
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR  CONDITIONS OF ANY KIND, either express 
 * or implied. 
 *   
 * See the licenses for the specific language governing permissions, limitations and more details.
 *   
 * You should have received a copy of the EUPL1.1 and the LGPLv3 licenses along with this program; 
 * if not, you may find them at: 
 *   
 * https://joinup.ec.europa.eu/software/page/eupl/licence-eupl http://www.gnu.org/licenses/   and 
 * https://www.gnu.org/licenses/lgpl.txt
 */
package org.sentilo.platform.service.messaging;

import java.util.Collection;

import org.sentilo.platform.common.event.InputMessageEvent;
import org.springframework.context.SmartLifecycle;
import org.springframework.data.redis.listener.Topic;

/**
 * Internal abstraction used by the framework representing a message listener container. Not meant
 * to be implemented externally.
 *
 */
public interface MessageListenerContainer extends SmartLifecycle {

  void addMessageListener(final EventMessageListener eventMessageListener, final Topic topic);

  /**
   * Removes the given message listener completely (from all topics). After remove, the listener
   * stops receiving (matching) messages as soon as possible.
   *
   * @param eventMessageListener message listener
   */
  void removeMessageListener(final EventMessageListener eventMessageListener);

  /**
   * Removes a message listener from the given topic. After remove, the listener stops receiving
   * (matching) messages as soon as possible.
   *
   * @param eventMessageListener message listener
   * @param topic message topic
   */
  void removeMessageListener(final EventMessageListener eventMessageListener, final Topic topic);

  /**
   * Removes a message listener from the given topics. After remove, the listener stops receiving
   * (matching) messages as soon as possible.
   *
   * @param listener message listener
   * @param topics message listener topics
   */
  void removeMessageListener(final EventMessageListener eventMessageListener, final Collection<? extends Topic> topics);

  /**
   * Return whether this container is currently active, that is, whether it has been set up and
   * could process events.
   */
  boolean isActive();

  /**
   * Enable container to process input events
   */
  void enable();

  /**
   * Disable container to process input events. These events will be queued until it will be active
   */
  void disable();

  /**
   * Clear listeners and topic in-memory collections. Utility for reload subscriptions
   */
  void clear();

  void onApplicationEvent(final InputMessageEvent event);

}
