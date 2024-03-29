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
package org.sentilo.common.signal;

import org.sentilo.common.domain.SignalMessage;
import org.sentilo.common.enums.SignalType;
import org.sentilo.common.utils.MessagingUtils;
import org.sentilo.common.utils.ModuleUtils;
import org.sentilo.common.utils.SentiloConstants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.data.redis.listener.ChannelTopic;
import org.springframework.data.redis.listener.Topic;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;

@Component
public class DefaultPublishSignalServiceImpl implements PublishSignalService {

  private static final Logger LOGGER = LoggerFactory.getLogger(PublishSignalService.class);

  @Autowired
  private StringRedisTemplate redisTemplate;

  @Override
  public void publishInternalSignal(final SignalType signalType) {
    publishInternalSignal(signalType, ModuleUtils.getModuleName());
  }

  @Override
  @Async
  public void publishInternalSignal(final SignalType signalType, final String sender) {
    LOGGER.debug("Publish new signal event [{}] from [{}]", signalType.name(), sender);
    final SignalMessage message = new SignalMessage(System.currentTimeMillis(), signalType, sender);

    final Topic topic = new ChannelTopic(SentiloConstants.INTERNAL_SIGNALS_TOPIC);
    final String sMessage = MessagingUtils.marshal(message);
    redisTemplate.convertAndSend(topic.getTopic(), sMessage);
  }

}
