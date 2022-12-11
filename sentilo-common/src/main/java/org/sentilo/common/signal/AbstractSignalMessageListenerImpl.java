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

import org.sentilo.common.converter.DefaultStringMessageConverter;
import org.sentilo.common.converter.StringMessageConverter;
import org.sentilo.common.domain.SignalMessage;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.redis.connection.Message;
import org.springframework.data.redis.serializer.RedisSerializer;
import org.springframework.data.redis.serializer.StringRedisSerializer;

public abstract class AbstractSignalMessageListenerImpl implements SignalMessageListener {

  private static final Logger LOGGER = LoggerFactory.getLogger(SignalMessageListener.class);
  private final StringMessageConverter signalConverter = new DefaultStringMessageConverter();
  private final RedisSerializer<String> serializer = new StringRedisSerializer();

  @Override
  public void onMessage(final Message message, final byte[] pattern) {
    final String info = getInfo(message);
    final String channel = getChannel(message);

    LOGGER.debug("Get signal on channel {}", channel);
    LOGGER.debug("Signal message {}", info);

    final SignalMessage sMessage = unmarshal(info);
    doWithMessage(sMessage);
  }

  /**
   * Every implementation of this class must override this method to implements business logic
   *
   * @param eventMessage
   */
  public abstract void doWithMessage(final SignalMessage eventMessage);

  private String getInfo(final Message message) {
    return serializer.deserialize(message.getBody());
  }

  private String getChannel(final Message message) {
    return serializer.deserialize(message.getChannel());
  }

  private SignalMessage unmarshal(final String message) {
    return signalConverter.unmarshal(message, SignalMessage.class);
  }

}
