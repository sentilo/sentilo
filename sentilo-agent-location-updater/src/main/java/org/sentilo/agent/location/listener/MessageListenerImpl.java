/*
 * Sentilo
 *
 * Original version 1.4 Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de
 * Barcelona. Modified by Opentrends adding support for multitenant deployments and SaaS.
 * Modifications on version 1.5 Copyright (C) 2015 Opentrends Solucions i Sistemes, S.L.
 *
 *
 * This program is licensed and may be used, modified and redistributed under the terms of the
 * European Public License (EUPL), either version 1.1 or (at your option) any later version as soon
 * as they are approved by the European Commission.
 *
 * Alternatively, you may redistribute and/or modify this program under the terms of the GNU Lesser
 * General Public License as published by the Free Software Foundation; either version 3 of the
 * License, or (at your option) any later version.
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied.
 *
 * See the licenses for the specific language governing permissions, limitations and more details.
 *
 * You should have received a copy of the EUPL1.1 and the LGPLv3 licenses along with this program;
 * if not, you may find them at:
 *
 * https://joinup.ec.europa.eu/software/page/eupl/licence-eupl http://www.gnu.org/licenses/ and
 * https://www.gnu.org/licenses/lgpl.txt
 */
package org.sentilo.agent.location.listener;

import org.sentilo.agent.common.listener.AbstractMessageListenerImpl;
import org.sentilo.agent.location.batch.AsyncComponentLocationUpdater;
import org.sentilo.common.domain.EventMessage;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

@Component
public class MessageListenerImpl extends AbstractMessageListenerImpl {

  private static final Logger LOGGER = LoggerFactory.getLogger(MessageListenerImpl.class);

  @Autowired
  private AsyncComponentLocationUpdater asyncComponentLocationUpdater;

  public MessageListenerImpl() {
    this("location listener");
  }

  public MessageListenerImpl(final String name) {
    super(name);
  }

  @Override
  public void doWithMessage(final EventMessage eventMessage) {
    try {
      if (eventCouldBeProcessed(eventMessage)) {
        asyncComponentLocationUpdater.add(eventMessage);
      }
    } catch (final Exception e) {
      LOGGER.error("Error processing message {} ", eventMessage, e);
    }
  }

  private boolean eventCouldBeProcessed(final EventMessage eventMessage) {
    // A message could be processed only if it has filled in the following fields: provider, sensor,
    // timestamp and location
    return StringUtils.hasText(eventMessage.getProvider()) && StringUtils.hasText(eventMessage.getSensor())
        && StringUtils.hasText(eventMessage.getLocation()) && StringUtils.hasText(eventMessage.getTimestamp());
  }

}
