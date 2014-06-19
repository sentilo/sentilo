/*
 * Sentilo
 * 
 * Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de Barcelona.
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
package org.sentilo.platform.client.core.service.impl;

import org.sentilo.platform.client.core.domain.AlarmInputMessage;
import org.sentilo.platform.client.core.domain.AlarmsOutputMessage;
import org.sentilo.platform.client.core.parser.AlarmMessageConverter;
import org.sentilo.platform.client.core.service.AlarmServiceOperations;
import org.sentilo.platform.client.core.utils.RequestUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

@Service
public class DefaultAlarmServiceOperationsImpl extends AbstractServiceOperationsImpl implements AlarmServiceOperations {

  private final Logger logger = LoggerFactory.getLogger(DefaultAlarmServiceOperationsImpl.class);

  private AlarmMessageConverter converter = new AlarmMessageConverter();

  /*
   * (non-Javadoc)
   * 
   * @see
   * org.sentilo.platform.client.core.service.AlarmServiceOperations#publish(org.sentilo.platform
   * .client.core.domain.AlarmInputMessage)
   */
  @Override
  public void publish(final AlarmInputMessage message) {
    logger.debug("Publishing alarm message {}", message);
    getRestClient().put(RequestUtils.buildPath(message), converter.marshall(message), message.getIdentityToken());
    logger.debug("alarm published ");
  }

  /*
   * (non-Javadoc)
   * 
   * @see
   * org.sentilo.platform.client.core.service.AlarmServiceOperations#getLastAlarmMessages(org.sentilo
   * .platform.client.core.domain.AlarmInputMessage)
   */
  @Override
  public AlarmsOutputMessage getLastAlarmMessages(final AlarmInputMessage message) {
    logger.debug("Retrieving last alarm messages  {}", message);
    final String response = getRestClient().get(RequestUtils.buildPath(message), RequestUtils.buildParameters(message), message.getIdentityToken());
    logger.debug("Retrieved last alarm messages");
    return converter.unmarshall(response);
  }

}
