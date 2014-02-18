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
package org.sentilo.agent.relational.business.service.impl;

import org.sentilo.agent.relational.business.repository.AgentRelationalRepository;
import org.sentilo.agent.relational.business.service.DataTrackService;
import org.sentilo.agent.relational.common.domain.Alarm;
import org.sentilo.agent.relational.common.domain.Observation;
import org.sentilo.agent.relational.common.domain.Order;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
public class DataTrackServiceImpl implements DataTrackService {

  @Autowired
  private AgentRelationalRepository agentRelationalDao;

  /*
   * (non-Javadoc)
   * 
   * @see
   * org.sentilo.agent.relational.business.service.DataTrackService#save(org.sentilo.agent.relational
   * .common.domain.Order)
   */
  @Transactional
  public void save(final Order order) {
    agentRelationalDao.save(order);
  }

  /*
   * (non-Javadoc)
   * 
   * @see
   * org.sentilo.agent.relational.business.service.DataTrackService#save(org.sentilo.agent.relational
   * .common.domain.Alarm)
   */
  @Transactional
  public void save(final Alarm alarm) {
    agentRelationalDao.save(alarm);
  }

  /*
   * (non-Javadoc)
   * 
   * @see
   * org.sentilo.agent.relational.business.service.DataTrackService#save(org.sentilo.agent.relational
   * .common.domain.Observation)
   */
  @Transactional
  public void save(final Observation observation) {
    agentRelationalDao.save(observation);
  }

  public void setAgentRelationalDao(final AgentRelationalRepository agentRelationalDao) {
    this.agentRelationalDao = agentRelationalDao;
  }

}
