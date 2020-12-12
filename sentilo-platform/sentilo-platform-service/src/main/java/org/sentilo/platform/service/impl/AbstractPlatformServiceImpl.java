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
package org.sentilo.platform.service.impl;

import org.sentilo.platform.service.dao.SentiloKeysBuilder;
import org.sentilo.platform.service.dao.SentiloRedisTemplate;
import org.sentilo.platform.service.dao.SentiloSequenceUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.util.StringUtils;

public abstract class AbstractPlatformServiceImpl {

  protected static final String SID = "sid";
  protected static final String DATA = "data";
  protected static final String LOCATION = "location";
  protected static final String TIMESTAMP = "ts";
  protected static final String MESSAGE = "message";
  protected static final String SENDER = "sender";
  protected static final String ORDER = "order";
  protected static final String PROVIDER = "provider";
  protected static final String SENSOR = "sensor";
  protected static final String ALERT_TYPE = "alertType";
  protected static final String STATE = "state";
  protected static final String TTL = "ttl";
  protected static final String AID = "aid";
  protected static final String ALERT = "alert";
  protected static final String ENTITY = "entity";
  protected static final String ACTIVE = "active";

  protected SentiloKeysBuilder keysBuilder = new SentiloKeysBuilder();

  @Autowired
  protected SentiloSequenceUtils sequenceUtils;

  @Autowired
  protected SentiloRedisTemplate sRedisTemplate;

  @Value("${redis.expire.data.seconds}")
  protected int expireSeconds;

  public SentiloKeysBuilder getKeysBuilder() {
    return keysBuilder;
  }

  protected Integer ttlToExpiredTime(final String redisSensorSecondsTtl) {
    return StringUtils.hasText(redisSensorSecondsTtl) ? getExpiredTime(Integer.valueOf(redisSensorSecondsTtl)) : expireSeconds;
  }

  protected String catalogSensorTtlToRedisTtl(final Integer catalogSensorTtl) {
    final Integer redisSensorTtl = catalogSensorTtl != null ? getExpiredTime(catalogSensorTtl) : expireSeconds;
    return Integer.toString(redisSensorTtl);
  }

  private Integer getExpiredTime(final Integer sensorSecondsTtl) {
    return sensorSecondsTtl != null ? sensorSecondsTtl : expireSeconds;
  }

}
