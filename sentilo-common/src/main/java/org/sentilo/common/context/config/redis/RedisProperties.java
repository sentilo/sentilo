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
package org.sentilo.common.context.config.redis;

import java.util.Properties;

import javax.annotation.PostConstruct;

import org.apache.commons.pool2.impl.GenericObjectPoolConfig;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;

@Component
public class RedisProperties {

  @Autowired
  @Qualifier("sentiloConfigProperties")
  private Properties sentiloConfigProperties;

  private static String DEFAULT_REDIS_PWD = "sentilo";
  private static String DEFAULT_REDIS_HOST = "127.0.0.1";

  private long expire = 0;
  private String password = DEFAULT_REDIS_PWD;
  private String host = DEFAULT_REDIS_HOST;
  private int port = 6379;
  private long connTimeout = -1;
  private int maxTotal = GenericObjectPoolConfig.DEFAULT_MAX_TOTAL;
  private int maxIdle = GenericObjectPoolConfig.DEFAULT_MAX_IDLE;
  private int minIdle = GenericObjectPoolConfig.DEFAULT_MIN_IDLE;
  private long maxWaitMillis = GenericObjectPoolConfig.DEFAULT_MAX_WAIT_MILLIS;
  private boolean testOnBorrow = GenericObjectPoolConfig.DEFAULT_TEST_ON_BORROW;
  private boolean testOnCreate = GenericObjectPoolConfig.DEFAULT_TEST_ON_CREATE;
  private boolean testOnReturn = GenericObjectPoolConfig.DEFAULT_TEST_ON_RETURN;
  private boolean testWhileIdle = GenericObjectPoolConfig.DEFAULT_TEST_WHILE_IDLE;

  // Cluster properties
  private String nodes;
  private int maxRedirects;

  @PostConstruct
  public void init() {
    expire = getValue(expire, "sentilo.redis.expire.key.seconds", Long.class);
    password = getValue(password, "sentilo.redis.password", String.class);
    host = getValue(host, "sentilo.redis.host", String.class);
    port = getValue(port, "sentilo.redis.port", Integer.class);
    nodes = getValue(nodes, "sentilo.redis.cluster.nodes", String.class);
    maxRedirects = getValue(maxRedirects, "sentilo.redis.cluster.maxRedirects", Integer.class);
    connTimeout = getValue(connTimeout, "sentilo.redis.connTimeout", Long.class);
    maxTotal = getValue(maxTotal, "sentilo.redis.client.maxTotal", Integer.class);
    maxIdle = getValue(maxIdle, "sentilo.redis.client.maxIdle", Integer.class);
    minIdle = getValue(minIdle, "sentilo.redis.client.minIdle", Integer.class);
    maxWaitMillis = getValue(maxWaitMillis, "sentilo.redis.client.maxWaitMillis", Long.class);
    testOnBorrow = getValue(testOnBorrow, "sentilo.redis.client.testOnBorrow", Boolean.class);
    testOnCreate = getValue(testOnCreate, "sentilo.redis.client.testOnCreate", Boolean.class);
    testOnReturn = getValue(testOnReturn, "sentilo.redis.client.testOnReturn", Boolean.class);
    testWhileIdle = getValue(testWhileIdle, "sentilo.redis.client.testWhileIdle", Boolean.class);
  }

  @SuppressWarnings("unchecked")
  private <T> T getValue(final T defaultValue, final String configKey, final Class<T> type) {
    T response = defaultValue;
    if (sentiloConfigProperties.containsKey(configKey)) {
      final String value = sentiloConfigProperties.getProperty(configKey);
      switch (type.getSimpleName()) {
        case "Integer":
          response = (T) Integer.valueOf(value);
          break;
        case "Long":
          response = (T) Long.valueOf(value);
          break;
        case "Boolean":
          response = (T) Boolean.valueOf(value);
          break;
        default:
          response = (T) value;
          break;
      }
    }

    return response;
  }

  public long getExpire() {
    return expire;
  }

  public String getPassword() {
    return password;
  }

  public String getHost() {
    return host;
  }

  public int getPort() {
    return port;
  }

  public long getConnTimeout() {
    return connTimeout;
  }

  public int getMaxTotal() {
    return maxTotal;
  }

  public int getMaxIdle() {
    return maxIdle;
  }

  public int getMinIdle() {
    return minIdle;
  }

  public int getMaxRedirects() {
    return maxRedirects;
  }

  public String getNodes() {
    return nodes;
  }

  public long getMaxWaitMillis() {
    return maxWaitMillis;
  }

  public boolean isTestOnBorrow() {
    return testOnBorrow;
  }

  public boolean isTestOnCreate() {
    return testOnCreate;
  }

  public boolean isTestOnReturn() {
    return testOnReturn;
  }

  public boolean isTestWhileIdle() {
    return testWhileIdle;
  }

}
