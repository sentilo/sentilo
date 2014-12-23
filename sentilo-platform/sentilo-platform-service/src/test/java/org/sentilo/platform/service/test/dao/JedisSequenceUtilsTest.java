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
package org.sentilo.platform.service.test.dao;

import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.platform.service.dao.JedisSequenceUtils;
import org.sentilo.platform.service.dao.JedisTemplate;

public class JedisSequenceUtilsTest {

  private static final String PID_KEY = "global:pid";
  private static final String SID_KEY = "global:sid";
  private static final String SDID_KEY = "global:sdid";
  private static final String SOID_KEY = "global:soid";
  private static final String AID_KEY = "global:aid";
  private static final String AMID_KEY = "global:amid";

  final String providerId = "provider1";
  final String sensorId = "sensor1";
  final String alarmId = "alarm1";

  @Mock
  private JedisTemplate<String, String> jedisTemplate;

  @InjectMocks
  private JedisSequenceUtils jedisSequenceUtils;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void getPid() {
    when(jedisTemplate.get("provider:" + providerId + ":pid")).thenReturn("1");

    jedisSequenceUtils.getPid(providerId);
    jedisSequenceUtils.getPid(providerId);

    verify(jedisTemplate, times(1)).get("provider:" + providerId + ":pid");
  }

  @Test
  public void setPid() {
    jedisSequenceUtils.setPid(providerId);
    jedisSequenceUtils.setPid(providerId);

    verify(jedisTemplate, times(1)).getKeyNextValue(PID_KEY);
  }

  @Test
  public void removePid() {
    jedisSequenceUtils.setPid(providerId);
    jedisSequenceUtils.getPid(providerId);
    jedisSequenceUtils.removePid(providerId);
    jedisSequenceUtils.getPid(providerId);

    verify(jedisTemplate, times(1)).get("provider:" + providerId + ":pid");
  }

  @Test
  public void getSid() {
    when(jedisTemplate.get("sensor:" + providerId + ":" + sensorId + ":sid")).thenReturn("2");

    jedisSequenceUtils.getSid(providerId, sensorId);
    jedisSequenceUtils.getSid(providerId, sensorId);

    verify(jedisTemplate, times(1)).get("sensor:" + providerId + ":" + sensorId + ":sid");
  }

  @Test
  public void setSid() {
    jedisSequenceUtils.setSid(providerId, sensorId);
    jedisSequenceUtils.setSid(providerId, sensorId);

    verify(jedisTemplate, times(1)).getKeyNextValue(SID_KEY);
  }

  @Test
  public void removeSid() {
    jedisSequenceUtils.setSid(providerId, sensorId);
    jedisSequenceUtils.getSid(providerId, sensorId);
    jedisSequenceUtils.removeSid(providerId, sensorId);
    jedisSequenceUtils.getSid(providerId, sensorId);

    verify(jedisTemplate, times(1)).get("sensor:" + providerId + ":" + sensorId + ":sid");
  }

  @Test
  public void getAid() {
    when(jedisTemplate.get("alarm:" + alarmId + ":aid")).thenReturn("1");

    jedisSequenceUtils.getAid(alarmId);
    jedisSequenceUtils.getAid(alarmId);

    verify(jedisTemplate, times(1)).get("alarm:" + alarmId + ":aid");
  }

  @Test
  public void setAid() {
    jedisSequenceUtils.setAid(alarmId);
    jedisSequenceUtils.setAid(alarmId);

    verify(jedisTemplate, times(1)).getKeyNextValue(AID_KEY);
  }

  @Test
  public void removeAid() {
    jedisSequenceUtils.setAid(alarmId);
    jedisSequenceUtils.getAid(alarmId);
    jedisSequenceUtils.removeAid(alarmId);
    jedisSequenceUtils.getAid(alarmId);

    verify(jedisTemplate, times(1)).get("alarm:" + alarmId + ":aid");
  }

  @Test
  public void getSdid() {
    jedisSequenceUtils.getSdid();

    verify(jedisTemplate).getKeyNextValue(SDID_KEY);
  }

  @Test
  public void getSoid() {
    jedisSequenceUtils.getSoid();

    verify(jedisTemplate).getKeyNextValue(SOID_KEY);
  }

  @Test
  public void getAmid() {
    jedisSequenceUtils.getAmid();

    verify(jedisTemplate).getKeyNextValue(AMID_KEY);
  }

  @Test
  public void getCurrentSdid() {
    jedisSequenceUtils.getCurrentSdid();

    verify(jedisTemplate).get(SDID_KEY);
  }

  @Test
  public void getCurrentSoid() {
    jedisSequenceUtils.getCurrentSoid();

    verify(jedisTemplate).get(SOID_KEY);
  }

  @Test
  public void getCurrentAmid() {
    jedisSequenceUtils.getCurrentAmid();

    verify(jedisTemplate).get(AMID_KEY);
  }

}
