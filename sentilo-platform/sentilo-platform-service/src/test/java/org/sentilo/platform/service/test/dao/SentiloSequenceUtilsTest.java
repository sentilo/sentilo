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
package org.sentilo.platform.service.test.dao;

import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.concurrent.locks.Lock;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.lock.LockFactory;
import org.sentilo.platform.service.dao.SentiloKeysBuilder;
import org.sentilo.platform.service.dao.SentiloRedisTemplate;
import org.sentilo.platform.service.dao.SentiloSequenceUtils;

public class SentiloSequenceUtilsTest {

  private static final String PID_KEY = "global:pid";
  private static final String SID_KEY = "global:sid";
  private static final String SDID_KEY = "global:sdid";
  private static final String SOID_KEY = "global:soid";
  private static final String AID_KEY = "global:aid";
  private static final String AMID_KEY = "global:amid";

  final String providerId = "provider1";
  final String sensorId = "sensor1";
  final String alertId = "alert1";

  @Mock
  private SentiloRedisTemplate sRedisTemplate;

  @Mock
  private LockFactory lockProvider;

  @Mock
  private Lock lock;

  @InjectMocks
  private SentiloSequenceUtils sequenceUtils;

  private final SentiloKeysBuilder keysBuilder = new SentiloKeysBuilder();

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    when(lockProvider.getLock(anyString())).thenReturn(lock);
  }

  @Test
  public void getPid() {
    when(sRedisTemplate.get(keysBuilder.getReverseProviderKey(providerId))).thenReturn("1");

    sequenceUtils.getPid(providerId);
    sequenceUtils.getPid(providerId);

    verify(sRedisTemplate, times(1)).get(keysBuilder.getReverseProviderKey(providerId));
  }

  @Test
  public void getNewPid() {
    sequenceUtils.getNewPidIfAbsent(providerId);
    sequenceUtils.getNewPidIfAbsent(providerId);

    verify(sRedisTemplate, times(1)).getKeyNextValue(PID_KEY);
  }

  @Test
  public void removePid() {
    sequenceUtils.getNewPidIfAbsent(providerId);
    sequenceUtils.getPid(providerId);
    sequenceUtils.removePid(providerId);
    sequenceUtils.getPid(providerId);

    verify(sRedisTemplate, times(2)).get(keysBuilder.getReverseProviderKey(providerId));
  }

  @Test
  public void getSid() {
    when(sRedisTemplate.get(keysBuilder.getReverseSensorKey(providerId, sensorId))).thenReturn("2");

    sequenceUtils.getSid(providerId, sensorId);
    sequenceUtils.getSid(providerId, sensorId);

    verify(sRedisTemplate, times(1)).get(keysBuilder.getReverseSensorKey(providerId, sensorId));
  }

  @Test
  public void getNewSid() {
    sequenceUtils.getNewSidIfAbsent(providerId, sensorId);
    sequenceUtils.getNewSidIfAbsent(providerId, sensorId);

    verify(sRedisTemplate, times(1)).getKeyNextValue(SID_KEY);
  }

  @Test
  public void removeSid() {
    sequenceUtils.getNewSidIfAbsent(providerId, sensorId);
    sequenceUtils.getSid(providerId, sensorId);
    sequenceUtils.removeSid(providerId, sensorId);
    sequenceUtils.getSid(providerId, sensorId);

    verify(sRedisTemplate, times(2)).get(keysBuilder.getReverseSensorKey(providerId, sensorId));
  }

  @Test
  public void getAid() {
    when(sRedisTemplate.get(keysBuilder.getReverseAlertKey(alertId))).thenReturn("1");

    sequenceUtils.getAid(alertId);
    sequenceUtils.getAid(alertId);

    verify(sRedisTemplate, times(1)).get(keysBuilder.getReverseAlertKey(alertId));
  }

  @Test
  public void getNewAid() {
    sequenceUtils.getNewAidIfAbsent(alertId);
    sequenceUtils.getNewAidIfAbsent(alertId);

    verify(sRedisTemplate, times(1)).getKeyNextValue(AID_KEY);
  }

  @Test
  public void removeAid() {
    sequenceUtils.getNewAidIfAbsent(alertId);
    sequenceUtils.getAid(alertId);
    sequenceUtils.removeAid(alertId);
    sequenceUtils.getAid(alertId);

    verify(sRedisTemplate, times(2)).get(keysBuilder.getReverseAlertKey(alertId));
  }

  @Test
  public void getSdid() {
    sequenceUtils.getNewSdid();

    verify(sRedisTemplate).getKeyNextValue(SDID_KEY);
  }

  @Test
  public void getSoid() {
    sequenceUtils.getNewSoid();

    verify(sRedisTemplate).getKeyNextValue(SOID_KEY);
  }

  @Test
  public void getAmid() {
    sequenceUtils.getNewAmid();

    verify(sRedisTemplate).getKeyNextValue(AMID_KEY);
  }

  @Test
  public void getCurrentSdid() {
    sequenceUtils.getCurrentSdid();

    verify(sRedisTemplate).get(SDID_KEY);
  }

  @Test
  public void getCurrentSoid() {
    sequenceUtils.getCurrentSoid();

    verify(sRedisTemplate).get(SOID_KEY);
  }

  @Test
  public void getCurrentAmid() {
    sequenceUtils.getCurrentAmid();

    verify(sRedisTemplate).get(AMID_KEY);
  }

}
