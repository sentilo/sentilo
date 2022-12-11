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
package org.sentilo.platform.service.test.service;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.converter.DefaultStringMessageConverter;
import org.sentilo.common.converter.StringMessageConverter;
import org.sentilo.common.enums.SignalType;
import org.sentilo.common.enums.SubscribeType;
import org.sentilo.common.utils.MessagingUtils;
import org.sentilo.platform.common.domain.AlarmSubscription;
import org.sentilo.platform.common.domain.DataSubscription;
import org.sentilo.platform.common.domain.NotificationParams;
import org.sentilo.platform.common.domain.OrderSubscription;
import org.sentilo.platform.common.domain.Subscription;
import org.sentilo.platform.common.exception.ResourceNotFoundException;
import org.sentilo.platform.common.exception.ResourceOfflineException;
import org.sentilo.platform.common.service.AlarmService;
import org.sentilo.platform.common.service.PublishService;
import org.sentilo.platform.common.service.ResourceService;
import org.sentilo.platform.service.impl.SubscribeServiceImpl;
import org.sentilo.platform.service.subscriber.SubscriberRegistry;
import org.springframework.util.CollectionUtils;

public class SubscribeServiceImplTest {

  private final StringMessageConverter converter = new DefaultStringMessageConverter();

  @Mock
  private PublishService publishService;
  @Mock
  private SubscriberRegistry subsRegistry;
  @Mock
  private DataSubscription dataSubscription;
  @Mock
  private AlarmSubscription alarmSubscription;
  @Mock
  private OrderSubscription orderSubscription;
  @Mock
  private ResourceService resourceService;
  @Mock
  private AlarmService alarmService;

  @InjectMocks
  private SubscribeServiceImpl service;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void dataSubscribe() {
    final String provider = "prov1";
    final String sensor = "sensor1";
    when(dataSubscription.getType()).thenReturn(SubscribeType.DATA);
    when(dataSubscription.getSensorId()).thenReturn(sensor);
    when(dataSubscription.getProviderId()).thenReturn(provider);
    when(dataSubscription.getTopic()).thenReturn(MessagingUtils.buildTopic(SubscribeType.DATA, provider, sensor));
    doNothing().when(resourceService).checkSensorState(provider, sensor);

    verifySubscribe(dataSubscription);
  }

  @Test(expected = ResourceNotFoundException.class)
  public void subscribeToUnknowSensor() {
    final String provider = "prov1";
    final String sensor = "sensor1";
    when(dataSubscription.getType()).thenReturn(SubscribeType.DATA);
    when(dataSubscription.getSensorId()).thenReturn(sensor);
    when(dataSubscription.getProviderId()).thenReturn(provider);
    doThrow(ResourceNotFoundException.class).when(resourceService).checkSensorState(provider, sensor);

    verifySubscribe(dataSubscription);
  }

  @Test(expected = ResourceOfflineException.class)
  public void subscribeToDisabledSensor() {
    final String provider = "prov1";
    final String sensor = "sensor1";
    when(dataSubscription.getType()).thenReturn(SubscribeType.DATA);
    when(dataSubscription.getSensorId()).thenReturn(sensor);
    when(dataSubscription.getProviderId()).thenReturn(provider);
    doThrow(ResourceOfflineException.class).when(resourceService).checkSensorState(provider, sensor);

    verifySubscribe(dataSubscription);
  }

  @Test
  public void alarmSubscribe() {
    final String alertId = "alert1";
    when(alarmSubscription.getType()).thenReturn(SubscribeType.ALARM);
    when(alarmSubscription.getAlertId()).thenReturn(alertId);
    when(alarmService.getAlertOwner(eq(alertId))).thenReturn("mockOwner");
    when(alarmSubscription.getTopic()).thenReturn(MessagingUtils.buildTopic(SubscribeType.ALARM, alertId));
    doNothing().when(resourceService).checkAlertState(alertId);

    verifySubscribe(alarmSubscription);
  }

  @Test(expected = ResourceNotFoundException.class)
  public void subscribeToUnknowAlert() {
    final String alertId = "alert1";
    when(alarmSubscription.getType()).thenReturn(SubscribeType.ALARM);
    when(alarmSubscription.getAlertId()).thenReturn(alertId);
    doThrow(ResourceNotFoundException.class).when(resourceService).checkAlertState(alertId);

    verifySubscribe(alarmSubscription);
  }

  @Test(expected = ResourceOfflineException.class)
  public void subscribeToDisabledAlert() {
    final String alertId = "alert1";
    when(alarmSubscription.getType()).thenReturn(SubscribeType.ALARM);
    when(alarmSubscription.getAlertId()).thenReturn(alertId);
    when(alarmService.getAlertOwner(eq(alertId))).thenReturn("mockOwner");
    doThrow(ResourceOfflineException.class).when(resourceService).checkAlertState(alertId);

    verifySubscribe(alarmSubscription);
  }

  @Test
  public void orderSubscribe() {
    when(orderSubscription.getType()).thenReturn(SubscribeType.ORDER);
    when(orderSubscription.getTopic()).thenReturn(MessagingUtils.buildTopic(SubscribeType.ORDER));

    verifySubscribe(orderSubscription);
  }

  @Test
  public void sensorOrderSubscribe() {
    final String provider = "prov1";
    final String sensor = "sensor1";

    when(orderSubscription.getType()).thenReturn(SubscribeType.ORDER);
    when(orderSubscription.getSensorId()).thenReturn(sensor);
    when(orderSubscription.getOwnerEntityId()).thenReturn(provider);
    when(orderSubscription.getTopic()).thenReturn(MessagingUtils.buildTopic(SubscribeType.ORDER, provider, sensor));
    doNothing().when(resourceService).checkSensorState(provider, sensor);

    verifySubscribe(orderSubscription);
  }

  @Test(expected = ResourceNotFoundException.class)
  public void rejectOrderSubscription() {
    final String provider = "prov1";
    final String sensor = "sensor1";

    when(orderSubscription.getType()).thenReturn(SubscribeType.ORDER);
    when(orderSubscription.getSensorId()).thenReturn(sensor);
    when(orderSubscription.getOwnerEntityId()).thenReturn(provider);
    doThrow(ResourceNotFoundException.class).when(resourceService).checkSensorState(provider, sensor);

    verifySubscribe(orderSubscription);
  }

  @Test
  public void removeSubscriptionToOneTopic() {
    initSubscription(dataSubscription);
    when(dataSubscription.getType()).thenReturn(SubscribeType.DATA);
    when(dataSubscription.hasResourceIdentified()).thenReturn(true);

    service.remove(dataSubscription);

    verify(subsRegistry).removeSubscriptions(dataSubscription.getSourceEntityId(), Collections.singletonList(dataSubscription.getTopic()));

  }

  @Test
  public void removeAllSubscriptionsOfOneType() {
    initSubscription(dataSubscription);
    when(dataSubscription.getType()).thenReturn(SubscribeType.DATA);
    when(dataSubscription.hasResourceIdentified()).thenReturn(false);

    service.remove(dataSubscription);

    verify(subsRegistry).removeSubscriptions(dataSubscription.getSourceEntityId(), SubscribeType.DATA);
    verify(publishService).publishInternalSignal(SignalType.RELOAD_SUBSCRIPTIONS);
  }

  @Test
  public void removeAllSubscriberSubscriptions() {
    initSubscription(dataSubscription);
    when(dataSubscription.getType()).thenReturn(null);
    when(dataSubscription.hasResourceIdentified()).thenReturn(true);

    service.remove(dataSubscription);

    verify(subsRegistry).removeSubscriptions(dataSubscription.getSourceEntityId());
    verify(publishService).publishInternalSignal(SignalType.RELOAD_SUBSCRIPTIONS);
  }

  @Test
  public void getSubscriptions() {
    initSubscription(dataSubscription);
    final String subscriberId = dataSubscription.getSourceEntityId();
    final SubscribeType type = SubscribeType.DATA;
    when(dataSubscription.getType()).thenReturn(type);
    when(subsRegistry.loadSubscription(subscriberId, type)).thenReturn(buildSubscriptions());

    // Retrieve all subscriptions of type DATA from subscriber subscriberId
    final List<Subscription> subscriptions = service.get(dataSubscription);

    verify(subsRegistry).loadSubscription(subscriberId, type);
    assertTrue(subscriptions.size() == buildSubscriptions().size());
    for (final Subscription subscription : subscriptions) {
      assertEquals("http://127.0.0.1/endpoint", subscription.getNotificationParams().getEndpoint());
    }

  }

  @Test
  public void getEmptySubscriptions() {
    initSubscription(dataSubscription);
    final String subscriberId = dataSubscription.getSourceEntityId();
    final SubscribeType type = SubscribeType.DATA;
    when(dataSubscription.getType()).thenReturn(type);
    when(subsRegistry.loadSubscription(subscriberId, type)).thenReturn(Collections.emptyMap());

    final List<Subscription> subscriptions = service.get(dataSubscription);

    verify(subsRegistry).loadSubscription(subscriberId, type);
    assertTrue(CollectionUtils.isEmpty(subscriptions));

  }

  private void initSubscription(final Subscription subscription) {
    final NotificationParams notificationParams = new NotificationParams("http://127.0.0.1/endpoint", "ABCDEFGH", 3, 5);
    when(subscription.getSourceEntityId()).thenReturn("prov1");
    when(subscription.getNotificationParams()).thenReturn(notificationParams);

  }

  private void verifySubscribe(final Subscription subscription) {
    initSubscription(subscription);

    service.subscribe(subscription);

    verify(subsRegistry).add(subscription.getSourceEntityId(), subscription.getTopic(), subscription.getNotificationParams());
    verify(publishService).publishInternalSignal(SignalType.RELOAD_SUBSCRIPTIONS);
  }

  private Map<String, NotificationParams> buildSubscriptions() {
    final Map<String, NotificationParams> subscriptions = new HashMap<String, NotificationParams>();
    subscriptions.put("/data/provider1/sensor1",
        unmarshallNotificationParams("{\"endpoint\":\"http://127.0.0.1/endpoint\", \"secretCallbackKey\":\"ABCDEFGH\"}"));
    subscriptions.put("/data/provider2*", unmarshallNotificationParams("{\"endpoint\":\"http://127.0.0.1/endpoint\"}"));
    return subscriptions;
  }

  private NotificationParams unmarshallNotificationParams(final String jsonValue) {
    return converter.unmarshal(jsonValue, NotificationParams.class);
  }
}
