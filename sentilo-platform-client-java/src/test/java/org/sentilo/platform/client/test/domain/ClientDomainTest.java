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
package org.sentilo.platform.client.test.domain;

import static org.mockito.Mockito.mock;

import java.beans.PropertyDescriptor;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.junit.Assert;
import org.junit.Test;
import org.sentilo.common.domain.AlertOwner;
import org.sentilo.common.domain.AuthorizedProvider;
import org.sentilo.common.domain.CatalogAlert;
import org.sentilo.common.domain.CatalogComponent;
import org.sentilo.common.domain.CatalogSensor;
import org.sentilo.common.domain.OrderMessage;
import org.sentilo.common.domain.QueryFilterParams;
import org.sentilo.common.domain.SensorOrdersMessage;
import org.sentilo.common.utils.DateUtils;
import org.sentilo.platform.client.core.domain.AlarmInputMessage;
import org.sentilo.platform.client.core.domain.AlarmMessage;
import org.sentilo.platform.client.core.domain.AlarmsOutputMessage;
import org.sentilo.platform.client.core.domain.CatalogAlertInputMessage;
import org.sentilo.platform.client.core.domain.CatalogAlertOutputMessage;
import org.sentilo.platform.client.core.domain.CatalogDeleteInputMessage;
import org.sentilo.platform.client.core.domain.CatalogInputMessage;
import org.sentilo.platform.client.core.domain.CatalogOutputMessage;
import org.sentilo.platform.client.core.domain.DataInputMessage;
import org.sentilo.platform.client.core.domain.Observation;
import org.sentilo.platform.client.core.domain.ObservationsOutputMessage;
import org.sentilo.platform.client.core.domain.OrderInputMessage;
import org.sentilo.platform.client.core.domain.OrdersOutputMessage;
import org.sentilo.platform.client.core.domain.ProviderObservations;
import org.sentilo.platform.client.core.domain.SensorObservations;
import org.sentilo.platform.client.core.domain.SubscribeInputMessage;
import org.sentilo.platform.client.core.domain.Subscription;
import org.sentilo.platform.client.core.domain.SubscriptionParams;
import org.sentilo.platform.client.core.domain.SubscriptionsOutputMessage;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.MutablePropertyValues;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.validation.DataBinder;

public class ClientDomainTest {

  @Test
  public void subscribeInputMessage() throws Exception {
    final String identityToken = "mockToken";
    final SubscribeInputMessage message = new SubscribeInputMessage();
    message.setIdentityToken(identityToken);

    Assert.assertTrue(message.toString().startsWith("--- Subscription ---"));
    Assert.assertEquals(identityToken, message.getIdentityToken());

  }

  @Test
  public void alarmInputMessage() throws Exception {
    final String alertId = "mockAlarmId";
    final String identityToken = "mockToken";
    final QueryFilterParams queryFilters = mock(QueryFilterParams.class);
    final AlarmInputMessage message = new AlarmInputMessage(alertId, queryFilters);
    message.setIdentityToken(identityToken);

    Assert.assertTrue(message.hasQueryFilters());
    Assert.assertTrue(message.toString().startsWith("\n\t --- Alarm ---"));
    Assert.assertEquals(identityToken, message.getIdentityToken());
    Assert.assertEquals(alertId, message.getAlertId());
  }

  @Test
  public void dataInputMessage() throws Exception {
    final String providerId = "mockProvId";
    final String identityToken = "mockToken";
    final QueryFilterParams queryFilters = mock(QueryFilterParams.class);
    final DataInputMessage message = new DataInputMessage(providerId, queryFilters);
    message.setIdentityToken(identityToken);

    Assert.assertTrue(message.hasQueryFilters());
    Assert.assertTrue(message.toString().startsWith("--- Message ---"));
    Assert.assertEquals(identityToken, message.getIdentityToken());
    Assert.assertEquals(providerId, message.getProviderId());
    Assert.assertNull(message.getSensorId());
    Assert.assertNull(message.getObservation());
  }

  @Test
  public void orderInputMessage() throws Exception {
    final String providerId = "mockProvId";
    final String sensorId = "mockSensorId";
    final String identityToken = "mockToken";
    final QueryFilterParams queryFilters = mock(QueryFilterParams.class);
    final OrderInputMessage message = new OrderInputMessage(providerId, sensorId, queryFilters);
    message.setIdentityToken(identityToken);

    Assert.assertTrue(message.hasQueryFilters());
    Assert.assertTrue(message.toString().startsWith("--- Order ---"));
    Assert.assertEquals(identityToken, message.getIdentityToken());
    Assert.assertEquals(providerId, message.getProviderId());
    Assert.assertEquals(sensorId, message.getSensorId());

    final OrderInputMessage message2 = new OrderInputMessage(providerId);
    Assert.assertEquals(providerId, message2.getProviderId());
    Assert.assertNull(message2.getSensorId());
  }

  @Test
  public void alarmMessage() throws Exception {
    final String sTimestamp = "18/06/2014T08:00:00";
    final Object[] values = {"mockMessage", "mockSender", sTimestamp, DateUtils.toMillis(sTimestamp)};
    final String[] attributes = {"message", "sender", "timestamp", "time"};
    final Object[] constructorValues = {"mock order", "mock sender", sTimestamp};

    buildAndCompareObjects(AlarmMessage.class, attributes, values, constructorValues);
  }

  @Test
  public void alarmsOutputMessage() throws Exception {
    final Object[] values = {Collections.<AlarmMessage>emptyList(), "mockSender"};
    final String[] attributes = {"alarms", "alarm"};

    bindAndValidateMutableObject(AlarmsOutputMessage.class, attributes, values);
  }

  @Test
  public void catalogAlertInputMessage() throws Exception {
    final Object[] values = {Collections.<CatalogAlert>emptyList(), new String[0], "mockEntityId"};
    final String[] attributes = {"alerts", "alertsIds", "providerId"};
    final Object[] values1 = {null, null, "mockEntityId"};

    // Case 1: default constructor
    bindAndValidateMutableObject(CatalogAlertInputMessage.class, attributes, values);

    // Case 2: constructor with 1 param
    final CatalogAlertInputMessage message = new CatalogAlertInputMessage("mockEntityId");
    assertFieldValues(message, attributes, values1);

    // Case 2: constructor with 2 params
    final CatalogAlertInputMessage message2 = new CatalogAlertInputMessage("mockEntityId", Collections.<String, String>emptyMap());
    assertFieldValues(message2, attributes, values1);
  }

  @Test
  public void catalogAlertOutputMessage() throws Exception {
    final Object[] values = {Collections.<CatalogAlert>emptyList(), Collections.<AlertOwner>emptyList()};
    final String[] attributes = {"alerts", "owners"};

    bindAndValidateMutableObject(CatalogAlertOutputMessage.class, attributes, values);
  }

  @Test
  public void catalogDeleteInputMessage() throws Exception {
    final String[] emptyStringArray = new String[0];
    final Object[] values = {emptyStringArray, emptyStringArray, "mockToken"};
    final String[] attributes = {"sensors", "components", "identityToken"};

    final String[] attributesToCheck = {"sensors", "components", "identityToken", "providerId", "resourcesValues"};
    final Object[] valuesToCheck = {emptyStringArray, emptyStringArray, "mockToken", "mockProvider", Arrays.asList("mockProvider")};

    final CatalogDeleteInputMessage message = new CatalogDeleteInputMessage("mockProvider");
    bindProperties(message, attributes, values);
    assertFieldValues(message, attributesToCheck, valuesToCheck);
  }

  @Test
  public void catalogInputMessage() throws Exception {

    final Object[] values = {"mockProvider", Collections.<CatalogSensor>emptyList(), Collections.<CatalogComponent>emptyList(), "mockToken",
        Collections.<String, String>emptyMap()};
    final String[] attributes = {"providerId", "sensors", "components", "identityToken", "parameters"};

    // Case 1: default constructor
    bindAndValidateMutableObject(CatalogInputMessage.class, attributes, values);

    // Case 2: constructor with 1 param
    final String[] attributesToCheck = {"providerId", "sensors", "resourcesValues"};
    final Object[] valuesToCheck = {"mockProvider", Collections.<CatalogSensor>emptyList(), Arrays.asList("mockProvider")};

    final CatalogInputMessage message = new CatalogInputMessage("mockProvider");
    assertFieldValues(message, attributesToCheck, valuesToCheck);

    // Case 3: constructor with 2 params
    final CatalogInputMessage message2 = new CatalogInputMessage("mockProvider", Collections.<CatalogSensor>emptyList());
    assertFieldValues(message2, attributesToCheck, valuesToCheck);
  }

  @Test
  public void catalogOutputMessage() throws Exception {
    final Object[] values = {Collections.<AuthorizedProvider>emptyList()};
    final String[] attributes = {"providers"};

    bindAndValidateMutableObject(CatalogOutputMessage.class, attributes, values);
  }

  @Test
  public void subscriptionParams() throws Exception {
    final Object[] valuesToCheck = {"mockEndpoint", "123456789AFCD", 4L, 10L};
    final String[] attributesToCheck = {"endpoint", "secretCallbackKey", "maxRetries", "retryDelay"};

    final SubscriptionParams subscriptionParams = new SubscriptionParams("mockEndpoint", "123456789AFCD", 4L, 10L);
    assertFieldValues(subscriptionParams, attributesToCheck, valuesToCheck);
  }

  @Test
  public void observationsOutputMessage() throws Exception {
    final Object[] values = {Collections.<Observation>emptyList(), Collections.<SensorObservations>emptyList()};
    final String[] attributes = {"observations", "sensors"};

    bindAndValidateMutableObject(ObservationsOutputMessage.class, attributes, values);
  }

  @Test
  public void ordersOutputMessage() throws Exception {
    final Object[] values = {Collections.<OrderMessage>emptyList(), Collections.<SensorOrdersMessage>emptyList()};
    final String[] attributes = {"orders", "sensors"};

    bindAndValidateMutableObject(OrdersOutputMessage.class, attributes, values);
  }

  @Test
  public void providerObservations() throws Exception {
    final Object[] values = {Collections.<SensorObservations>emptyList()};
    final String[] attributes = {"sensorsObservations"};

    bindAndValidateMutableObject(ProviderObservations.class, attributes, values);

    // toString method
    final ProviderObservations obj = new ProviderObservations();
    Assert.assertEquals("--- Provider observations ---", obj.toString());
  }

  @Test
  public void sensorObservations() throws Exception {
    final Object[] values = {Collections.<Observation>emptyList(), "mockLocation", "mockSensor"};
    final String[] attributes = {"observations", "location", "sensor"};

    final SensorObservations obj = new SensorObservations("mockSensor", "mockLocation");
    assertFieldValues(obj, attributes, values);

    // toString method
    Assert.assertTrue(obj.toString().startsWith("--- Sensor observations ---"));
  }

  @Test
  public void subscription() throws Exception {
    final Object[] values = {"mockEndpoint", "mockType", "mockProv", "mockSensor", "mockAlert"};
    final String[] attributes = {"endpoint", "type", "provider", "sensor", "alert"};

    bindAndValidateMutableObject(Subscription.class, attributes, values);
  }

  @Test
  public void subscriptionsOutputMessage() throws Exception {
    final Object[] values = {Collections.<Subscription>emptyList()};
    final String[] attributes = {"subscriptions"};

    bindAndValidateMutableObject(SubscriptionsOutputMessage.class, attributes, values);
  }

  // Utility methods

  private <T> void buildAndCompareObjects(final Class<T> clazz, final String[] attributes, final Object[] values, final Object[] constructorValues)
      throws Exception {
    final T message = clazz.newInstance();
    bindProperties(message, attributes, values);

    final T message2 = buildInstance(clazz, constructorValues);
    if (!constructorValues.equals(values)) {
      bindProperties(message2, attributes, values);
    }

    assertAreEquals(message, message2);
  }

  private <T> void bindAndValidateMutableObject(final Class<T> clazz, final String[] attributes, final Object[] values) throws Exception {
    final T target = clazz.newInstance();

    bindProperties(target, attributes, values);

    assertFieldValues(target, attributes, values);
  }

  private void assertFieldValues(final Object target, final String[] attributes, final Object[] values) {
    for (int i = 0; i < attributes.length; i++) {
      Assert.assertEquals("Error validating attribute " + attributes[i], values[i], ReflectionTestUtils.getField(target, attributes[i]));
    }
  }

  private void bindProperties(final Object target, final String[] attributes, final Object[] values) {
    final DataBinder binder = new DataBinder(target);
    binder.bind(buildMutablePropertyValues(attributes, values));
  }

  private MutablePropertyValues buildMutablePropertyValues(final String[] attributes, final Object[] values) {
    assert attributes != null && values != null && attributes.length == values.length;
    final Map<String, Object> properties = new HashMap<String, Object>();

    for (int i = 0; i < attributes.length; i++) {
      properties.put(attributes[i], values[i]);
    }

    return new MutablePropertyValues(properties);
  }

  @SuppressWarnings("unchecked")
  private <T> T buildInstance(final Class<T> clazz, final Object[] params) throws Exception {
    T newInstance = null;
    final Constructor<?>[] constructors = clazz.getDeclaredConstructors();
    for (final Constructor<?> constructor : constructors) {
      try {
        newInstance = (T) constructor.newInstance(params);
        break;
      } catch (final Exception e) {
      }
    }

    if (newInstance != null) {
      return newInstance;
    } else {
      final String template = "Class %s has not constructor with %d params";
      throw new IllegalArgumentException(String.format(template, clazz.getName(), params.length));

    }
  }

  private void assertAreEquals(final Object source, final Object target) throws Exception {
    Assert.assertEquals(source.getClass(), target.getClass());

    final PropertyDescriptor[] propertyDescriptors = BeanUtils.getPropertyDescriptors(source.getClass());

    for (final PropertyDescriptor propertyDescriptor : propertyDescriptors) {
      final Method readMethod = propertyDescriptor.getReadMethod();
      if (!readMethod.getName().equals("getClass")) {
        Assert.assertEquals(readMethod.invoke(source), readMethod.invoke(target));
      }
    }

  }

}
