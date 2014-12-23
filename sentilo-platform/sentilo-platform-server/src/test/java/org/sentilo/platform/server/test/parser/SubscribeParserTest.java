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
package org.sentilo.platform.server.test.parser;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.when;

import java.io.ByteArrayOutputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.apache.http.HttpVersion;
import org.apache.http.entity.ByteArrayEntity;
import org.apache.http.message.BasicHttpResponse;
import org.apache.http.message.BasicStatusLine;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.domain.SubscribeType;
import org.sentilo.platform.common.domain.AlarmSubscription;
import org.sentilo.platform.common.domain.DataSubscription;
import org.sentilo.platform.common.domain.OrderSubscription;
import org.sentilo.platform.common.domain.Subscription;
import org.sentilo.platform.server.parser.SubscribeParser;
import org.sentilo.platform.server.request.SentiloRequest;
import org.sentilo.platform.server.request.SentiloResource;
import org.sentilo.platform.server.response.SentiloResponse;

public class SubscribeParserTest {

  private SubscribeParser parser;
  @Mock
  private SentiloRequest sentiloRequest;
  @Mock
  private SentiloResource resource;

  @Before
  public void setUp() throws Exception {
    final String identityKeyProvider = "prov1";
    MockitoAnnotations.initMocks(this);
    parser = new SubscribeParser();

    when(sentiloRequest.getResource()).thenReturn(resource);
    when(sentiloRequest.getEntitySource()).thenReturn(identityKeyProvider);
  }

  @Test
  public void parseDataRequest() throws Exception {
    final String eventType = "data";
    final String identityKeyProvider = "prov1";
    final String providerId = identityKeyProvider;
    final String sensorId = "sensor1";
    final String endpoint = "www.opentrends.net";

    final String json = "{\"endpoint\":\"www.opentrends.net\", \"secretCallbackKey\":\"ABCDEFGH\"}";

    when(sentiloRequest.getBody()).thenReturn(json);
    when(sentiloRequest.getResourcePart(0)).thenReturn(eventType);
    when(sentiloRequest.getResourcePart(1)).thenReturn(providerId);
    when(sentiloRequest.getResourcePart(2)).thenReturn(sensorId);

    final Subscription message = parser.parseRequest(sentiloRequest);

    assertEquals(endpoint, message.getEndpoint());
    assertEquals(identityKeyProvider, message.getSourceEntityId());
    assertTrue(message instanceof DataSubscription);
    assertEquals(providerId, ((DataSubscription) message).getProviderId());
    assertEquals(sensorId, ((DataSubscription) message).getSensorId());
    assertEquals("ABCDEFGH", message.getSecretCallbackKey());
  }

  @Test
  public void parseDataRequestWithoutEventType() throws Exception {
    final String identityKeyProvider = "prov1";
    final String providerId = identityKeyProvider;
    final String sensorId = "sensor1";
    final String endpoint = "www.opentrends.net";

    final String json = "{\"endpoint\":\"www.opentrends.net\"}";

    when(sentiloRequest.getBody()).thenReturn(json);
    when(sentiloRequest.getResourcePart(0)).thenReturn(providerId);
    when(sentiloRequest.getResourcePart(1)).thenReturn(sensorId);

    final Subscription message = parser.parseRequest(sentiloRequest, SubscribeType.DATA);

    assertEquals(endpoint, message.getEndpoint());
    assertEquals(identityKeyProvider, message.getSourceEntityId());
    assertTrue(message instanceof DataSubscription);
    assertEquals(providerId, ((DataSubscription) message).getProviderId());
    assertEquals(sensorId, ((DataSubscription) message).getSensorId());
    assertNull(message.getSecretCallbackKey());
  }

  @Test
  public void parseAlarmRequest() throws Exception {
    final String eventType = "alarm";
    final String identityKeyProvider = "prov1";
    final String alarmId = "alarm1";
    final String endpoint = "www.opentrends.net";

    final String json = "{\"endpoint\":\"www.opentrends.net\"}";

    when(sentiloRequest.getBody()).thenReturn(json);
    when(sentiloRequest.getResourcePart(0)).thenReturn(eventType);
    // when(sentiloRequest.getResourcePart(1)).thenReturn(identityKeyProvider);
    when(sentiloRequest.getResourcePart(1)).thenReturn(alarmId);

    final Subscription message = parser.parseRequest(sentiloRequest);

    assertEquals(endpoint, message.getEndpoint());
    assertEquals(identityKeyProvider, message.getSourceEntityId());
    assertTrue(message instanceof AlarmSubscription);
    assertEquals(alarmId, ((AlarmSubscription) message).getAlertId());
    assertNull(message.getOwnerEntityId());
    assertNull(message.getSecretCallbackKey());
  }

  @Test
  public void parseOrderRequest() throws Exception {
    final String eventType = "order";
    final String identityKeyProvider = "prov1";
    final String providerId = identityKeyProvider;
    final String sensorId = "sensor1";
    final String endpoint = "www.opentrends.net";

    final String json = "{\"endpoint\":\"www.opentrends.net\"}";

    when(sentiloRequest.getBody()).thenReturn(json);
    when(sentiloRequest.getResourcePart(0)).thenReturn(eventType);
    when(sentiloRequest.getResourcePart(1)).thenReturn(providerId);
    when(sentiloRequest.getResourcePart(2)).thenReturn(sensorId);

    final Subscription message = parser.parseRequest(sentiloRequest);

    assertEquals(endpoint, message.getEndpoint());
    assertEquals(identityKeyProvider, message.getSourceEntityId());
    assertEquals(providerId, message.getOwnerEntityId());
    assertTrue(message instanceof OrderSubscription);
    assertNull(message.getSecretCallbackKey());
  }

  @Test
  public void parseWriteResponse() throws Exception {
    final SentiloResponse response = SentiloResponse.build(new BasicHttpResponse(new BasicStatusLine(HttpVersion.HTTP_1_0, 200, "")));
    parser.writeResponse(response, getSubscriptions());
    final ByteArrayOutputStream baos = new ByteArrayOutputStream();
    ((ByteArrayEntity) response.getHttpResponse().getEntity()).writeTo(baos);
    final String expected =
        "{\"subscriptions\":[{\"endpoint\":\"htt://dev.connecta.cat\",\"type\":\"ALARM\",\"alert\":\"alarm1\"},{\"endpoint\":\"htt://dev.connecta.cat\",\"type\":\"ORDER\",\"provider\":\"prov2\"},{\"endpoint\":\"htt://dev.connecta.cat\",\"type\":\"DATA\",\"provider\":\"prov2\"},{\"endpoint\":\"htt://dev.connecta.cat\",\"type\":\"DATA\",\"provider\":\"prov2\",\"sensor\":\"sensor2\"}]}";
    assertEquals(expected, baos.toString());
  }

  @Test
  public void parseEmptyWriteResponse() throws Exception {
    final SentiloResponse response = SentiloResponse.build(new BasicHttpResponse(new BasicStatusLine(HttpVersion.HTTP_1_0, 200, "")));
    final List<Subscription> subscriptionList = Collections.emptyList();
    parser.writeResponse(response, subscriptionList);
    final ByteArrayOutputStream baos = new ByteArrayOutputStream();
    ((ByteArrayEntity) response.getHttpResponse().getEntity()).writeTo(baos);
    final String expected = "{\"subscriptions\":[]}";
    assertEquals(expected, baos.toString());
  }

  private List<Subscription> getSubscriptions() {
    final List<Subscription> list = new ArrayList<Subscription>();
    list.add(new AlarmSubscription("prov1", "prov2", "htt://dev.connecta.cat", "alarm1"));
    list.add(new OrderSubscription("prov1", "prov2", "htt://dev.connecta.cat"));
    list.add(new DataSubscription("prov1", "htt://dev.connecta.cat", "prov2"));
    list.add(new DataSubscription("prov1", "htt://dev.connecta.cat", "prov2", "sensor2"));
    return list;
  }
}
