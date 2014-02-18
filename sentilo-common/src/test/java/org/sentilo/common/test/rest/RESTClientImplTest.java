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
package org.sentilo.common.test.rest;

import static org.mockito.Matchers.notNull;
import static org.mockito.Mockito.when;

import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.HttpVersion;
import org.apache.http.StatusLine;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.entity.StringEntity;
import org.apache.http.message.BasicStatusLine;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.exception.RESTClientException;
import org.sentilo.common.rest.impl.RESTClientImpl;

public class RESTClientImplTest {

  private final String host = "http://l27.0.0.1";

  private RESTClientImpl restClient;

  @Mock
  private HttpClient httpClient;
  @Mock
  HttpResponse httpResponse;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);

    restClient = new RESTClientImpl();
    restClient.setHost(host);
    restClient.setHttpClient(httpClient);

  }

  @Test
  public void invalidGet() throws Exception {
    final String path = "data";
    final StatusLine statusLine = new BasicStatusLine(HttpVersion.HTTP_1_0, HttpStatus.SC_FORBIDDEN, "Forbidden request");

    when(httpClient.execute(notNull(HttpGet.class))).thenReturn(httpResponse);
    when(httpResponse.getStatusLine()).thenReturn(statusLine);

    try {
      restClient.get(path);
    } catch (final RESTClientException rce) {
      Assert.assertEquals(HttpStatus.SC_FORBIDDEN, rce.getStatus());
      Assert.assertEquals("Forbidden request", rce.getMessage());
    }
  }

  @Test
  public void get() throws Exception {
    final String path = "data";
    final String responseContent = "Lorem ipsum";
    final StatusLine statusLine = new BasicStatusLine(HttpVersion.HTTP_1_0, HttpStatus.SC_OK, "");

    when(httpClient.execute(notNull(HttpGet.class))).thenReturn(httpResponse);
    when(httpResponse.getStatusLine()).thenReturn(statusLine);
    when(httpResponse.getEntity()).thenReturn(new StringEntity(responseContent));

    final String result = restClient.get(path);
    Assert.assertEquals(responseContent, result);
  }

}
