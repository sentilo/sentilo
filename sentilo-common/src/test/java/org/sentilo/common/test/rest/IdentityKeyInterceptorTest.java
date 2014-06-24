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

import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.apache.http.Header;
import org.apache.http.HttpRequest;
import org.apache.http.protocol.HttpContext;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.rest.interceptor.IdentityKeyInterceptor;
import org.sentilo.common.utils.SentiloConstants;
import org.springframework.test.util.ReflectionTestUtils;

public class IdentityKeyInterceptorTest {

  @InjectMocks
  private IdentityKeyInterceptor interceptor;

  @Mock
  private Header identityHeader;
  @Mock
  private HttpRequest request;
  @Mock
  private HttpContext context;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void setIdentityToken() {
    final String identityToken = "ABCDEF";

    interceptor.setIdentityToken(identityToken);

    Header identityHeader = (Header) ReflectionTestUtils.getField(interceptor, "identityHeader");
    Assert.assertNotNull(identityHeader);
    Assert.assertEquals(SentiloConstants.IDENTITY_KEY_HEADER, identityHeader.getName());
    Assert.assertEquals(identityToken, identityHeader.getValue());
  }

  @Test
  public void process() throws Exception {
    when(request.containsHeader(SentiloConstants.IDENTITY_KEY_HEADER)).thenReturn(false);

    interceptor.process(request, context);

    verify(request).addHeader(eq(identityHeader));
  }

  @Test
  public void processWithoutAddHEader() throws Exception {
    when(request.containsHeader(SentiloConstants.IDENTITY_KEY_HEADER)).thenReturn(true);

    interceptor.process(request, context);

    verify(request, times(0)).addHeader(eq(identityHeader));
  }
}
