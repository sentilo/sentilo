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
package org.sentilo.platform.server.test.handler;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.platform.server.handler.AbstractHandler;
import org.sentilo.platform.server.handler.HandlerLocator;
import org.sentilo.platform.server.request.SentiloRequest;

public class HandlerLocatorTest {

  private HandlerLocator locator;

  @Mock
  private AbstractHandler dummyTwoTokensHandler;
  @Mock
  private AbstractHandler dummySimpleHandler;
  @Mock
  private SentiloRequest request;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    locator = new HandlerLocator();
    locator.add("/token1", dummySimpleHandler);
    locator.add("/token1/token2", dummyTwoTokensHandler);
  }

  @Test
  public void lookupSimplePath() {
    final String path = "/token1/resource1/resource2";
    lookupAndVerifyHandler(path, dummySimpleHandler);
    verify(request).setPathParts(eq("/token1"), eq("resource1/resource2"));
  }

  @Test
  public void lookupComplexPath() {
    final String twoTokensPath = "/token1/token2/resource1/resource2";
    final String threeTokensPath = "/token1/token2/token3/resource1/resource2";
    lookupAndVerifyHandler(twoTokensPath, dummyTwoTokensHandler);
    verify(request).setPathParts(eq("/token1/token2"), eq("resource1/resource2"));
    lookupAndVerifyHandler(threeTokensPath, dummyTwoTokensHandler);
    verify(request).setPathParts(eq("/token1/token2"), eq("token3/resource1/resource2"));
  }

  @Test
  public void lookForUnexistingService() {
    when(request.getPath()).thenReturn("/unexisting");

    final AbstractHandler result = locator.lookup(request);

    assertNull(result);
    verify(request, times(0)).setPathParts(any(String.class), any(String.class));
  }

  private void lookupAndVerifyHandler(final String path, final AbstractHandler handlerExpected) {
    when(request.getPath()).thenReturn(path);

    final AbstractHandler result = locator.lookup(request);

    assertNotNull(result);
    assertEquals(handlerExpected, result);
  }
}
