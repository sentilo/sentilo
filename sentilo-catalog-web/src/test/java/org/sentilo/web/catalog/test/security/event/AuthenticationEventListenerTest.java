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
package org.sentilo.web.catalog.test.security.event;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.web.catalog.security.event.AuthenticationFailureListener;
import org.sentilo.web.catalog.security.event.AuthenticationSuccessEventListener;
import org.sentilo.web.catalog.security.service.LoginAttemptService;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.authentication.event.AuthenticationFailureBadCredentialsEvent;
import org.springframework.security.authentication.event.AuthenticationSuccessEvent;

public class AuthenticationEventListenerTest {

  @InjectMocks
  private AuthenticationFailureListener failureListener;

  @InjectMocks
  private AuthenticationSuccessEventListener successListener;

  @Mock
  private LoginAttemptService loginAttemptService;

  @Mock
  private AuthenticationFailureBadCredentialsEvent failureEvent;

  @Mock
  private AuthenticationSuccessEvent successEvent;

  @Mock
  private UsernamePasswordAuthenticationToken token;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void onFailureEvent() {
    final String mockPrincipal = "mock_id";
    when(failureEvent.getSource()).thenReturn(token);
    when(token.getPrincipal()).thenReturn(mockPrincipal);

    failureListener.onApplicationEvent(failureEvent);

    verify(failureEvent).getSource();
    verify(loginAttemptService).loginFailed(token.getPrincipal().toString());
  }

  @Test
  public void onSuccessEvent() {
    final String mockPrincipal = "mock_id";
    when(successEvent.getSource()).thenReturn(token);
    when(token.getPrincipal()).thenReturn(mockPrincipal);

    successListener.onApplicationEvent(successEvent);

    verify(successEvent).getSource();
    verify(loginAttemptService).loginSucceeded(token.getPrincipal().toString());
  }

}
