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
package org.sentilo.agent.federation.test.service;

import static org.mockito.Matchers.argThat;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.List;

import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentMatcher;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.agent.federation.domain.FederationConfig;
import org.sentilo.agent.federation.service.impl.RemotePlatformServiceImpl;
import org.sentilo.common.rest.RESTClient;
import org.sentilo.common.rest.RequestContext;
import org.springframework.util.StringUtils;

public class RemotePlatformServiceImplTest {

  private final static String MOCK_ENDPOINT = "http://mock-domain:8081/sentilo";
  private final static String MOCK_TOKEN = "123456789";

  @InjectMocks
  private RemotePlatformServiceImpl remoteService;

  @Mock
  private RESTClient remoteRestClient;

  @Mock
  private FederationConfig fConfig;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void getPermissions() {
    final String path = "catalog";

    when(fConfig.getSourceEndpoint()).thenReturn(MOCK_ENDPOINT);
    when(fConfig.getAppClientToken()).thenReturn(MOCK_TOKEN);

    remoteService.getPermissions(fConfig);

    verify(remoteRestClient).get(argThat(new RequestContextMatcher(path, MOCK_ENDPOINT, MOCK_TOKEN)));
  }

  @Test
  public void getSubscriptions() {
    final String path = "subscribe";

    when(fConfig.getSourceEndpoint()).thenReturn(MOCK_ENDPOINT);
    when(fConfig.getAppClientToken()).thenReturn(MOCK_TOKEN);

    remoteService.getSubscriptions(fConfig);

    verify(remoteRestClient).get(argThat(new RequestContextMatcher(path, MOCK_ENDPOINT, MOCK_TOKEN)));
  }

  @Test
  public void createSubscriptions() {
    final List<String> mockProvidersList = Arrays.asList("prov1", "prov2");

    when(fConfig.getSourceEndpoint()).thenReturn(MOCK_ENDPOINT);
    when(fConfig.getAppClientToken()).thenReturn(MOCK_TOKEN);

    remoteService.createSubscriptions(mockProvidersList, fConfig);

    verify(remoteRestClient, times(2)).put(argThat(new RequestContextMatcher("subscribe/data/prov", MOCK_ENDPOINT, MOCK_TOKEN)));
  }

  @Test
  public void deleteSubscriptions() {
    final List<String> mockProvidersList = Arrays.asList("prov1", "prov2");

    when(fConfig.getSourceEndpoint()).thenReturn(MOCK_ENDPOINT);
    when(fConfig.getAppClientToken()).thenReturn(MOCK_TOKEN);

    remoteService.deleteSubscriptions(mockProvidersList, fConfig);

    verify(remoteRestClient, times(2)).delete(argThat(new RequestContextMatcher("subscribe/data/prov", MOCK_ENDPOINT, MOCK_TOKEN)));
  }

  class RequestContextMatcher extends ArgumentMatcher<RequestContext> {

    private final String path;
    private final String endpoint;
    private final String token;

    public RequestContextMatcher(final String path, final String endpoint, final String token) {
      this.path = path;
      this.endpoint = endpoint;
      this.token = token;
    }

    @Override
    public boolean matches(final Object argument) {
      final RequestContext rc = (RequestContext) argument;
      final boolean matchesPath = StringUtils.hasText(rc.getPath()) && rc.getPath().startsWith(path);
      final boolean matchesEndpoint = StringUtils.hasText(rc.getHost()) && rc.getHost().startsWith(endpoint);
      final boolean matchesToken = StringUtils.hasText(rc.getIdentityToken()) && rc.getIdentityToken().startsWith(token);

      return matchesPath && matchesEndpoint && matchesToken;
    }

  }
}
