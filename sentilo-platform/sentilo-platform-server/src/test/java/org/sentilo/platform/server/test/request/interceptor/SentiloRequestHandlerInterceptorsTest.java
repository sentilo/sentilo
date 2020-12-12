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
package org.sentilo.platform.server.test.request.interceptor;

import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.apache.commons.codec.digest.Md5Crypt;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.config.SentiloArtifactConfigService;
import org.sentilo.common.enums.HttpHeader;
import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.platform.common.domain.EntityMetadataMessage;
import org.sentilo.platform.common.exception.InboundRateLimiterException;
import org.sentilo.platform.common.ratelimiter.QuotaContext;
import org.sentilo.platform.common.ratelimiter.QuotaContextHolder;
import org.sentilo.platform.common.ratelimiter.service.RateLimiterService;
import org.sentilo.platform.common.security.RequesterContext;
import org.sentilo.platform.common.security.RequesterContextHolder;
import org.sentilo.platform.common.service.InternalAlarmService;
import org.sentilo.platform.server.auth.AuthenticationService;
import org.sentilo.platform.server.exception.MessageValidationException;
import org.sentilo.platform.server.exception.SSLRequiredException;
import org.sentilo.platform.server.exception.UnauthorizedException;
import org.sentilo.platform.server.request.SentiloRequest;
import org.sentilo.platform.server.request.interceptor.BodyLengthInterceptor;
import org.sentilo.platform.server.request.interceptor.CredentialInterceptor;
import org.sentilo.platform.server.request.interceptor.RateLimiterInterceptor;
import org.sentilo.platform.server.request.interceptor.SSLAccessInterceptor;
import org.springframework.test.util.ReflectionTestUtils;

public class SentiloRequestHandlerInterceptorsTest {

  @Mock
  private SentiloRequest sRequest;

  @Mock
  private EntityMetadataMessage entityMetadataMessage;

  @Mock
  private AuthenticationService authenticationService;

  @Mock
  private RateLimiterService rateLimiterService;

  @Mock
  private InternalAlarmService internalAlarmService;

  @Mock
  private SentiloArtifactConfigService serviceConfig;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
  }

  @After
  public void tearDown() {
    RequesterContextHolder.clearContext();
    QuotaContextHolder.clearContext();
  }

  @Test
  public void pass_bodyLengthInterceptor() {
    final String provider = "mockProvider";
    final BodyLengthInterceptor srhi = new BodyLengthInterceptor();
    ReflectionTestUtils.setField(srhi, "maxLength", 500l);
    ReflectionTestUtils.setField(srhi, "serviceConfig", serviceConfig);

    when(serviceConfig.getConfigValue("catalog.id", SentiloConstants.DEFAULT_CATALOG_ID)).thenReturn(SentiloConstants.DEFAULT_CATALOG_ID);
    when(sRequest.getEntitySource()).thenReturn(provider);
    when(sRequest.getContentLength()).thenReturn(0l);

    srhi.invoke(sRequest);
  }

  @Test
  public void no_apply_bodyLengthInterceptor() {
    final BodyLengthInterceptor srhi = new BodyLengthInterceptor();
    ReflectionTestUtils.setField(srhi, "maxLength", 500l);
    ReflectionTestUtils.setField(srhi, "serviceConfig", serviceConfig);

    when(serviceConfig.getConfigValue("catalog.id", SentiloConstants.DEFAULT_CATALOG_ID)).thenReturn(SentiloConstants.DEFAULT_CATALOG_ID);
    when(sRequest.getEntitySource()).thenReturn(SentiloConstants.DEFAULT_CATALOG_ID);

    srhi.invoke(sRequest);

    verify(sRequest, times(0)).getContentLength();

  }

  @Test(expected = MessageValidationException.class)
  public void error_when_bodyLengthInterceptor() {
    final String provider = "mockProvider";
    final BodyLengthInterceptor srhi = new BodyLengthInterceptor();
    ReflectionTestUtils.setField(srhi, "maxLength", 500l);
    ReflectionTestUtils.setField(srhi, "serviceConfig", serviceConfig);

    when(serviceConfig.getConfigValue("catalog.id", SentiloConstants.DEFAULT_CATALOG_ID)).thenReturn(SentiloConstants.DEFAULT_CATALOG_ID);
    when(sRequest.getEntitySource()).thenReturn(provider);
    when(sRequest.getContentLength()).thenReturn(1000l);

    srhi.invoke(sRequest);
  }

  @Test
  public void pass_sslAccessInterceptor() {
    final SSLAccessInterceptor srhi = new SSLAccessInterceptor();
    RequesterContextHolder.setContext(new RequesterContext(entityMetadataMessage));

    when(sRequest.extractHeader(HttpHeader.X_FORWARDED_PROTO)).thenReturn("https");
    when(entityMetadataMessage.isRestHttps()).thenReturn(Boolean.TRUE);

    srhi.invoke(sRequest);
  }

  @Test(expected = SSLRequiredException.class)
  public void error_when_sslAccessInterceptor() {
    final SSLAccessInterceptor srhi = new SSLAccessInterceptor();
    RequesterContextHolder.setContext(new RequesterContext(entityMetadataMessage));

    when(sRequest.extractHeader(HttpHeader.X_FORWARDED_PROTO)).thenReturn("http");
    when(entityMetadataMessage.isRestHttps()).thenReturn(Boolean.TRUE);

    srhi.invoke(sRequest);
  }

  @Test
  public void pass_credentialInterceptor() {
    final CredentialInterceptor srhi = new CredentialInterceptor();
    ReflectionTestUtils.setField(srhi, "authenticationService", authenticationService);
    final String identity_token = Md5Crypt.apr1Crypt("123456789");

    when(sRequest.extractHeader(HttpHeader.IDENTITY_KEY)).thenReturn(identity_token);

    srhi.invoke(sRequest);
  }

  @Test(expected = UnauthorizedException.class)
  public void error_when_credentialInterceptor() {
    final CredentialInterceptor srhi = new CredentialInterceptor();
    ReflectionTestUtils.setField(srhi, "authenticationService", authenticationService);
    final String identity_token = Md5Crypt.apr1Crypt("123456789");

    when(sRequest.extractHeader(HttpHeader.IDENTITY_KEY)).thenReturn(identity_token);
    doThrow(UnauthorizedException.class).when(authenticationService).checkCredential(identity_token);

    srhi.invoke(sRequest);
  }

  @Test
  public void pass_rateLimiterInterceptor() {
    RequesterContextHolder.setContext(new RequesterContext(entityMetadataMessage));
    final RateLimiterInterceptor srhi = new RateLimiterInterceptor();
    ReflectionTestUtils.setField(srhi, "rateLimiterService", rateLimiterService);
    ReflectionTestUtils.setField(srhi, "internalAlarmService", internalAlarmService);
    final String provider = "mockProvider";

    when(entityMetadataMessage.getApiInputQuota()).thenReturn(5l);
    when(sRequest.getEntitySource()).thenReturn(provider);
    when(rateLimiterService.allow(provider)).thenReturn(true);

    srhi.invoke(sRequest);

    verify(rateLimiterService).allow(provider);
  }

  @Test
  public void pass_instance_rateLimiterInterceptor() {
    final long globalQuota = 10l;
    RequesterContextHolder.setContext(new RequesterContext(entityMetadataMessage));
    final RateLimiterInterceptor srhi = new RateLimiterInterceptor();
    ReflectionTestUtils.setField(srhi, "rateLimiterService", rateLimiterService);
    ReflectionTestUtils.setField(srhi, "internalAlarmService", internalAlarmService);
    ReflectionTestUtils.setField(srhi, "serviceConfig", serviceConfig);
    ReflectionTestUtils.setField(srhi, "globalQuota", globalQuota);
    final String provider = "mockProvider";

    when(entityMetadataMessage.getApiInputQuota()).thenReturn(5l);
    when(sRequest.getEntitySource()).thenReturn(provider);
    when(serviceConfig.getConfigValue("catalog.id", SentiloConstants.DEFAULT_CATALOG_ID)).thenReturn(SentiloConstants.DEFAULT_CATALOG_ID);
    when(rateLimiterService.allow(RateLimiterService.INSTANCE_ID)).thenReturn(true);
    when(rateLimiterService.allow(provider)).thenReturn(true);

    srhi.invoke(sRequest);

    verify(rateLimiterService).allow(RateLimiterService.INSTANCE_ID);
    verify(rateLimiterService).allow(provider);
  }

  @Test(expected = InboundRateLimiterException.class)
  public void error_when_rateLimiterInterceptor() {
    RequesterContextHolder.setContext(new RequesterContext(entityMetadataMessage));
    final QuotaContext quotaContext = Mockito.mock(QuotaContext.class);

    final RateLimiterInterceptor srhi = new RateLimiterInterceptor();
    ReflectionTestUtils.setField(srhi, "rateLimiterService", rateLimiterService);
    ReflectionTestUtils.setField(srhi, "internalAlarmService", internalAlarmService);
    final String provider = "mockProvider";

    when(quotaContext.getType()).thenReturn(QuotaContext.Type.ENTITY);
    when(entityMetadataMessage.getApiInputQuota()).thenReturn(5l);
    when(sRequest.getEntitySource()).thenReturn(provider);
    when(rateLimiterService.allow(provider)).thenReturn(false);

    QuotaContextHolder.setContext(quotaContext);
    srhi.invoke(sRequest);

    verify(rateLimiterService).allow(provider);
    verify(internalAlarmService).publishInboundRateLimiterAlarm(provider, QuotaContextHolder.getContext(QuotaContext.Type.ENTITY));
  }

  @Test(expected = InboundRateLimiterException.class)
  public void error_when_instance_rateLimiterInterceptor() {
    final long globalQuota = 10l;
    RequesterContextHolder.setContext(new RequesterContext(entityMetadataMessage));
    final QuotaContext quotaContext = Mockito.mock(QuotaContext.class);

    final RateLimiterInterceptor srhi = new RateLimiterInterceptor();
    ReflectionTestUtils.setField(srhi, "rateLimiterService", rateLimiterService);
    ReflectionTestUtils.setField(srhi, "internalAlarmService", internalAlarmService);
    ReflectionTestUtils.setField(srhi, "serviceConfig", serviceConfig);
    ReflectionTestUtils.setField(srhi, "globalQuota", globalQuota);
    final String provider = "mockProvider";

    when(quotaContext.getType()).thenReturn(QuotaContext.Type.GLOBAL);
    when(entityMetadataMessage.getApiInputQuota()).thenReturn(5l);
    when(sRequest.getEntitySource()).thenReturn(provider);
    when(serviceConfig.getConfigValue("catalog.id", SentiloConstants.DEFAULT_CATALOG_ID)).thenReturn(SentiloConstants.DEFAULT_CATALOG_ID);
    when(rateLimiterService.allow(RateLimiterService.INSTANCE_ID)).thenReturn(false);

    QuotaContextHolder.setContext(quotaContext);
    srhi.invoke(sRequest);

    verify(rateLimiterService).allow(RateLimiterService.INSTANCE_ID);
    verify(rateLimiterService, times(0)).allow(provider);
    verify(internalAlarmService).publishInboundRateLimiterAlarm(provider, QuotaContextHolder.getContext(QuotaContext.Type.GLOBAL));
  }
}
