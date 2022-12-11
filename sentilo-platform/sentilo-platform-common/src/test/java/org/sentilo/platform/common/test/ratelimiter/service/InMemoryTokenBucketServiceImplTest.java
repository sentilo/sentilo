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
package org.sentilo.platform.common.test.ratelimiter.service;

import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.concurrent.locks.Lock;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.lock.LockFactory;
import org.sentilo.platform.common.domain.EntityMetadataMessage;
import org.sentilo.platform.common.ratelimiter.QuotaContext.Type;
import org.sentilo.platform.common.ratelimiter.QuotaContextHolder;
import org.sentilo.platform.common.ratelimiter.RateLimiterStatus;
import org.sentilo.platform.common.ratelimiter.service.InMemoryTokenBucketServiceImpl;
import org.sentilo.platform.common.security.RequesterContext;
import org.sentilo.platform.common.security.RequesterContextHolder;
import org.sentilo.platform.common.security.repository.EntityMetadataRepository;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.util.CollectionUtils;

public class InMemoryTokenBucketServiceImplTest {

  final Long entityOutputLimitQuota = 3l;

  @InjectMocks
  private InMemoryTokenBucketServiceImpl service;

  @Mock
  private LockFactory lockProvider;

  @Mock
  private EntityMetadataMessage entityMetadata;

  @Mock
  private EntityMetadataRepository entityMetadataRepository;

  @Mock
  private Lock lock;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);

    final RequesterContext rc = new RequesterContext(entityMetadata);
    RequesterContextHolder.setContext(rc);
    when(entityMetadataRepository.getEntityMetadataFromId("mockEntity")).thenReturn(entityMetadata);
    when(entityMetadata.getApiOutputQuota()).thenReturn(entityOutputLimitQuota);
    when(lockProvider.getLock("rl_token_bucket")).thenReturn(lock);

    service.init();
  }

  @After
  public void tearDowm() throws Exception {
    QuotaContextHolder.clearContext();
    RequesterContextHolder.clearContext();
  }

  @Test
  public void init() {
    Assert.assertEquals(lock, ReflectionTestUtils.getField(service, "lock"));
  }

  @Test
  public void allow_when_no_limit() {
    when(entityMetadata.getApiOutputQuota()).thenReturn(0l);

    final boolean allow = service.allow("mockEntity");

    Assert.assertTrue(allow);
  }

  @Test
  public void allow() {
    final boolean allow = service.allow("mockEntity");

    Assert.assertTrue(allow);
    Assert.assertTrue(QuotaContextHolder.getContext(Type.ENTITY).getMinutesToReset() > 0);
    verify(lock).lock();
    verify(lock).unlock();
  }

  @Test
  public void no_allow_when_overlimit() {
    // Account has a limit of 3 output requests by hour
    service.allow("mockEntity");
    service.allow("mockEntity");
    final boolean allow = service.allow("mockEntity");
    final boolean allow2 = service.allow("mockEntity");

    Assert.assertTrue(allow);
    Assert.assertFalse(allow2);
    Assert.assertTrue(QuotaContextHolder.getContext(Type.ENTITY).getMinutesToReset() > 0);
    verify(lock, times(4)).lock();
    verify(lock, times(4)).unlock();
  }

  @Test
  public void getAccountsStatus_when_init() {
    final RateLimiterStatus rls = service.getAccountsStatus();

    Assert.assertTrue(CollectionUtils.isEmpty(rls.getAccounts()));
  }

  @Test
  public void getAccountsStatus() {
    // Initialize account status with data
    service.allow("mockEntity");
    service.allow("mockEntity");

    final RateLimiterStatus rls = service.getAccountsStatus();

    Assert.assertFalse(CollectionUtils.isEmpty(rls.getAccounts()));
    Assert.assertNotNull(rls.getAccounts().get("mockEntity"));
    Assert.assertEquals(entityOutputLimitQuota, rls.getAccounts().get("mockEntity").get("quota"));
    Assert.assertEquals(2l, rls.getAccounts().get("mockEntity").get("total"));
  }

}
