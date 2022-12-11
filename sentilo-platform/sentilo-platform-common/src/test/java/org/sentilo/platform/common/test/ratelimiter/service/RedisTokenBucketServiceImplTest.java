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

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyVararg;
import static org.mockito.Matchers.argThat;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Lock;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.lock.LockFactory;
import org.sentilo.common.test.AbstractBaseTest;
import org.sentilo.platform.common.domain.EntityMetadataMessage;
import org.sentilo.platform.common.ratelimiter.QuotaContext.Type;
import org.sentilo.platform.common.ratelimiter.QuotaContextHolder;
import org.sentilo.platform.common.ratelimiter.RateLimiterStatus;
import org.sentilo.platform.common.ratelimiter.service.RedisTokenBucketServiceImpl;
import org.sentilo.platform.common.security.RequesterContext;
import org.sentilo.platform.common.security.RequesterContextHolder;
import org.sentilo.platform.common.security.repository.EntityMetadataRepository;
import org.springframework.data.redis.core.HashOperations;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.util.CollectionUtils;

public class RedisTokenBucketServiceImplTest extends AbstractBaseTest {

  final Long entityOutputLimitQuota = 3l;

  @InjectMocks
  private RedisTokenBucketServiceImpl service;

  @Mock
  private StringRedisTemplate redisTemplate;

  @Mock
  private LockFactory lockProvider;

  @Mock
  private EntityMetadataMessage entityMetadata;

  @Mock
  private EntityMetadataRepository entityMetadataRepository;

  @Mock
  private Lock lock;

  @Mock
  private HashOperations<String, Object, Object> hashOps;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);

    final RequesterContext rc = new RequesterContext(entityMetadata);
    RequesterContextHolder.setContext(rc);
    when(entityMetadataRepository.getEntityMetadataFromId("mockEntity")).thenReturn(entityMetadata);
    when(entityMetadata.getApiOutputQuota()).thenReturn(entityOutputLimitQuota);
    when(lockProvider.getLock("rl_token_bucket")).thenReturn(lock);
    when(redisTemplate.opsForHash()).thenReturn(hashOps);

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
  public void allow_first_request() {
    final Object[] responseValues = {entityOutputLimitQuota, 2l, 3600l};
    final List<?> response = Arrays.asList(responseValues);
    when(redisTemplate.execute(any(), argThat(new ListMatcher(Arrays.asList("rl:out:account:mockEntity"))), anyVararg())).thenReturn(response);

    final boolean allow = service.allow("mockEntity");

    Assert.assertTrue(allow);
    Assert.assertTrue(QuotaContextHolder.getContext(Type.ENTITY).getMinutesToReset() == TimeUnit.SECONDS.toMinutes(3600));
    Assert.assertEquals(1, QuotaContextHolder.getContext(Type.ENTITY).getCurrent());
    Assert.assertEquals(2, QuotaContextHolder.getContext(Type.ENTITY).getRemaining());
    verify(lock).lock();
    verify(lock).unlock();
  }

  @Test
  public void allow() {
    final Object[] responseValues = {entityOutputLimitQuota, 1l, 3000l};
    final List<?> response = Arrays.asList(responseValues);
    when(redisTemplate.execute(any(), argThat(new ListMatcher(Arrays.asList("rl:out:account:mockEntity"))), anyVararg())).thenReturn(response);

    final boolean allow = service.allow("mockEntity");

    Assert.assertTrue(allow);
    Assert.assertTrue(QuotaContextHolder.getContext(Type.ENTITY).getMinutesToReset() == TimeUnit.SECONDS.toMinutes(3000));
    Assert.assertEquals(2, QuotaContextHolder.getContext(Type.ENTITY).getCurrent());
    Assert.assertEquals(1, QuotaContextHolder.getContext(Type.ENTITY).getRemaining());
    verify(lock).lock();
    verify(lock).unlock();
  }

  @Test
  public void no_allow_when_overlimit() {
    final Object[] responseValues = {entityOutputLimitQuota, 0l, 1800l};
    final List<?> response = Arrays.asList(responseValues);
    when(redisTemplate.execute(any(), argThat(new ListMatcher(Arrays.asList("rl:out:account:mockEntity"))), anyVararg())).thenReturn(response);

    final boolean allow = service.allow("mockEntity");

    Assert.assertFalse(allow);
    Assert.assertTrue(QuotaContextHolder.getContext(Type.ENTITY).getMinutesToReset() == TimeUnit.SECONDS.toMinutes(1800));
    Assert.assertEquals(3, QuotaContextHolder.getContext(Type.ENTITY).getCurrent());
    Assert.assertEquals(0, QuotaContextHolder.getContext(Type.ENTITY).getRemaining());
    verify(lock).lock();
    verify(lock).unlock();
  }

  @Test
  public void getAccountsStatus_when_init() {
    final RateLimiterStatus rls = service.getAccountsStatus();

    Assert.assertTrue(CollectionUtils.isEmpty(rls.getAccounts()));
  }

  @Test
  public void getAccountsStatus() {
    final Object[] responseValues = {entityOutputLimitQuota, 1l, 1800l};
    final List<?> response = Arrays.asList(responseValues);
    final Map<Object, Object> accountEntries = new HashMap<Object, Object>();
    accountEntries.put("mockEntity", "3");
    accountEntries.put("expiredEntity", "3");
    when(hashOps.entries("rl:out:accounts")).thenReturn(accountEntries);
    when(redisTemplate.hasKey("rl:out:account:mockEntity")).thenReturn(true);
    when(redisTemplate.hasKey("rl:out:account:expiredEntity")).thenReturn(false);
    when(redisTemplate.execute(any(), argThat(new ListMatcher(Arrays.asList("rl:out:account:mockEntity"))), anyVararg())).thenReturn(response);

    final RateLimiterStatus rls = service.getAccountsStatus();

    Assert.assertFalse(CollectionUtils.isEmpty(rls.getAccounts()));
    Assert.assertNotNull(rls.getAccounts().get("mockEntity"));
    Assert.assertEquals(entityOutputLimitQuota, rls.getAccounts().get("mockEntity").get("quota"));
    Assert.assertEquals(2l, rls.getAccounts().get("mockEntity").get("total"));
    verify(hashOps).delete(eq("rl:out:accounts"), eq("expiredEntity"));
  }


}
