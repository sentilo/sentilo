package org.sentilo.web.catalog.test.security.service;

import static org.mockito.Matchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.cache.LRUCache;
import org.sentilo.web.catalog.domain.User;
import org.sentilo.web.catalog.repository.UserRepository;
import org.sentilo.web.catalog.security.service.impl.LoginUserAttemptServiceImpl;
import org.sentilo.web.catalog.service.impl.UserServiceImpl;
import org.springframework.test.util.ReflectionTestUtils;

public class LoginUserAttemptServiceImplTest {

  private static Integer MAX_ATTEMPTS = 3;

  @Mock
  private UserServiceImpl userService;

  @Mock
  private UserRepository repository;

  @Mock
  private User user;

  @InjectMocks
  private LoginUserAttemptServiceImpl loginService;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    ReflectionTestUtils.setField(loginService, "maxAttempts", MAX_ATTEMPTS);
  }

  @SuppressWarnings({"unchecked", "rawtypes"})
  @Test
  public void loginSucceeded() {
    final String userName = "mockUserName";
    final LRUCache<String, Integer> attemptsCache = (LRUCache) ReflectionTestUtils.getField(loginService, "attemptsCache");

    loginService.loginSucceeded(userName);

    Assert.assertFalse(attemptsCache.contains(userName));
  }

  @SuppressWarnings({"unchecked", "rawtypes"})
  @Test
  public void loginFailed() {
    final String userName = "mockWrongUserName";
    final LRUCache<String, Integer> attemptsCache = (LRUCache) ReflectionTestUtils.getField(loginService, "attemptsCache");

    when(userService.exists(userName)).thenReturn(true);
    when(userService.find(any(User.class))).thenReturn(user);
    when(userService.getRepository()).thenReturn(repository);

    for (int i = 0; i < MAX_ATTEMPTS; i++) {
      loginService.loginFailed(userName);
    }

    Assert.assertTrue(attemptsCache.contains(userName));
    Assert.assertEquals(MAX_ATTEMPTS, attemptsCache.get(userName));
    verify(repository, times(1)).save(user);
  }

}
