package org.sentilo.web.catalog.security.service.impl;

import org.sentilo.common.cache.LRUCache;
import org.sentilo.common.cache.impl.LRUCacheImpl;
import org.sentilo.web.catalog.domain.User;
import org.sentilo.web.catalog.security.service.LoginAttemptService;
import org.sentilo.web.catalog.service.impl.UserServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

/**
 * Simple control to evict DoS attack. It maintains an in-memory cache to control user failed
 * attempts. If a user exceeds the maximum predefined limit of errors then he is blocked.
 */
@Service
public class LoginUserAttemptServiceImpl implements LoginAttemptService {

  @Autowired
  private UserServiceImpl userService;

  @Value("${login.attempts:3}")
  private int maxAttempts;

  private LRUCache<String, Integer> attemptsCache;

  public LoginUserAttemptServiceImpl() {
    super();
    attemptsCache = new LRUCacheImpl<String, Integer>(10000, 24 * 60);
  }

  @Override
  public void loginFailed(final String userName) {
    Integer currentAttempts = attemptsCache.get(userName, 0);
    currentAttempts++;
    attemptsCache.put(userName, currentAttempts);

    if (userService.exists(userName) && currentAttempts == maxAttempts) {
      final User user = userService.find(new User(userName));
      user.setActive(false);
      userService.getRepository().save(user);
    }
  }

  @Override
  public void loginSucceeded(final String userName) {
    attemptsCache.remove(userName);
  }
}
