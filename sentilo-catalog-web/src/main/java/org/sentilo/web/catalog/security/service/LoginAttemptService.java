package org.sentilo.web.catalog.security.service;

public interface LoginAttemptService {

  void loginSucceeded(String key);

  void loginFailed(String key);
}
