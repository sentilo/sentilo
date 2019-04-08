package org.sentilo.web.catalog.security.event;

import org.sentilo.web.catalog.security.service.LoginAttemptService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationListener;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.authentication.event.AuthenticationFailureBadCredentialsEvent;
import org.springframework.stereotype.Component;

@Component
public class AuthenticationFailureListener implements ApplicationListener<AuthenticationFailureBadCredentialsEvent> {

  @Autowired
  private LoginAttemptService loginAttemptService;

  public void onApplicationEvent(final AuthenticationFailureBadCredentialsEvent e) {
    final UsernamePasswordAuthenticationToken source = (UsernamePasswordAuthenticationToken) e.getSource();
    loginAttemptService.loginFailed(source.getPrincipal().toString());
  }
}
