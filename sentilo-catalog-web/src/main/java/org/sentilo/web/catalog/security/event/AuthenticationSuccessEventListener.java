package org.sentilo.web.catalog.security.event;

import org.sentilo.web.catalog.security.service.LoginAttemptService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationListener;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.authentication.event.AuthenticationSuccessEvent;
import org.springframework.stereotype.Component;

@Component
public class AuthenticationSuccessEventListener implements ApplicationListener<AuthenticationSuccessEvent> {

  @Autowired
  private LoginAttemptService loginAttemptService;

  public void onApplicationEvent(final AuthenticationSuccessEvent e) {
    final UsernamePasswordAuthenticationToken source = (UsernamePasswordAuthenticationToken) e.getSource();
    loginAttemptService.loginSucceeded(source.getPrincipal().toString());
  }
}
