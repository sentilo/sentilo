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
