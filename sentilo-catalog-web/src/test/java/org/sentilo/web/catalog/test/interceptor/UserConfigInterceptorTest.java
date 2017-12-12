package org.sentilo.web.catalog.test.interceptor;

import static org.mockito.Mockito.verify;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.web.catalog.interceptor.UserConfigInterceptor;
import org.sentilo.web.catalog.service.UserConfigService;

public class UserConfigInterceptorTest {

  @InjectMocks
  private UserConfigInterceptor service;

  @Mock
  private UserConfigService userConfigService;

  @Mock
  private HttpServletRequest request;

  @Mock
  private HttpServletResponse response;

  @Mock
  private Object handler;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void preHandle() throws Exception {

    final boolean result = service.preHandle(request, response, handler);

    Assert.assertTrue(result);
    verify(userConfigService).refreshCatalogUserConfigContext();
  }

}
