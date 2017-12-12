package org.sentilo.web.catalog.test.service;

import static org.mockito.Mockito.verify;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.web.catalog.service.PlatformService;
import org.sentilo.web.catalog.service.impl.SubscriptionServiceImpl;

public class SubscriptionServiceImplTest {

  @Mock
  private PlatformService platformService;

  @InjectMocks
  private SubscriptionServiceImpl service;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void getActiveSubscriptions() {
    final String entityId = "mockId";

    service.getActiveSubscriptions(entityId);

    verify(platformService).getActiveSubscriptions(entityId);
  }

}
