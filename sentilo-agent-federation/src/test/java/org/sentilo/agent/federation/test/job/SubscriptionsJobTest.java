package org.sentilo.agent.federation.test.job;

import static org.mockito.Matchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.List;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.agent.federation.domain.FederationConfig;
import org.sentilo.agent.federation.job.SubscriptionsJob;
import org.sentilo.agent.federation.service.LocalPlatformService;
import org.sentilo.agent.federation.service.SubscriptionService;
import org.sentilo.common.test.AbstractBaseTest;

public class SubscriptionsJobTest extends AbstractBaseTest {

  @InjectMocks
  private SubscriptionsJob job;

  @Mock
  private SubscriptionService subscriptionService;

  @Mock
  private LocalPlatformService federationConfigService;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void run() throws Exception {
    final List<FederationConfig> federatedConfigs = generateRandomList(FederationConfig.class);
    when(federationConfigService.getFederatedConfigs()).thenReturn(federatedConfigs);

    job.run();

    verify(federationConfigService).getFederatedConfigs();
    verify(subscriptionService, times(federatedConfigs.size())).synchronize(any(FederationConfig.class));
  }

  @Test
  public void runEmptyList() {
    final List<FederationConfig> federatedConfigs = Collections.emptyList();
    when(federationConfigService.getFederatedConfigs()).thenReturn(federatedConfigs);

    job.run();

    verify(federationConfigService).getFederatedConfigs();
    verify(subscriptionService, times(0)).synchronize(any(FederationConfig.class));
  }

}
