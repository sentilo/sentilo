package org.sentilo.agent.federation.job;

import java.util.List;

import org.sentilo.agent.federation.domain.FederationConfig;
import org.sentilo.agent.federation.service.LocalPlatformService;
import org.sentilo.agent.federation.service.SubscriptionService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

@Component
public class SubscriptionsJob {

  private static final Logger LOGGER = LoggerFactory.getLogger(SubscriptionsJob.class);

  final static int INITIAL_DELAY = 30 * 1000; // 30 seconds
  final static int FIXED_DELAY = 10 * 60 * 1000; // 10 minutes

  @Autowired
  private SubscriptionService subscriptionService;

  @Autowired
  private LocalPlatformService federationConfigService;

  @Scheduled(initialDelay = INITIAL_DELAY, fixedDelay = FIXED_DELAY)
  public void run() {
    LOGGER.info("Running sentilo's federation process to synchronize subscriptions");
    final List<FederationConfig> federatedConfigs = federationConfigService.getFederatedConfigs();
    federatedConfigs.forEach(federatedConfig -> {
      subscriptionService.synchronize(federatedConfig);
    });
  }
}
