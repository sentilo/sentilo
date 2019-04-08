package org.sentilo.agent.federation.service;

import org.sentilo.agent.federation.domain.FederationConfig;

public interface SubscriptionService {

  void synchronize(final FederationConfig fConfig);
}
