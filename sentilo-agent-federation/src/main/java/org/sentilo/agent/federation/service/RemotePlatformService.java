package org.sentilo.agent.federation.service;

import java.util.List;

import org.sentilo.agent.federation.domain.FederationConfig;
import org.sentilo.common.domain.AuthorizedProvider;
import org.sentilo.platform.client.core.domain.Subscription;

public interface RemotePlatformService {

  List<AuthorizedProvider> getPermissions(final FederationConfig fConfig);

  List<Subscription> getSubscriptions(final FederationConfig fConfig);

  void createSubscriptions(final List<String> providersIds, final FederationConfig fConfig);

  void deleteSubscriptions(final List<String> providersIds, final FederationConfig fConfig);

  boolean deleteSubscriptions(final FederationConfig fConfig);
}
