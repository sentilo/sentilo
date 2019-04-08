package org.sentilo.agent.federation.service;

import java.util.List;

import org.sentilo.agent.federation.domain.FederationConfig;

public interface LocalPlatformService {

  FederationConfig getFederatedConfig(final String federatedId);

  List<FederationConfig> getFederatedConfigs();

  String getTokenMasterApp();

  void deleteFederatedConfig(final FederationConfig resource);
}
