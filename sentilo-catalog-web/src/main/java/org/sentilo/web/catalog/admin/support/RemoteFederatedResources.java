package org.sentilo.web.catalog.admin.support;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.collections.CollectionUtils;
import org.sentilo.common.domain.AuthorizedProvider;
import org.sentilo.common.domain.CatalogComponent;
import org.sentilo.common.domain.CatalogSensor;
import org.sentilo.web.catalog.converter.CatalogDocumentConverter;
import org.sentilo.web.catalog.domain.FederationConfig;
import org.sentilo.web.catalog.utils.CompoundKeyBuilder;
import org.sentilo.web.catalog.utils.TenantUtils;
import org.springframework.util.StringUtils;

public class RemoteFederatedResources {

  private final FederationConfig fConfig;
  private final List<AuthorizedProvider> resources;
  private Set<String> providers;
  private Map<String, CatalogComponent> components;
  private Map<String, CatalogSensor> sensors;

  public RemoteFederatedResources(final FederationConfig fConfig, final List<AuthorizedProvider> resources) {
    super();
    this.fConfig = fConfig;
    this.resources = CollectionUtils.isEmpty(resources) ? Collections.<AuthorizedProvider>emptyList() : resources;
    splitResourcesByType();
  }

  public Set<String> getProviders() {
    return providers;
  }

  public Map<String, CatalogComponent> getComponents() {
    return components;
  }

  public Map<String, CatalogSensor> getSensors() {
    return sensors;
  }

  private String buildLocalProviderId(final FederationConfig fConfig, final String remoteProviderId) {
    // If fConfig belongs to a tenant, providerId should be formatted as
    // tenantId@federatedId_providerId,
    // otherwise it should be federatedId_providerId
    String localProviderId = String.format("%s_%s", fConfig.getId(), remoteProviderId);
    if (StringUtils.hasText(fConfig.getTenantId())) {
      localProviderId = TenantUtils.buildResourceIdWithTenant(fConfig.getTenantId(), localProviderId);
    }

    return localProviderId;
  }

  private void splitResourcesByType() {
    // This method splits remote resources into 3 groups: providers, components and sensors
    providers = new HashSet<String>();
    components = new HashMap<String, CatalogComponent>();
    sensors = new HashMap<String, CatalogSensor>();

    for (final AuthorizedProvider item : resources) {
      final String remoteProviderId = item.getProvider();
      final String localProviderId = buildLocalProviderId(fConfig, remoteProviderId);
      providers.add(localProviderId);

      final List<CatalogSensor> remoteComponentAndSensors = item.getSensors();
      for (final CatalogSensor remoteComponentAndSensor : remoteComponentAndSensors) {
        final String componentId = CompoundKeyBuilder.buildCompoundKey(localProviderId, remoteComponentAndSensor.getComponent());
        final String sensorId = CompoundKeyBuilder.buildCompoundKey(componentId, remoteComponentAndSensor.getSensor());
        remoteComponentAndSensor.setProvider(localProviderId);
        // Remote component fields are always wrapped into a CatalogSensor
        components.put(componentId, CatalogDocumentConverter.extractCatalogComponent(remoteComponentAndSensor));
        sensors.put(sensorId, remoteComponentAndSensor);
      }
    }
  }
}
