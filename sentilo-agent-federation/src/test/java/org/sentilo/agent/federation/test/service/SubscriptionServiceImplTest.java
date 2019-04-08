package org.sentilo.agent.federation.test.service;

import static org.mockito.Matchers.argThat;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentMatcher;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.agent.federation.domain.FederationConfig;
import org.sentilo.agent.federation.service.LocalPlatformService;
import org.sentilo.agent.federation.service.RemotePlatformService;
import org.sentilo.agent.federation.service.impl.SubscriptionServiceImpl;
import org.sentilo.common.domain.AuthorizedProvider;
import org.sentilo.platform.client.core.domain.Subscription;
import org.springframework.util.CollectionUtils;

public class SubscriptionServiceImplTest {

  private final static String MOCK_PROV_ID_1 = "mockProv1";
  private final static String MOCK_PROV_ID_2 = "mockProv2";
  private final static String MOCK_PROV_ID_3 = "mockProv3";

  private final static String MOCK_PERMISSION = "w";

  @InjectMocks
  private SubscriptionServiceImpl subscriptionService;

  @Mock
  private RemotePlatformService remotePlatformService;

  @Mock
  private LocalPlatformService localPlatformService;

  @Mock
  private FederationConfig fConfig;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void synchronize() {
    when(fConfig.isActive()).thenReturn(true);
    when(remotePlatformService.getPermissions(fConfig)).thenReturn(getRemoteProviders());
    when(remotePlatformService.getSubscriptions(fConfig)).thenReturn(getRemoteSubscriptions());

    subscriptionService.synchronize(fConfig);

    verify(remotePlatformService).createSubscriptions(argThat(new ListMatcher(Arrays.asList(MOCK_PROV_ID_2))), eq(fConfig));
    verify(remotePlatformService).deleteSubscriptions(argThat(new ListMatcher(Arrays.asList(MOCK_PROV_ID_3))), eq(fConfig));
  }

  @Test
  public void disableService() {
    when(fConfig.isActive()).thenReturn(false);
    when(remotePlatformService.deleteSubscriptions(fConfig)).thenReturn(true);

    subscriptionService.synchronize(fConfig);

    verify(remotePlatformService).deleteSubscriptions(fConfig);
    verify(localPlatformService).deleteFederatedConfig(fConfig);
  }

  @Test
  public void disablePartiallyService() {
    when(fConfig.isActive()).thenReturn(false);
    when(remotePlatformService.deleteSubscriptions(fConfig)).thenReturn(false);

    subscriptionService.synchronize(fConfig);

    verify(remotePlatformService).deleteSubscriptions(fConfig);
    verify(localPlatformService, times(0)).deleteFederatedConfig(fConfig);
  }

  private List<AuthorizedProvider> getRemoteProviders() {
    return Arrays.asList(new AuthorizedProvider(MOCK_PROV_ID_1, MOCK_PERMISSION, Collections.emptyList()),
        new AuthorizedProvider(MOCK_PROV_ID_2, MOCK_PERMISSION, Collections.emptyList()));
  }

  private List<Subscription> getRemoteSubscriptions() {
    final Subscription subs1 = new Subscription();
    final Subscription subs2 = new Subscription();

    subs1.setProvider(MOCK_PROV_ID_1);
    subs2.setProvider(MOCK_PROV_ID_3);

    return Arrays.asList(subs1, subs2);
  }

  class ListMatcher extends ArgumentMatcher<List<String>> {

    private final List<String> items;

    public ListMatcher(final List<String> items) {
      this.items = items;
    }

    @SuppressWarnings("unchecked")
    @Override
    public boolean matches(final Object argument) {
      final List<String> values = (List<String>) argument;
      boolean matches = !CollectionUtils.isEmpty(values) && (values.size() == items.size());

      if (matches) {
        // Check if exists a value into values that it isn't in items
        matches = values.stream().filter(value -> !items.contains(value)).collect(Collectors.toList()).size() == 0;
      }

      return matches;
    }

  }

}
