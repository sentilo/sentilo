package org.sentilo.agent.federation.test.service;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.List;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.agent.federation.domain.Application;
import org.sentilo.agent.federation.domain.FederationConfig;
import org.sentilo.agent.federation.service.impl.LocalPlatformServiceImpl;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.query.Query;

public class LocalPlatformServiceImplTest {

  private final static String MOCK_TOKEN = "123456789";

  @InjectMocks
  private LocalPlatformServiceImpl localService;

  @Mock
  private MongoTemplate mongoOps;

  @Mock
  private FederationConfig fConfig;

  @Mock
  private Application masterApp;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void getFederatedConfig() {
    final String federatedServerId = "FED1";

    when(fConfig.getId()).thenReturn(federatedServerId);
    when(mongoOps.findOne(any(Query.class), eq(FederationConfig.class))).thenReturn(fConfig);
    when(fConfig.getAppClientToken()).thenReturn(MOCK_TOKEN);

    localService.getFederatedConfig(federatedServerId);
    localService.getFederatedConfig(federatedServerId);

    verify(mongoOps, times(1)).findOne(any(Query.class), eq(FederationConfig.class));
  }

  @Test
  public void getFederatedConfigs() {
    final List<FederationConfig> fConfigs = Arrays.asList(fConfig);
    when(mongoOps.findAll(FederationConfig.class)).thenReturn(fConfigs);

    final List<FederationConfig> result = localService.getFederatedConfigs();

    verify(mongoOps).findAll(eq(FederationConfig.class));
    Assert.assertEquals(fConfigs, result);
  }

  @Test
  public void getTokenMasterApp() {
    when(mongoOps.findOne(any(Query.class), eq(Application.class))).thenReturn(masterApp);
    when(masterApp.getToken()).thenReturn(MOCK_TOKEN);

    final String result1 = localService.getTokenMasterApp();
    final String result2 = localService.getTokenMasterApp();

    Assert.assertEquals(MOCK_TOKEN, result1);
    Assert.assertEquals(MOCK_TOKEN, result2);
    verify(mongoOps, times(1)).findOne(any(Query.class), eq(Application.class));
  }

}
