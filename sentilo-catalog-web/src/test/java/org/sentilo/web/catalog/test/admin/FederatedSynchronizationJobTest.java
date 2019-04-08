package org.sentilo.web.catalog.test.admin;

import static org.mockito.Mockito.verify;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.web.catalog.admin.scheduler.FederatedSynchronizationJob;
import org.sentilo.web.catalog.admin.service.FederatedSynchronizationService;
import org.sentilo.web.catalog.utils.Constants;
import org.springframework.security.core.context.SecurityContextHolder;

public class FederatedSynchronizationJobTest {

  @InjectMocks
  private FederatedSynchronizationJob job;

  @Mock
  private FederatedSynchronizationService service;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);

    System.setProperty(SentiloConstants.SENTILO_FEDERATION_ENABLED_PROP_KEY, "true");
  }

  @Test
  public void syncFederatedServices() {
    job.syncFederatedServices();

    Assert.assertNotNull(SecurityContextHolder.getContext().getAuthentication());
    Assert.assertEquals(Constants.BATCH_USER, SecurityContextHolder.getContext().getAuthentication().getName());
    verify(service).syncCatalogs();
  }

  @After
  public void tearDown() throws Exception {
    SecurityContextHolder.clearContext();
    System.clearProperty(SentiloConstants.SENTILO_FEDERATION_ENABLED_PROP_KEY);
  }
}
