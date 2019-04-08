package org.sentilo.web.catalog.test.controller;

import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.sentilo.web.catalog.controller.admin.ActiveSubscriptionsController;
import org.sentilo.web.catalog.domain.ActiveSubscription;
import org.sentilo.web.catalog.service.ActiveSubscriptionsService;
import org.sentilo.web.catalog.service.TenantCustomParamsService;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

public class ActiveSubscriptionsControllerTest {

  @InjectMocks
  private ActiveSubscriptionsController controller;

  @Mock
  private TenantCustomParamsService tenantCustomParamsService;

  @Mock
  private ActiveSubscriptionsService activeSubscriptionsService;

  private MockMvc mockMvc;

  @Before
  public void setup() {
    MockitoAnnotations.initMocks(this);
    mockMvc = MockMvcBuilders.standaloneSetup(controller).build();
  }

  @Test
  public void testGetDetail() throws Exception {
    final ActiveSubscription as = Mockito.mock(ActiveSubscription.class);
    when(as.getId()).thenReturn("3");
    when(activeSubscriptionsService.findAndThrowErrorIfNotExist(eq(as))).thenReturn(as);

    mockMvc.perform(get("/admin/activesubscriptions/3/detail")).andExpect(status().isOk());
  }

}
