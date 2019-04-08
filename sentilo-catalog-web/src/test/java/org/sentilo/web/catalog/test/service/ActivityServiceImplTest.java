package org.sentilo.web.catalog.test.service;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.argThat;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Collection;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.domain.PlatformActivity;
import org.sentilo.common.domain.PlatformMetricsMessage;
import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.web.catalog.context.TenantContextHolder;
import org.sentilo.web.catalog.context.UserConfigContextHolder;
import org.sentilo.web.catalog.context.UserConfigContextImpl;
import org.sentilo.web.catalog.domain.Activity;
import org.sentilo.web.catalog.service.PlatformService;
import org.sentilo.web.catalog.service.impl.ActivityServiceImpl;
import org.sentilo.web.catalog.utils.Constants;
import org.springframework.data.mongodb.core.BulkOperations;
import org.springframework.data.mongodb.core.BulkOperations.BulkMode;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.security.core.context.SecurityContextHolder;

import com.mongodb.bulk.BulkWriteResult;

public class ActivityServiceImplTest extends AbstractBaseCrudServiceImplTest {

  @InjectMocks
  private ActivityServiceImpl activityService;

  @Mock
  private UserConfigContextImpl userConfigContext;

  @Mock
  private PlatformService platformService;

  @Before
  public void setUp() throws Exception {
    TenantContextHolder.clearContext();
    UserConfigContextHolder.clearContext();
    System.clearProperty(SentiloConstants.SENTILO_MULTITENANT_PROP_KEY);

    MockitoAnnotations.initMocks(this);

    UserConfigContextHolder.setContext(userConfigContext);
    when(userConfigContext.getChartVisiblePointsNumber()).thenReturn(Constants.DEFAULT_CHART_POINTS_NUMBER);
  }

  @After
  public void tearDown() {
    System.clearProperty(SentiloConstants.SENTILO_MULTITENANT_PROP_KEY);
    TenantContextHolder.clearContext();
    SecurityContextHolder.clearContext();
  }

  @Test
  public void getInitialLastActivityLogs() {
    final Long from = null, to = null;

    activityService.getLastActivityLogs(from, to);

    verify(mongoOps).find(argThat(new FromToQueryMatcher(from, to)), eq(Activity.class));
  }

  @Test
  public void getLastActivityLogs() {
    final Long from = 1510597015229l, to = 1510597025229l;

    activityService.getLastActivityLogs(from, to);

    verify(mongoOps).find(argThat(new FromToQueryMatcher(from, to)), eq(Activity.class));
  }

  @Test
  public void deleteOldActivityLogs() {
    activityService.deleteOldActivityLogs();

    verify(mongoOps).remove(any(Query.class), eq(Activity.class));
  }

  @Test
  public void getAndStorePlatformActivity() throws Exception {
    final PlatformMetricsMessage pmm = Mockito.mock(PlatformMetricsMessage.class);
    final Collection<PlatformActivity> activities = generateRandomList(PlatformActivity.class);
    final BulkOperations bulkOps = Mockito.mock(BulkOperations.class);
    final BulkWriteResult result = Mockito.mock(BulkWriteResult.class);
    when(platformService.getPlatformActivity()).thenReturn(pmm);
    when(pmm.getActivity()).thenReturn(activities);
    when(mongoOps.bulkOps(BulkMode.UNORDERED, Activity.class)).thenReturn(bulkOps);
    when(bulkOps.execute()).thenReturn(result);

    activityService.getAndStorePlatformActivity();

    verify(bulkOps, times(activities.size())).insert(any(Activity.class));
    verify(bulkOps).execute();
  }
}
