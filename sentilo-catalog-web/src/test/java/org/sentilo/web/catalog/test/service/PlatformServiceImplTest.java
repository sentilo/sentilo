package org.sentilo.web.catalog.test.service;

import static org.mockito.Matchers.argThat;
import static org.mockito.Mockito.verify;

import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentMatcher;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.rest.RESTClient;
import org.sentilo.common.rest.RequestContext;
import org.sentilo.web.catalog.domain.PlatformAdminInputMessage;
import org.sentilo.web.catalog.service.impl.PlatformServiceImpl;

public class PlatformServiceImplTest {

  @Mock
  private RESTClient restClient;

  @InjectMocks
  private PlatformServiceImpl service;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void getCurrentPlatformStats() {
    service.getCurrentPlatformStats();

    verify(restClient).get(argThat(new PathRequestContextMatcher("admin/stats")));
  }

  @Test
  public void getPlatformActivity() {
    service.getPlatformActivity();

    verify(restClient).get(argThat(new PathRequestContextMatcher("admin/activity")));
  }

  @Test
  public void getPlatformPerformance() {
    service.getPlatformPerformance();

    verify(restClient).get(argThat(new PathRequestContextMatcher("admin/performance")));
  }

  @Test
  public void saveResources() {
    final PlatformAdminInputMessage paim = new PlatformAdminInputMessage();
    service.saveResources(paim);

    verify(restClient).post(argThat(new PathRequestContextMatcher("admin/save")));
  }

  @Test
  public void deleteResources() {
    final PlatformAdminInputMessage paim = new PlatformAdminInputMessage();
    service.deleteResources(paim);

    verify(restClient).put(argThat(new PathRequestContextMatcher("admin/delete")));
  }

  @Test
  public void getActiveSubscriptions() {
    final String entityId = "mockId";
    final String path = String.format("admin/subscriptions/%s", entityId);
    service.getActiveSubscriptions(entityId);

    verify(restClient).get(argThat(new PathRequestContextMatcher(path)));
  }

  class PathRequestContextMatcher extends ArgumentMatcher<RequestContext> {

    final String path;

    public PathRequestContextMatcher(final String path) {
      this.path = path;
    }

    @Override
    public boolean matches(final Object argument) {
      final RequestContext rc = (RequestContext) argument;
      return this.path.equals(rc.getPath());
    }

  }

}
