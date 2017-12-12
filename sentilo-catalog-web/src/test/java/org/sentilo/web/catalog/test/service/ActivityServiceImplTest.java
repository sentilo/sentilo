package org.sentilo.web.catalog.test.service;

import static org.mockito.Matchers.argThat;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.verify;

import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentMatcher;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.test.AbstractBaseTest;
import org.sentilo.web.catalog.domain.Activity;
import org.sentilo.web.catalog.service.impl.ActivityServiceImpl;
import org.sentilo.web.catalog.utils.CatalogUtils;
import org.springframework.data.mongodb.core.MongoOperations;
import org.springframework.data.mongodb.core.query.Query;

public class ActivityServiceImplTest extends AbstractBaseTest {

  @InjectMocks
  private ActivityServiceImpl activityService;

  @Mock
  private MongoOperations mongoOperations;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void getInitialLastActivityLogs() {
    final Long from = null, to = null;

    activityService.getLastActivityLogs(from, to);

    verify(mongoOperations).find(argThat(new QueryMatcher(from, to)), eq(Activity.class));
  }

  @Test
  public void getLastActivityLogs() {
    final Long from = 1510597015229l, to = 1510597025229l;

    activityService.getLastActivityLogs(from, to);

    verify(mongoOperations).find(argThat(new QueryMatcher(from, to)), eq(Activity.class));
  }

  class QueryMatcher extends ArgumentMatcher<Query> {

    private final Long from;
    private final Long to;

    public QueryMatcher(final Long from, final Long to) {
      this.from = from;
      this.to = to == null ? CatalogUtils.getMaxSystemTimeMillis() : to;
    }

    @Override
    public boolean matches(final Object argument) {
      final Query query = (Query) argument;
      final boolean matchesPage = query.getLimit() == 20 && query.getSkip() == 0;
      final boolean matchesSort = query.getSortObject().toString().equals("{ \"timestamp\" : -1}");
      final boolean matchesQuery =
          from != null ? query.getQueryObject().toString().equals("{ \"timestamp\" : { \"$lte\" : " + to + " , \"$gte\" : " + from + "}}")
              : query.getQueryObject().toString().equals("{ \"timestamp\" : { \"$lte\" : " + to + "}}");

      return matchesPage && matchesSort && matchesQuery;
    }

  }
}
