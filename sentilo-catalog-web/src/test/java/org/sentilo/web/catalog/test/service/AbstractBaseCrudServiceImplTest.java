package org.sentilo.web.catalog.test.service;

import static org.mockito.Matchers.argThat;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.verify;

import java.util.Arrays;
import java.util.Collection;
import java.util.Map;

import org.mockito.ArgumentMatcher;
import org.mockito.Mock;
import org.sentilo.web.catalog.domain.CatalogDocument;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.utils.CatalogUtils;
import org.sentilo.web.catalog.utils.Constants;
import org.springframework.data.mongodb.core.MongoOperations;
import org.springframework.data.mongodb.core.query.Query;

public class AbstractBaseCrudServiceImplTest extends AbstractBaseServiceImplTest {

  @Mock
  protected MongoOperations mongoOps;

  public <V extends CatalogDocument> void verifyDeleteFromFilter(final SearchFilter filter, final Class<V> resoureType) {
    verify(mongoOps).remove(argThat(new SearchFilterQueryMatcher(filter)), eq(resoureType));
  }

  class FromToQueryMatcher extends ArgumentMatcher<Query> {

    private final Long from;
    private final Long to;

    public FromToQueryMatcher(final Long from, final Long to) {
      this.from = from;
      this.to = to == null ? CatalogUtils.getMaxSystemTimeMillis() : to;
    }

    @Override
    public boolean matches(final Object obj) {
      final Query query = (Query) obj;
      final boolean matchesPage = query.getLimit() == Constants.DEFAULT_CHART_POINTS_NUMBER && query.getSkip() == 0;
      final boolean matchesSort = query.getSortObject().toString().equals("Document{{timestamp=-1}}");
      final boolean matchesQuery =
          from != null ? query.getQueryObject().toString().equals("Document{{timestamp=Document{{$lte=" + to + ", $gte=" + from + "}}}}")
              : query.getQueryObject().toString().equals("Document{{timestamp=Document{{$lte=" + to + "}}}}");

      return matchesPage && matchesSort && matchesQuery;
    }

  }

  class EqualListSizeMatcher<E> extends ArgumentMatcher<Collection<E>> {

    private final Integer size;

    public EqualListSizeMatcher(final Integer size) {
      this.size = size;
    }

    @SuppressWarnings("unchecked")
    @Override
    public boolean matches(final Object obj) {
      final Collection<E> objects = (Collection<E>) obj;

      return objects.size() == size;
    }
  }

  class SearchFilterQueryMatcher extends ArgumentMatcher<Query> {

    private final SearchFilter filter;

    public SearchFilterQueryMatcher(final SearchFilter filter) {
      this.filter = filter;
    }

    @Override
    public boolean matches(final Object obj) {
      final Query query = (Query) obj;
      final String sQuery = query.getQueryObject().toString();

      boolean matchesAndParams = true;
      boolean matchesOrParams = true;

      final Map<String, Object> andParams = filter.getAndParams();
      for (final String field : andParams.keySet()) {
        final String sequenceToCheck = buildAndSequenceToCheck(field, andParams);
        matchesAndParams = matchesAndParams && sQuery.contains(sequenceToCheck);
      }

      final Map<String, Object> orParams = filter.getParams();
      for (final String field : orParams.keySet()) {
        final String sequenceToCheck = buildOrSequenceToCheck(field, orParams);
        matchesOrParams = matchesOrParams && sQuery.contains(sequenceToCheck);
      }

      return matchesAndParams && matchesOrParams;
    }

    private String buildAndSequenceToCheck(final String field, final Map<String, Object> params) {
      String sequenceToCheck = "";
      if (isCollectionValue(params.get(field))) {
        sequenceToCheck = String.format("Document{{%s=Document{{$in=%s}}}}", field, getCollectionValue(params.get(field)));
      } else {
        sequenceToCheck = String.format("Document{{%s=%s}}", field, params.get(field));
      }

      return sequenceToCheck;
    }

    private String buildOrSequenceToCheck(final String field, final Map<String, Object> params) {
      final String sequenceToCheck = String.format("Document{{%s=.*%s.*}}", field, params.get(field));
      // Document{{tenantsAuth=.*mockTenant.*}}
      return sequenceToCheck;
    }

    private boolean isCollectionValue(final Object value) {
      return value != null && (value.getClass().isArray() || Collection.class.isAssignableFrom(value.getClass()));
    }

    private String getCollectionValue(final Object value) {
      if (value.getClass().isArray()) {
        return Arrays.asList((Object[]) value).toString();
      } else {
        return ((Collection<?>) value).toString();
      }
    }

  }

}
