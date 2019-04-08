package org.sentilo.web.catalog.test.search;

import java.util.List;

import org.junit.Assert;
import org.junit.Test;
import org.sentilo.common.test.AbstractBaseTest;
import org.sentilo.web.catalog.domain.Sensor;
import org.sentilo.web.catalog.search.SearchFilterResult;

public class SearchFilterResultTest extends AbstractBaseTest {

  @Test
  public void getContent() throws Exception {
    final List<Sensor> content = generateRandomList(Sensor.class);
    final SearchFilterResult<Sensor> result = new SearchFilterResult<Sensor>(content);

    Assert.assertTrue(result.hasContent());
    Assert.assertEquals(content, result.getContent());
    Assert.assertEquals(content.size(), result.getTotalElements());
  }

}
