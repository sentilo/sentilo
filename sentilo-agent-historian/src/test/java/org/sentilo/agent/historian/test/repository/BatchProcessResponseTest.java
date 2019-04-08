package org.sentilo.agent.historian.test.repository;

import org.junit.Assert;
import org.junit.Test;
import org.sentilo.agent.historian.repository.batch.BatchProcessResponse;

public class BatchProcessResponseTest {

  @Test
  public void errors() {
    final BatchProcessResponse response = new BatchProcessResponse();
    final BatchProcessResponse responseWithErrors = new BatchProcessResponse(true);

    Assert.assertFalse(response.hasAllItemsRejected());
    Assert.assertTrue(responseWithErrors.hasAllItemsRejected());
  }

}
