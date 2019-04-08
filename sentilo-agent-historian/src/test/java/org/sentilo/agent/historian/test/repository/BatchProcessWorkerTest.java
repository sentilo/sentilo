package org.sentilo.agent.historian.test.repository;

import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.argThat;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentMatcher;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.agent.historian.repository.batch.BatchProcessCallback;
import org.sentilo.agent.historian.repository.batch.BatchProcessContext;
import org.sentilo.agent.historian.repository.batch.BatchProcessResult;
import org.sentilo.agent.historian.repository.batch.BatchProcessWorker;
import org.sentilo.common.domain.EventMessage;
import org.sentilo.common.exception.RESTClientException;
import org.sentilo.common.rest.RESTClient;
import org.sentilo.common.rest.RequestContext;
import org.springframework.util.CollectionUtils;

public class BatchProcessWorkerTest {

  @Mock
  private BatchProcessContext context;

  @Mock
  private BatchProcessCallback callback;

  @Mock
  private RESTClient restClient;

  @Mock
  private EventMessage eventMessage;

  private BatchProcessWorker worker;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    when(context.getCallback()).thenReturn(callback);
    when(context.getRestClient()).thenReturn(restClient);
    when(context.getNumMaxRetries()).thenReturn(3);
    when(context.getEventsToProcess()).thenReturn(buildMockList(50));
    when(eventMessage.getType()).thenReturn("data");

    worker = new BatchProcessWorker(context);
  }

  @Test
  public void call() {
    when(restClient.post(argThat(new PathRequestContextMatcher()))).thenReturn(anyString());

    final BatchProcessResult result = worker.call();

    verify(restClient).post(argThat(new PathRequestContextMatcher()));
    verify(callback).notifyBatchProcessIsDone(result);
    Assert.assertTrue(CollectionUtils.isEmpty(result.getPendingEvents()));
  }

  @SuppressWarnings("unchecked")
  @Test
  public void callWithErrors() {
    when(restClient.post(argThat(new PathRequestContextMatcher()))).thenThrow(RESTClientException.class);

    final BatchProcessResult result = worker.call();

    verify(restClient, times(context.getNumMaxRetries())).post(argThat(new PathRequestContextMatcher()));
    verify(callback).notifyBatchProcessIsDone(result);
    Assert.assertFalse(CollectionUtils.isEmpty(result.getPendingEvents()));
    Assert.assertTrue(result.getPendingEvents().size() == context.getEventsToProcess().size());
  }

  private List<EventMessage> buildMockList(final long total) {
    final List<EventMessage> resources = new ArrayList<EventMessage>();
    for (int i = 0; i < total; i++) {
      final EventMessage event = new EventMessage();
      event.setTime(System.currentTimeMillis());
      event.setType("data");
      event.setTopic("/data/mockProvider/mockSensor");
      event.setMessage(Integer.toString(i));
      resources.add(event);
    }

    return resources;
  }

  class PathRequestContextMatcher extends ArgumentMatcher<RequestContext> {

    @Override
    public boolean matches(final Object argument) {
      final RequestContext rc = (RequestContext) argument;
      return "/api/put?details".equals(rc.getPath());
    }

  }
}
