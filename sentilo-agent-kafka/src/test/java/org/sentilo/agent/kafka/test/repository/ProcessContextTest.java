package org.sentilo.agent.kafka.test.repository;

import static org.junit.Assert.assertEquals;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.agent.kafka.repository.ProcessCallback;
import org.sentilo.agent.kafka.repository.ProcessContext;
import org.sentilo.common.domain.EventMessage;
import org.springframework.kafka.core.KafkaTemplate;

public class ProcessContextTest {

  @Mock
  ProcessCallback callback;
  @Mock
  EventMessage eventToProcess;
  @Mock
  KafkaTemplate<String, String> kafkaTemplate;
  int numMaxRetries = 1;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void constructor() {

    final ProcessContext ctx = new ProcessContext(eventToProcess, numMaxRetries, callback, kafkaTemplate);
    assertEquals(callback, ctx.getCallback());
    assertEquals(eventToProcess, ctx.getEventToProcess());
    assertEquals(kafkaTemplate, ctx.getKafkaTemplate());
    assertEquals(numMaxRetries, ctx.getNumMaxRetries());
  }
}
