package org.sentilo.agent.kafka.test.repository;

import java.util.concurrent.ExecutorService;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.agent.kafka.repository.ProcessMonitor;
import org.sentilo.agent.kafka.repository.impl.KafkaAgentRepositoryImpl;
import org.sentilo.common.domain.EventMessage;
import org.springframework.kafka.core.KafkaTemplate;

public class KafkaAgentRepositoryImplTest {

  @InjectMocks
  private KafkaAgentRepositoryImpl repository;

  @Mock
  private ProcessMonitor streamProcessMonitor;

  @Mock
  private EventMessage event;

  @Mock
  private KafkaTemplate<String, String> kafkaTemplate;

  @Mock
  private ExecutorService workersManager;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);

    repository.init();
  }

  @Test
  public void publishMessageToKafka() {
    repository.publishMessageToKafka(event);

  }
}
