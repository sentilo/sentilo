package org.sentilo.agent.historian.test.repository;

import static org.mockito.Matchers.anyListOf;
import static org.mockito.Matchers.anyLong;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.List;

import org.junit.Assert;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import org.sentilo.agent.common.repository.PendingEventsRepository;
import org.sentilo.agent.historian.repository.batch.BatchProcessMonitor;
import org.sentilo.agent.historian.repository.batch.BatchProcessResult;
import org.sentilo.common.domain.EventMessage;
import org.sentilo.common.test.AbstractBaseTest;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.test.util.ReflectionTestUtils;

@RunWith(PowerMockRunner.class)
@PrepareForTest({LoggerFactory.class})
public class BatchProcessMonitorTest extends AbstractBaseTest {

  @InjectMocks
  private BatchProcessMonitor batchProcessMonitor;

  @Mock
  private PendingEventsRepository pendingEventRepository;

  @Mock
  private BatchProcessResult result;

  private static Logger logger;

  @BeforeClass
  public static void setUpStatic() throws Exception {
    PowerMockito.mockStatic(LoggerFactory.class);
    logger = PowerMockito.mock(Logger.class);
    when(LoggerFactory.getLogger(anyString())).thenReturn(logger);
    when(LoggerFactory.getLogger(BatchProcessMonitor.class)).thenReturn(logger);
  }

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void notifyOkBatchProcessIsDone() {
    when(result.getPendingEvents()).thenReturn(Collections.<EventMessage>emptyList());

    batchProcessMonitor.notifyBatchProcessIsDone(result);

    verify(pendingEventRepository, times(0)).storePendingEvents(anyListOf(EventMessage.class));
    Assert.assertEquals(ReflectionTestUtils.getField(batchProcessMonitor, "numTasksKo"), new Long(0));
    Assert.assertEquals(ReflectionTestUtils.getField(batchProcessMonitor, "numTasksOk"), new Long(1));
    Assert.assertEquals(ReflectionTestUtils.getField(batchProcessMonitor, "numTasksProcessed"), new Long(1));
  }

  @Test
  public void notifyKoBatchProcessIsDone() throws Exception {
    final List<EventMessage> pendingEvents = generateRandomList(EventMessage.class);
    when(result.getPendingEvents()).thenReturn(pendingEvents);

    batchProcessMonitor.notifyBatchProcessIsDone(result);

    verify(pendingEventRepository).storePendingEvents(pendingEvents);
    Assert.assertEquals(ReflectionTestUtils.getField(batchProcessMonitor, "numTasksKo"), new Long(1));
    Assert.assertEquals(ReflectionTestUtils.getField(batchProcessMonitor, "numTasksOk"), new Long(0));
    Assert.assertEquals(ReflectionTestUtils.getField(batchProcessMonitor, "numTasksProcessed"), new Long(1));
  }

  @Test
  public void writeState() {
    batchProcessMonitor.writeState();

    verify(logger).info(" ---- BatchProcessMonitor ---- ");
    verify(logger).info(anyString(), anyLong(), anyLong(), anyLong(), anyLong());
  }
}
