package org.sentilo.web.catalog.test.format.support;

import java.util.Date;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.web.catalog.format.datetime.LocalDateFormatter;
import org.sentilo.web.catalog.format.support.SentiloFormattingConversionService;

public class SentiloFormattingConversionServiceTest {

  @InjectMocks
  private SentiloFormattingConversionService service;

  @Mock
  private LocalDateFormatter formatter;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void customFormattersRegistry() {
    service.customFormattersRegistry();

    Assert.assertTrue(service.canConvert(Date.class, String.class));
  }
}
