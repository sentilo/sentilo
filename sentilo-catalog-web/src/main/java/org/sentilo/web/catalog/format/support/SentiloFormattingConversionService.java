package org.sentilo.web.catalog.format.support;

import javax.annotation.PostConstruct;

import org.sentilo.web.catalog.format.datetime.LocalDateFormatter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.format.support.DefaultFormattingConversionService;
import org.springframework.stereotype.Component;

@Component("sentiloConversionService")
public class SentiloFormattingConversionService extends DefaultFormattingConversionService {

  @Autowired
  private LocalDateFormatter sentiloDateFormatter;

  @PostConstruct
  public void customFormattersRegistry() {
    addFormatter(sentiloDateFormatter);
  }
}
