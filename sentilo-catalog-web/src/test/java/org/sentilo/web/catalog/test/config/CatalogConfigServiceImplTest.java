package org.sentilo.web.catalog.test.config;

import static org.mockito.Matchers.any;
import static org.mockito.Mockito.verify;

import java.util.Map;
import java.util.Properties;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.config.SentiloArtifactConfigRepository;
import org.sentilo.common.test.AbstractBaseTest;
import org.sentilo.web.catalog.config.CatalogConfigServiceImpl;
import org.sentilo.web.catalog.service.PlatformService;

public class CatalogConfigServiceImplTest extends AbstractBaseTest {

  private static final String USER_TIMEZONE_PARAM = "user.timezone";
  private static final String FILE_ENCODING_PARAM = "file.encoding";
  private static final String SPRING_PROFILES_ACTIVE_PARAM = "spring.profiles.active";

  @InjectMocks
  private CatalogConfigServiceImpl configService;

  @Mock
  private PlatformService platformService;

  @Mock
  private Properties configProperties;

  @Mock
  private SentiloArtifactConfigRepository repository;

  @Before
  public void setUp() throws Exception {
    System.setProperty(SPRING_PROFILES_ACTIVE_PARAM, "test");
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void getName() {
    final String actualAgentName = configService.getName();
    Assert.assertEquals("catalog-web", actualAgentName);
  }

  @Test
  public void getArtifactConfig() {
    final Map<String, Object> config = configService.getArtifactConfig();
    Assert.assertEquals(System.getProperty(USER_TIMEZONE_PARAM, "-"), config.get(USER_TIMEZONE_PARAM));
    Assert.assertEquals(System.getProperty(FILE_ENCODING_PARAM, "-"), config.get(FILE_ENCODING_PARAM));
    Assert.assertEquals("test", config.get(SPRING_PROFILES_ACTIVE_PARAM));
  }

  @Test
  public void save() {
    configService.save();
    verify(platformService).saveCatalogConfig(any());
  }

}
