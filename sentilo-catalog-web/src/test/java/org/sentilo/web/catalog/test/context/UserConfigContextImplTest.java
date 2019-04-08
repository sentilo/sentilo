package org.sentilo.web.catalog.test.context;

import java.util.TimeZone;

import org.junit.Assert;
import org.junit.Test;
import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.web.catalog.context.UserConfigContextImpl;
import org.sentilo.web.catalog.utils.Constants;

public class UserConfigContextImplTest {

  @Test
  public void defaultConfig() {
    final UserConfigContextImpl config = new UserConfigContextImpl();

    Assert.assertEquals(SentiloConstants.TIMESTAMP_PATTERN, config.getUserDatePattern());
    Assert.assertEquals(TimeZone.getTimeZone(Constants.DEFAULT_TIME_ZONE), config.getUserTimeZone());
    Assert.assertEquals(Constants.DEFAULT_CHART_POINTS_NUMBER, config.getChartVisiblePointsNumber());
  }

  @Test
  public void customConfig() {
    final Integer userChartPointsNum = 10;
    final String datePattern = "yyyy/MM/dd HH:mm";
    final TimeZone tz = TimeZone.getTimeZone("GMT+02:00");

    final UserConfigContextImpl config = new UserConfigContextImpl(tz, datePattern, userChartPointsNum);

    Assert.assertEquals(datePattern, config.getUserDatePattern());
    Assert.assertEquals(tz, config.getUserTimeZone());
    Assert.assertEquals(userChartPointsNum, config.getChartVisiblePointsNumber());
  }

  @Test
  public void defaultConfigToString() {
    final UserConfigContextImpl config = new UserConfigContextImpl();
    final String sConfig = config.toString();

    final String expected =
        "org.sentilo.web.catalog.context.UserConfigContextImpl [ userDatePattern: dd/MM/yyyy'T'HH:mm:ss|userTimeZone: UTC|userChartNumObs: 10 ]";

    Assert.assertEquals(expected, sConfig);
  }

}
