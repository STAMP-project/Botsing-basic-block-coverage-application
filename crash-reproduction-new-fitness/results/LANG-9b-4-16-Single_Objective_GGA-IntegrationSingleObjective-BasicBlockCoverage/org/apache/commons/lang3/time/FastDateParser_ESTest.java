/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 15:58:37 UTC 2020
 */

package org.apache.commons.lang3.time;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.util.Locale;
import java.util.TimeZone;
import org.apache.commons.lang3.time.FastDateParser;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class FastDateParser_ESTest extends FastDateParser_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      TimeZone timeZone0 = TimeZone.getDefault();
      Locale locale0 = Locale.GERMAN;
      FastDateParser fastDateParser0 = new FastDateParser("GMTNET", timeZone0, locale0);
      FastDateParser fastDateParser1 = new FastDateParser("E?Av6ATQ", timeZone0, locale0);
      fastDateParser1.getLocale();
      Locale locale1 = FastDateParser.JAPANESE_IMPERIAL;
      FastDateParser fastDateParser2 = new FastDateParser("GMT", timeZone0, locale1);
  }
}
